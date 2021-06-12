%% -*- mode:erlang; erlang-indent-level:4; indent-tabs-mode:nil -*-
-module(ci_art_circleci).

-export([ fetch_artifacts/2
        %% , pipelines/3
        %% , master_pipelines/0
        , jobs/2
        , recent_runs/2
        , artifacts/3 ]).

%% Filter funs
-import(ci_art, [ f_date/2
                , f_date_cut/2
                , f_re/1
                , f_not/1
                , f_and/2
                , f_or/2 ]).

fetch_artifacts(Params, Filter) ->
    LatestRun = maps:get(latest_run, Params, #{ <<"branch">> => <<"master">>
                                              , <<"status">> => <<"success">> }),
    JobSpec = maps:get(job, Params),
    case recent_runs(LatestRun, Params) of
        [Latest | _] ->
            RunId = maps:get(<<"id">>, Latest),
            log("Latest run: ~p", [RunId], Params),
            case jobs(maps:get(<<"id">>, Latest), JobSpec) of
                [Job | _] ->
                    JobNo = maps:get(<<"job_number">>, Job),
                    log("Job number: ~p", [JobNo], Params),
                    artifacts(JobNo, Filter, Params);
                [] ->
                    error(no_such_job)
            end;
        [] ->
            error(no_such_run)
    end.

%% artifacts_of_test_latest(Filter) ->
%%     case recent_runs(#{<<"branch">> => <<"master">>, <<"status">> => <<"success">>}) of
%%         [Latest | _] ->
%%             case jobs(maps:get(<<"id">>, Latest), #{<<"name">> => <<"test_latest">>}) of
%%                 [Job | _] ->
%%                     artifacts(maps:get(<<"job_number">>, Job), Filter);
%%                 [] ->
%%                     {error, no_such_job}
%%             end;
%%         [] ->
%%             {error, no_such_run}
%%     end.

%% pipelines() ->
%%     pipelines(#{"mine" => true}, #{}).

%% master_pipelines() ->
%%     pipelines(#{"mine" => false}, #{<<"vcs">> => #{<<"branch">> => <<"master">>}}).

%% pipelines(Opts0, Filter, Params) ->
%%     Opts = Opts0#{"org-slug" => org_slug(Params)},
%%     try page_next(pipeline, Opts, Filter)
%%     catch
%%         throw:{done, Result} ->
%%             Result
%%     end.

jobs(#{<<"id">> := Id}, Filter) ->
    jobs_(Id, Filter);
jobs(Id, Filter) ->
    jobs_(Id, Filter).

jobs_(Id, Filter) ->
    page_next(jobs, #{ "id" => Id }, Filter).

recent_runs(Filter, Params) ->
    page_next(recent_runs, #{ "project-slug" => project_slug(Params)
                            , "branch" => maps:get(branch, Params, "master")
                            , "workflow" => maps:get(workflow, Params) }, Filter).

artifacts(Job, Filter, Params) ->
    page_next(artifacts, #{ "project-slug" => project_slug(Params)
                          , "job-number" => Job}, Filter).

page_next(Cmd, Opts, Filter) ->
    page_next(Cmd, Opts, Filter, []).

page_next(Cmd, Opts, Filter, Acc) ->
    case request(Cmd, Opts) of
        {ok, #{<<"next_page_token">> := T, <<"items">> := Items0}} ->
            Items = try filter(Items0, Filter)
                    catch
                        throw:{cut, Sofar} ->
                            throw({done, Acc ++ Sofar})
                    end,
            case T of
                null ->
                    Acc ++ Items;
                _ ->
                    page_next(Cmd, Opts#{"page-token" => T}, Filter, Acc ++ Items)
            end;
        _Other ->
            %% io:fwrite("Got Other = ~p~n", [Other]),
            Acc
    end.

circle_token() ->
    case os:getenv("CIRCLECI_TOKEN") of
        false ->
            error(missing_circleci_token);
        T when is_list(T) ->
            T
    end.

base_uri() ->
    "https://circleci.com/api/v2".

project_slug(#{project := Project}) ->
    Project;
project_slug(_) ->
    error(missing_circleci_project).

%% org_slug(Params) ->
%%     [VCS, Owner, _Proj] = filename:split(project_slug(Params)),
%%     filename:join(VCS, Owner).

headers() ->
    [ {"Circle-Token", circle_token()} ].

request(jobs, #{ "id" := Id }) ->
    URI = flat([base_uri(), "/workflow/", Id, "/job"]),
    httpc_request(get, {URI, headers()});
request(recent_runs, #{"project-slug" := PS, "workflow" := WF}) ->
    URI = flat([base_uri(), "/insights/", PS, "/workflows/", WF]),
    httpc_request(get, {URI, headers()});
request(recent_runs_of_job, #{"project-slug" := PS, "workflow" := WF,
                              "job-name" := JN}) ->
    URI = flat([base_uri(), "/insights/", PS, "/workflows/", WF, "/jobs/", JN]),
    httpc_request(get, {URI, headers()});
request(artifacts, #{"project-slug" := PS, "job-number" := JN}) ->
    URI = flat([base_uri(), "/project/", PS, "/", str(JN), "/artifacts"]),
    httpc_request(get, {URI, headers()});
request(Cmd, Args) ->
    URI = [base_uri(), "/", str(Cmd)],
    Req = {add_args(Args, URI), headers()},
    httpc_request(get, Req).

log(Fmt, Args, Params) ->
    F = maps:get(log, Params, fun io:fwrite/2),
    F(Fmt, Args).

flat(L) ->
    binary_to_list(iolist_to_binary(L)).

httpc_request(Op, Req) ->
    case httpc:request(Op, Req, [], [{sync,true}]) of
        {ok, {{_, 200, "OK"}, _Hdrs, Body}} ->
            %% io:fwrite("Body = ~s~n", [Body]),
            {ok, jsx:decode(list_to_binary(Body), [return_maps])};
        Error ->
            Error
    end.

add_args(M, Acc) when map_size(M) == 0 ->
    Acc;
add_args(M, AccIn) when is_map(M) ->
    binary_to_list(
      iolist_to_binary(
        maps:fold(
          fun(K, V, Acc) ->
                  [Acc, "&", str(K), "=", str(V)]
          end, [AccIn, "?"], M))).


str(S) when is_list(S) ->
    S;
str(B) when is_binary(B) ->
    binary_to_list(B);
str(T) ->
    io_lib:fwrite("~w", [T]).

filter(Items, Filter) ->
    lists:reverse(
      lists:foldl(fun(I, Acc) -> filter_(I, Filter, Acc) end, [], Items)).

filter_(I, F, Acc) ->
    DateFilters = [<<"updated_at">>, <<"created_at">>, <<"stopped_at">>],
    %% We first do date matching, allowing even non-matching entries to
    %% trigger a date-time cut
    try match_item(I, maps:with(DateFilters, F)) of
        true ->
            try match_item(I, maps:without(DateFilters, F)) of
                true ->
                    [I | Acc];
                false ->
                    Acc
            catch
                throw:cut ->
                    throw({cut, lists:reverse(Acc)})
            end
    catch
        throw:cut ->
            throw({cut, lists:reverse(Acc)})
    end.

match_item(Item, Filter) ->
    try maps:fold(fun(K, V, Acc) ->
                          case maps:find(K, Item) of
                              {ok, V} ->
                                  Acc;
                              {ok, V1} when is_function(V, 1) ->
                                  V(V1);
                              {ok, V1} when is_map(V), is_map(V1) ->
                                  match_item(V1, V);
                              {ok, _} ->
                                  throw(no_match);
                              error ->
                                  throw(no_match)
                          end
                  end, true, Filter)
    catch
        throw:no_match ->
            false
    end.
