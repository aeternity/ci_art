-module(ci_art_update_prv).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
                 [
                   {name      , update}
                 , {namespace , artifacts}
                 , {module    , ?MODULE}
                 , {bare      , true}
                 , {deps      , [{default, app_discovery}]}
                 , {example   , "rebar3 compile artifacts"}
                 , {opts      , []}
                 , {short_desc, "Fetch latest CircleCI artifacts"}
                 , {desc      , "Longer description"}
                 ]),
    {ok, rebar_state:add_provider(State, Provider)}.


do(State) ->
    rebar_api:info("Checking CI artifacts...", []),
    update(State),
    {ok, State}.

update(State) ->
    Roots = rebar_state:get(State, artifact_roots, []),
    rebar_api:info("Roots = ~p", [Roots]),
    try lists:foreach(fun(Root) -> update_root(Root, State) end, Roots),
         {ok, State}
    catch
        error:Error ->
            {error, format_error(Error)}
    end.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
                          

update_root(#{ root := Dir0 } = Root, State) ->
    Dir = filename:absname(Dir0) ++ "/",   % absname strips any trailing slash
    case filelib:is_dir(Dir) of
        true ->
            Files = filelib:fold_files(
                      Dir, ".*", true,
                      fun(F, Acc) ->
                              [[], F1] = re:split(F, Dir, [{return, list}]),
                              Acc ++ [F1]
                      end, []),
            fetch_artifacts(Files, Dir, Root, State);
        false ->
            error({invalid_root, Dir0})
    end.

fetch_artifacts(Files, RootDir, Root0, _State) ->
    Root = maps:merge(#{log => fun rebar_api:info/2}, Root0),
    PathFilter = fun(F) ->
                         lists:any(fun(F1) ->
                                           nomatch =/= re:run(F, F1, [])
                                   end, Files)
                 end,
    Res = case maps:get(system, Root, circleci) of
              circleci ->
                  ci_art_circleci:fetch_artifacts(Root, #{<<"path">> => PathFilter});
              Other ->
                  error({ci_backend_not_supported, Other})
          end,
    rebar_api:info("~p candidate artifacts ...", [length(Res)]),
    N = fetch_files(Res, Files, RootDir),
    rebar_api:info("~p files downloaded. Check result with `git status`", [N]),
    ok.



fetch_files([], _, _) ->
    0;
fetch_files([#{<<"path">> := P, <<"url">> := Link}|T], Files, Root) ->
    case [F || F <- Files,
               nomatch =/= re:run(P, F, [])] of
        [_|_] = Matches ->
            RelPath = longest_match(Matches),
            case httpc:request(get, {binary_to_list(Link), []}, [], []) of
                {ok, {{_, 200, "OK"}, _Headers, Body}} ->
                    file:write_file(RelPath, Body),
                    1 + fetch_files(T, Files, Root);
                FetchError ->
                    error({FetchError, Link})
            end;
        [] ->
            error(unexpected)
    end.

longest_match(L) ->
    {_, Longest} = lists:foldl(fun longer/2, {0, <<>>}, L),
    Longest.

longer(X, {Sz0,_} = Acc) ->
    case length(X) of
        Sz when Sz > Sz0 ->
            {Sz, X};
        _ ->
            Acc
    end.
    
