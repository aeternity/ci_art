%% -*- mode:erlang; erlang-indent-level:4; indent-tabs-mode:nil -*-
-module(ci_art).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% Filter functions for convenience
-export([ f_date/2
        , f_date_cut/2
        , f_re/1
        , f_not/1
        , f_and/2
        , f_or/2 ]).

init(State) ->
    {ok, State1} = ci_art_update_prv:init(State),
    {ok, State1}.

do(State) ->
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).



f_date(Op, D) when Op=='=='; Op=='=/='; Op=='>'; Op=='>='; Op=='<'; Op=='=<' ->
    fun(Dx) ->
            erlang:Op(dt2gs(parse_date(Dx)), dt2gs(D))
    end.

f_date_cut(Op, D) when Op=='=='; Op=='=/='; Op=='>'; Op=='>='; Op=='<'; Op=='=<' ->
    fun(Dx) ->
            DTx = parse_date(Dx),
            case erlang:Op(dt2gs(DTx), dt2gs(D)) of
                false ->
                    throw(cut);
                true ->
                    true
            end
    end.

f_re(Pat) ->
    fun(S) ->
            nomatch =/= re:run(S, Pat, [])
    end.

f_not(F) ->
    fun(X) -> not F(X) end.

f_and(A, B) ->
    fun(X) -> A(X) andalso B(X) end.

f_or(A, B) ->
    fun(X) -> A(X) orelse B(X) end.

parse_date(Bin) ->
    [Y,T] = re:split(Bin, <<"T">>, [{return, binary}]),
    [YY,Mo,D] = re:split(Y, <<"-">>, [{return, binary}]),
    [H,Mi,S0] = re:split(T, <<":">>, [{return, binary}]),
    [S,_] = re:split(S0, <<"\\.">>, [{return,binary}]),
    {{b2i(YY), b2i(Mo), b2i(D)}, {b2i(H), b2i(Mi), b2i(S)}}.

b2i(B) ->
    binary_to_integer(B).

dt2gs(DT) ->
    calendar:datetime_to_gregorian_seconds(DT).
