-module('rebar_alias').

-export([init/1]).
-export([init_alias/3]).

-define(PROVIDER, 'rebar_alias').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Aliases = rebar_state:get(State, alias, []),
    lists:foldl(fun({Alias, Cmds}, {ok, StateAcc}) ->
                        init_alias(Alias, Cmds, StateAcc)
                end, {ok, State}, Aliases).

init_alias(Alias, Cmds, State) ->
    {ok, MTs, _} = erl_scan:string(lists:flatten(io_lib:format("-module(~s).", [Alias]))),
    {ok, ETs, _} = erl_scan:string("-export([do/1])."),
    {ok, FTs, _} = erl_scan:string(do_func(Cmds)),

    {ok,MF} = erl_parse:parse_form(MTs),
    {ok,EF} = erl_parse:parse_form(ETs),
    {ok,FF} = erl_parse:parse_form(FTs),

    {ok, _, Bin} = compile:forms([MF,EF,FF]),
    code:load_binary(Alias, "nofile", Bin),

    Provider = providers:create([
            {name, Alias},
            {module, Alias},
            {bare, true},
            {deps, []},
            {example, example(Alias)},
            {opts, []},
            {short_desc, desc(Cmds)},
            {desc, desc(Cmds)}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

do_func(Cmds) ->
    CmdsString = [{atom_to_list(Cmd), []} || Cmd <- Cmds],
    lists:flatten(io_lib:format("do(State) -> rebar_prv_do:do_tasks(~p, State).", [CmdsString])).

example(Alias) ->
    "rebar3 " ++ atom_to_list(Alias).

desc(Cmds) ->
    "Equivalent to running: rebar3 do " ++ string:join([atom_to_list(Cmd) || Cmd <- Cmds], ",").
