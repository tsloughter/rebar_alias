-module(rebar_alias).

-export([init/1]).
-export([init_alias/3]).

-define(PROVIDER, rebar_alias).
-define(DEPS, []).

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
    {ok,MF} = erl_parse:parse_form(module(Alias)),
    {ok,EF} = erl_parse:parse_form(exports()),
    {ok,FF} = erl_parse:parse_form(do_func(Cmds)),

    {ok, _, Bin} = compile:forms([MF,EF,FF]),
    code:load_binary(Alias, "none", Bin),

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

example(Alias) ->
    "rebar3 " ++ atom_to_list(Alias).

desc(Cmds) ->
    "Equivalent to running: rebar3 do " ++ string:join([atom_to_list(Cmd) || Cmd <- Cmds], ",").

module(Name) ->
    [{'-',1},
     {atom,1,module},
     {'(',1},
     {atom,1,Name},
     {')',1},
     {dot,1}].

exports() ->
    [{'-',1},
     {atom,1,export},
     {'(',1},
     {'[',1},
     {atom,1,do},
     {'/',1},
     {integer,1,1},
     {']',1},
     {')',1},
     {dot,1}].

do_func(Cmds) ->
    [{atom,1,do},
     {'(',1},
     {var,1,'State'},
     {')',1},
     {'->',1},
     {atom,1,rebar_prv_do},
     {':',1},
     {atom,1,do_tasks},
     {'(',1},
     {'[',1}] ++ to_cmd_args_list(Cmds) ++ [{']',1},
                                            {',',1},
                                            {var,1,'State'},
                                            {')',1},
                                            {dot,1}].

to_cmd_args_list(Cmds) ->
    lists:droplast(lists:flatten([
                                 [{'{',1},
                                  {string,1,atom_to_list(Cmd)},
                                  {',',1},
                                  {'[',1},
                                  {']',1},
                                  {'}',1},
                                  {',',1}]
                                 || Cmd <- Cmds])).
