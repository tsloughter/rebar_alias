-module(rebar_alias).

-export([init/1]).

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
    Module = list_to_atom("rebar_prv_alias_" ++ atom_to_list(Alias)),

    MF = module(Module),
    EF = exports(),
    FF = do_func(Cmds),

    {ok, _, Bin} = compile:forms([MF, EF, FF]),
    code:load_binary(Module, "none", Bin),

    Provider = providers:create([
            {name, Alias},
            {module, Module},
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
    "Equivalent to running: rebar3 do " ++ string:join(lists:map(fun({Cmd, Args}) ->
                                                                     atom_to_list(Cmd) ++ " " ++ Args;
                                                                    (Cmd) ->
                                                                     atom_to_list(Cmd)
                                                                 end, Cmds), ",").

module(Name) ->
    {attribute,1,module,Name}.

exports() ->
    {attribute,1,export,[{do,1}]}.

do_func(Cmds) ->
    {function,1,do,1,
     [{clause,1,
       [{var,1,'State'}],
       [],
       [{call,1,
         {remote,1,{atom,1,rebar_prv_do},{atom,1,do_tasks}},
         [to_args(Cmds),{var,1,'State'}]}]}]}.


to_args([]) ->
    {nil,1};
to_args([{Cmd, Args} | Rest]) ->
    {cons,1,{tuple,1,[{string,1,atom_to_list(Cmd)},{string,1,Args}]}, to_args(Rest)};
to_args([Cmd | Rest]) ->
    {cons,1,{tuple,1,[{string,1,atom_to_list(Cmd)},{nil,1}]}, to_args(Rest)}.
