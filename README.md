rebar_alias
=====

A rebar plugin for defining aliases

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar_alias, ".*", {git, "git@host:user/rebar_alias.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:

```erlang
{alias, [{check, [eunit, ct]}]}.
```

    $ rebar3 check
    ===> Fetching rebar_alias
    ===> Compiling rebar_alias
    <Plugin Output>
