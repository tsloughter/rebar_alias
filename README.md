rebar_alias
=====

A rebar plugin for defining aliases

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [rebar_alias]}.

Then just call your plugin directly in an existing application:

```erlang
{alias, [{check, [eunit, {ct, "--sys_config=config/app.config"}]}]}.
```

    $ rebar3 check
    ===> Fetching rebar_alias
    ===> Compiling rebar_alias
    <Plugin Output>
