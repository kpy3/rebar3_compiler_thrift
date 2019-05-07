rebar3_compiler_thrift
======================

A thrift compiler module for rebar3 3.7+

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_compiler_thrift, ".*", {git, "https://github.com/kpy3/rebar3_thrift_compiler.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_compiler_thrift
    ===> Fetching rebar3_compiler_thrift
    ===> Compiling rebar3_compiler_thrift
    <Plugin Output>
