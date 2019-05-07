-module(rebar3_compiler_thrift).

%% Plugin API
-export([init/1]).

%% Compiler module API
-behaviour(rebar_compiler).

-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2
]).

%% Plugin API

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    rebar3_compiler_thrift_utils:check_rebar_version(),
    State1 = rebar_state:prepend_compilers(State, [?MODULE]),
    {ok, State1}.

%% Compiler module API

context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [
        {".erl", filename:join([Dir, "src"])},
        {".hrl", filename:join(Dir, "include")}
    ],

    #{
        src_dirs => ["thrift"],
        include_dirs => [],
        src_ext => ".thrift",
        out_mappings => Mappings
    }.

needed_files(_, FoundFiles, _, AppInfo) ->
%%    RebarOpts = rebar_app_info:opts(AppInfo),
%%    Dir = rebar_app_info:dir(AppInfo),

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), thrift_opts, []),
    {{[], Opts}, {FoundFiles, Opts}}.

dependencies(_, _, _) ->
    [].

compile(_Source, OutDirs, _, _Opts) ->
    {_, BinOut} = lists:keyfind(".erl", 1, OutDirs),
    {_, HrlOut} = lists:keyfind(".hrl", 1, OutDirs),

    ok = rebar_file_utils:ensure_dir(BinOut),
    ok = rebar_file_utils:ensure_dir(HrlOut),
%%    Thrift = filename:join(BinOut, filename:basename(Source, ".thrift")),
%%    HrlFilename = Thrift ++ ".hrl",

%%    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,

    ok. %% OR ?FAIL

clean(ThriftFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
        [rebar_utils:to_list(re:replace(F, "\\.thrift$", "_thrift.erl", [unicode]))
            || F <- ThriftFiles]).
