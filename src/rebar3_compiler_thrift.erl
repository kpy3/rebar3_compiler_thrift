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

%%    RebarOpts = rebar_app_info:opts(AppInfo),
    %% TODO Add load src dirs here?
    #{
        src_dirs => ["thrift"],
        include_dirs => [],
        src_ext => ".thrift",
        out_mappings => Mappings
    }.

needed_files(_, FoundFiles, _, AppInfo) ->
%%    RebarOpts = rebar_app_info:opts(AppInfo),
%%    Dir = rebar_app_info:dir(AppInfo),
    erlang:display({?MODULE,?LINE,FoundFiles}),
    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), thrift_opts, []),
    erlang:display({?MODULE,?LINE,Opts}),
    {{[], Opts}, {FoundFiles, Opts}}.

dependencies(_, _, _) ->
    [].

compile(Source, OutDirs, _Config, Opts) ->
    {_, ErlOut} = lists:keyfind(".erl", 1, OutDirs),
    {_, HrlOut} = lists:keyfind(".hrl", 1, OutDirs),

    ok = rebar_file_utils:ensure_dir(ErlOut),
    ok = rebar_file_utils:ensure_dir(HrlOut),
    TmpDir = rebar_file_utils:system_tmpdir([rebar3_compiler_thrift_utils:get_random_filename(16)]),
    ok = rebar_file_utils:ensure_dir(TmpDir),

    erlang:display({?MODULE,?LINE,Opts}),
    erlang:display({?MODULE,?LINE,TmpDir}),

    {ok, _} = rebar_utils:sh(io_lib:format("thrift -r -gen erl -out ~s ~s", [TmpDir, Source]), []),
    lists:foreach(
        fun(GeneratedFile) ->
            rebar_file_utils:mv(GeneratedFile, ErlOut)
        end,
        rebar_utils:find_files(TmpDir, "^.*\.erl$")),

    lists:foreach(
        fun(GeneratedFile) ->
            rebar_file_utils:mv(GeneratedFile, HrlOut)
        end,
        rebar_utils:find_files(TmpDir, "^.*\.hrl$")),


%%    Thrift = filename:join(BinOut, filename:basename(Source, ".thrift")),
%%    HrlFilename = Thrift ++ ".hrl",

%%    AllOpts = [{outdir, BinOut}, {i, [BinOut]}] ++ Opts,
    ok = rebar_file_utils:rm_rf(TmpDir),

    ok. %% OR ?FAIL

clean(ThriftFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
        [rebar_utils:to_list(re:replace(F, "\\.thrift$", "_thrift.erl", [unicode]))
            || F <- ThriftFiles]).
