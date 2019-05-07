-module(rebar3_compiler_thrift_utils).

%% API
-export([check_rebar_version/0]).

-define(MIN_SUPPORTED_VERSION, 307).

check_rebar_version() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    case re:run(Vsn, "(\\d+)\\.(\\d+)\\.(\\d+)", [{capture, all, list}]) of
        {match, [Full, Maj, Min, _Patch]} ->
            IntVsn = list_to_integer(Maj) * 100 + list_to_integer(Min),
            case IntVsn > ?MIN_SUPPORTED_VERSION of
                true ->  ok;
                false ->
                    rebar_utils:abort(
                        "rebar3_compiler_thrift: minimal supported rebar version is 3.7.0, current: ~s",
                        [Full])
            end;
        nomatch ->
            rebar_utils:abort("Could not parse rebar version: ~s", [Vsn])
    end.
