-module(rebar3_compiler_thrift_utils).

%% API
-export([check_rebar_version/0, get_random_filename/1]).

-define(MIN_SUPPORTED_VERSION, 307).
-define(FILENAME_ALLOWED_CHAR_RANGES, [{48, 10}, {65, 26}, {97, 26}]).

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

get_random_filename(FilenameLength) ->
    lists:foldl(
        fun(_, Acc) ->
            {Start, Length} =
                get_range(rand:uniform(2), ?FILENAME_ALLOWED_CHAR_RANGES),
            [Start + rand:uniform(Length - 1) | Acc]
        end, [], lists:seq(1, FilenameLength)).

get_range(N, Ranges) ->
    lists:nth(N+1, Ranges).