-module(tiny_song).
-author('Hardy Jones <jones3.hardy@gmail.com.').

-export([parse/1]).

-include("../../include/otp_bot.hrl").

parse(Query) ->
    QuotedQuery = mochiweb_util:quote_plus(string:join(Query, " ")),
    Url = string:join(["http://tinysong.com/s/", QuotedQuery,
                       "?format=json&key=", ?TS_API_KEY], ""),
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    receive
        {http, {RequestId, {_Resp, _Header, TsJson}}} ->
            case mochijson:decode(TsJson) of
                {array, []} ->
                    "No matches found";
                {array, RawSongs} ->
                    Songs = [Song ++ " by " ++ Artist ++ " - " ++ Link ||
                                {struct, [{_L, Link}, _Sid, {_S, Song}, _Artid,
                                          {_A, Artist}, _Albid, _Alb]}
                                    <- RawSongs],
                    {list, lists:sublist(Songs, 3)};
                {struct, [{"error", Error}]} ->
                    io:format("Error: ~p~n", [Error]),
                    "Wtf?";
                Url ->
                    Url
            end;
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    after 5000 ->
        timeout
    end.

