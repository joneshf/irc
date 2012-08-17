-module(help).
-author('Hardy Jones <jones3.hardy@gmail.com>').

-export([parse/1]).

-include("../../include/otp_bot.hrl").

parse(["ts"|_Rest]) ->
    "Search for music by album, artist, or song";
parse(_) ->
    "Here's what I can do: " ++ string:join(?COMMANDS, ", ").
