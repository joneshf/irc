-module(irc).
-author('Hardy Jones <jones3.hardy@gmail.com>').

-export([join/2, nick/2, pong/2, say/3, user/2]).

-include("../include/otp_bot.hrl").

join(Socket, ChannelList) ->
    io:format("Joining: ~p~n", [ChannelList]),
    send(Socket, "JOIN " ++ string:join(ChannelList, ",")).

nick(Socket, Nick) ->
    io:format("Setting nickname to ~p~n", [Nick]),
    send(Socket, "NICK " ++ Nick).

pong(Socket, Message) ->
    send(Socket, "PONG " ++ Message).

say(Socket, Channel, Message) ->
    io:format("Sending: PRIVMSG ~p :~p~n", [Channel, Message]),
    send(Socket, "PRIVMSG " ++ Channel ++ " :" ++ Message).

send(Socket, Message) ->
    gen_tcp:send(Socket, Message ++ "\r\n").

user(Socket, User) ->
    send(Socket, "USER " ++ User).
