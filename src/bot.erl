-module(bot).
-author("Hardy Jones <jones3.hardy@gmail.com>").

-export([connect/0, loop/1]).

-define(COMMANDS, ["help", "ts"]).
-define(CHANNELS, ["#confab", "#python-forum"]).
-define(HOST, "chat.freenode.net").
-define(NICK, "confabot").
-define(PORT, 6667).
-define(TS_API, "api key").

close(Socket) ->
    gen_tcp:close(Socket).

connect() ->
    {ok, Socket} = gen_tcp:connect(?HOST, ?PORT, [{active, false}]),
    nick(Socket, ?NICK),
    user(Socket, "bot bot bot :bot"),
    io:format("Starting inets~n"),
    inets:start(),
    io:format("Starting loop~n"),
    loop(Socket).

help(["ts"|_Rest]) ->
    "Search for music by album, artist, or song";
help(_) ->
    "Here's what I can do: " ++ string:join(?COMMANDS, ", ").

join(Socket, Channel) ->
    io:format("Joining: ~p~n", [Channel]),
    send(Socket, "JOIN " ++ Channel).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Message} ->
	    Lines = string:tokens(Message, "\r\n"),
	    parse(Socket, lists:map(fun(L) -> string:tokens(L, ": ") end,
				    Lines)),
	    loop(Socket);
	{error, Error} ->
	    io:format("~n~nError: ~p~n~n~n", [Error]),
	    loop(Socket)
    end.

nick(Socket, Nick) ->
    io:format("Setting nickname to ~p~n", [Nick]),
    send(Socket, "NICK " ++ Nick).

parse(_Socket, []) ->
    done_parsing;
parse(Socket, [["PING", Reply]|Tail]) ->
    io:format("Recieved: PING ~p~n", [Reply]),
    io:format("Replying with: PONG ~p~n", [Reply]), 
    pong(Socket, Reply),
    parse(Socket, Tail);
parse(Socket, [[_User, "PRIVMSG", Channel|Message]|Tail]) ->
    io:format("Recieved: PRIVMSG ~p~n", [Message]),
    case Message of
	[">help"|Rest] ->
	    HelpMessage = help(Rest),
	    say(Socket, Channel, HelpMessage);
	[">ts"|Query] ->
	    TsMessage = tiny_song(Query),
	    case TsMessage of
		{list, Songs} ->
		    lists:foreach(fun(Song) -> say(Socket, Channel, Song) end,
				  lists:sublist(Songs, 3));
		_ ->
		    say(Socket, Channel, TsMessage)
	    end;
	_ ->
	    parse(Socket, Tail)
    end;
parse(Socket, [["ERROR", Error]|Tail]) ->
    io:format("Error: ~p~n", [Error]),
    close(Socket),
    parse(Socket, Tail);
parse(Socket, [[_, "376"|_]|Tail]) ->
    io:format("Recieved end of MOTD, joining channel~n"),
    join(Socket, string:join(?CHANNELS, ",")),
    parse(Socket, Tail);
parse(Socket, [[_, "433"|Message]|Tail]) ->
    [[_Ast, Nick|_Rest]] = string:tokens(Message, " "),
    io:format("Nick in use, adding underscore~n"),
    nick(Socket, Nick ++ "_"),
    parse(Socket, Tail);
parse(Socket, [Line|Tail]) ->
    io:format("~p~n", [string:join(Line, " ")]),
    parse(Socket, Tail).

pong(Socket, Reply) ->
    send(Socket, "PONG " ++ Reply).

say(Socket, Channel, Message) ->
    io:format("Sending: PRIVMSG ~p :~p~n", [Channel, Message]),
    send(Socket, "PRIVMSG " ++ Channel ++ " :" ++ Message).

send(Socket, Message) ->
    gen_tcp:send(Socket, Message ++ "\r\n").

tiny_song(Query) ->
    QuotedQuery = string:join(Query, "+"),
    Url = ("http://tinysong.com/s/" ++ QuotedQuery ++ "?format=json" ++
	   "&key=" ++ ?TS_API),
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    receive
	{http, {RequestId, {_Resp, _Head, TsJson}}} ->
	    case mochijson:decode(TsJson) of
		{array, []} ->
		    "No matches found";
		{array, Songs} ->
		    {list, [Song ++ " by " ++ Artist ++ " - " ++ Link ||
			       {struct, [{_L, Link}, _Sid, {_S, Song}, _Artid,
					 {_A, Artist}, _Albid, _Alb]} 
				   <- Songs]};
		{struct, [{"error", Error}]} ->
		    io:format("Error: ~p~n", Error),
		    "Wtf?";
		Url ->
		    Url 
	    end;
	{error, Error} ->
	    io:format("Error: ~p~n", [Error])
    after 5000 ->
	timeout
    end.

user(Socket, User) ->
    send(Socket, "USER " ++ User).
