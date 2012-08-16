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

join(Socket, Channel) ->
    io:format("Joining: ~s~n", [Channel]),
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
    io:format("Setting nickname to ~s~n", [Nick]),
    send(Socket, "NICK " ++ Nick).

parse(_Socket, []) ->
    done_parsing;
parse(Socket, [["PING", Reply]|Tail]) ->
    io:format("Recieved: PING ~s~n", [Reply]),
    io:format("Replying with: PONG ~s~n", [Reply]), 
    pong(Socket, Reply),
    parse(Socket, Tail);
parse(Socket, [[_User, "PRIVMSG", Channel|Message]|Tail]) ->
    io:format("Recieved: PRIVMSG ~p~n", [Message]),
    case Message of
	[">help"|Rest] ->
	    case Rest of
		["ts"|_] ->
		    say(Socket, Channel, "Search for music by album, artist," ++
			    " or song");
		_ ->
		    say(Socket, Channel, "Prefix commands with '>'"),
		    say(Socket, Channel, "Here's what I can do: " ++
			    string:join(?COMMANDS, ", ")),
		    parse(Socket, Tail)
	    end;
	[">ts"|Query] ->
	    QuotedQuery = string:join(Query, "+"),
	    Url = ("http://tinysong.com/a/" ++ QuotedQuery ++ "?format=json" ++
		   "&key=" ++ ?TS_API),
	    {ok, RequestId} = httpc:request(get, {Url, []}, [],
					    [{sync, false}]),
	    receive
		{http, {RequestId, {_Resp, _Head, TsUrl}}} ->
		    case TsUrl of
			<<"[]">> ->
			    say(Socket, Channel, "No matches found");
			_ ->
			    Escaped = binary:replace(TsUrl,
						     [<<"\"">>, <<"\\">>],
						     <<"">>, [global]),
			    say(Socket, Channel, binary_to_list(Escaped))
                    end;
		{error, Error} ->
		    io:format("Error: ~p~n", [Error])
	    after 5000 ->
		timeout
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
    io:format("~s~n", [string:join(Line, " ")]),
    parse(Socket, Tail).

pong(Socket, Reply) ->
    send(Socket, "PONG " ++ Reply).

say(Socket, Channel, Message) ->
    io:format("Sending: PRIVMSG ~s :~s~n", [Channel, Message]),
    send(Socket, "PRIVMSG " ++ Channel ++ " :" ++ Message).

send(Socket, Message) ->
    gen_tcp:send(Socket, Message ++ "\r\n").

user(Socket, User) ->
    send(Socket, "USER " ++ User).
