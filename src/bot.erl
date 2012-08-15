-module(bot).
-author("Hardy Jones <jones3.hardy@gmail.com>").

-export([connect/0]).

close(Socket) ->
    gen_tcp:close(Socket).

connect() ->
    Host = "chat.freenode.net",
    Port = 6667,
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}]),
    io:format("~p~n", [Socket]),
    gen_tcp:send(Socket, "NICK confabbot\r\n"),
    gen_tcp:send(Socket, "USER confabbot confabbot confabbot :confabbot\r\n"),
    gen_tcp:send(Socket, "JOIN #confab\r\n"),
    io:format("Starting loop~n"),
    loop(Socket).

join(Socket, Channel) ->
    io:format("Joining: ~s~n", [Channel]),
    send(Socket, "JOIN " ++ Channel).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Message} ->
	    Everything = string:tokens(Message, ":\r\n "),
	    parse(Socket, Everything),
	    loop(Socket);
	_ ->
	    loop(Socket)
    end.

parse(Socket, ["PING", Reply]) ->
    io:format("Recieved: PING ~s~n", [Reply]),
    io:format("Replying with: PONG ~s~n", [Reply]), 
    pong(Socket, Reply);
parse(Socket, [_, "376"|_]) ->
    io:format("Recieved end of MOTD, joining channel~n"),
    join(Socket, "#confab");
parse(Socket, ["ERROR", Error]) ->
    io:format("Error: ~p~n", [Error]),
    close(Socket);
parse(_Socket, AllThings) ->
    io:format("~s~n", [string:join(AllThings, " ")]).

pong(Socket, Reply) ->
    send(Socket, "PONG " ++ Reply).

send(Socket, Message) ->
    gen_tcp:send(Socket, Message ++ "\r\n").
