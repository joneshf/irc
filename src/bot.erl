-module(bot).
-compile(export_all).

connect(Password) ->
    Host = "chat.freenode.net",
    Port = 6667,
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, false}]),
    io:format("~p~n", [Socket]),
    gen_tcp:send(Socket, "NICK confabbot\r\n"),
    gen_tcp:send(Socket, "USER confabbot confabbot confabbot :confabbot\r\n"),
    gen_tcp:send(Socket, "JOIN #confab\r\n"),
    Identify = string:join(["PRIVMSG confab :identify", Password], " "),
    gen_tcp:send(Socket, Identify),
    io:format("Starting loop~n"),
    loop(Socket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Message} ->
	    [Command|Rest] = string:tokens(Message, " "),
	    case Command of
		"PING" ->
		    io:format("Recieved: ~p~n", Rest),
		    [Server] = Rest,
		    Reply = string:concat("PONG ",
					  string:strip(Server, both, $:)),
		    io:format("Replying with: ~p~n", [Reply]), 
		    gen_tcp:send(Socket, Reply),
		    loop(Socket);
		"ERROR" ->
                    io:format("Error, closing connection~n");
		_ ->
		    loop(Socket)
	    end;
	_ ->
	    loop(Socket)
    end.
