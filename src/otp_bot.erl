-module(otp_bot).
-author('Hardy Jones <jones3.hardy@gmail.com>').

-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
         terminate/2]).
-export([start_link/0, connect/0]).

-define(HOST, "chat.freenode.net").
-define(PORT, 6667).
-define(CHANNEL, "#confab").
-define(NICK, "confabbot").

%%% bot api
start_link() ->
    gen_server:start_link(?MODULE, connect, []).

connect() ->
    case gen_tcp:connect(?HOST, ?PORT, [{active, false}]) of
        {ok, Socket} ->
            % Set up the nick and user.
            io:format("Got a socket: ~p~n", [Socket]),
            loop(Socket);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end.

%%% private
loop(Socket) ->
    case gen_tcp:recv(Socket, 0, 30000) of
        {ok, Message} ->
            Lines = lists:map(fun(L) ->
                                      string:tokens(L, ": ") end,
                              string:tokens(Message, "\r\n")),
            parse(Socket, Lines),
            loop(Socket);
        {error, Error} ->
            io:format("~n~nError: ~p~n~n~n", [Error])
    end.

parse(_Socket, []) ->
    done_parsing;
parse(Socket, [Line|Tail]) ->
    io:format("~p~n", [string:join(Line, " ")]),
    parse(Socket, Tail).

%%% gen_server
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, _State) ->
    ok.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    io:format("~p~n", [Info]),
    {noreply, State}.

init(_Args) ->
    {ok, []}.

terminate(Reason, _State) ->
    io:format("~p~n", [Reason]),
    ok.
