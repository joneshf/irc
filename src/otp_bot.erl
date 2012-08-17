-module(otp_bot).
-author('Hardy Jones <jones3.hardy@gmail.com>').

-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
         terminate/2]).
-export([start_link/0, connect/0]).

-include("../include/otp_bot.hrl").

%%% bot api
start_link() ->
    gen_server:start_link(?MODULE, connect, []).

connect() ->
    inets:start(),
    case gen_tcp:connect(?HOST, ?PORT, [{active, false}]) of
        {ok, Socket} ->
            % Set up the nick and user.
            irc:nick(Socket, ?NICK),
            irc:user(Socket, ?USER),
            io:format("Got a socket: ~p~n", [Socket]),
            loop(Socket);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end.

%%% private
loop(Socket) ->
    case gen_tcp:recv(Socket, 0, 300000) of
        {ok, RawMessage} ->
            Lines = lists:map(fun(L) ->
                                      string:tokens(L, " ") end,
                              string:tokens(RawMessage, "\r\n")),
            parse(Socket, Lines),
            loop(Socket);
        {error, Error} ->
            io:format("~n~nError: ~p~n~n~n", [Error])
    end.

parse(_Socket, []) ->
    done_parsing;
parse(Socket, [["PING", Reply]|Tail]) ->
    io:format("Received: PING ~p~nReplying with: PONG ~p~n", [Reply, Reply]),
    irc:pong(Socket, string:sub_string(Reply, 2)),
    parse(Socket, Tail);
parse(Socket, [[Hostmask, "PRIVMSG", Channel|Message]|Tail]) ->
    [$:|Nick] = string:sub_word(Hostmask, 1, $!),
    io:format("~s - ~s: ~s~n", [Channel, Nick, string:join(Message, " ")]),
    case Message of
        [":>help"|Rest] ->
            HelpMessage = help:parse(Rest),
            irc:say(Socket, Channel, HelpMessage);
        [":>ts"|Query] ->
            TsMessage = tiny_song:parse(Query),
            case TsMessage of
                {list, Songs} ->
                    lists:foreach(fun(S) -> irc:say(Socket, Channel, S) end,
                                  Songs);
                _ ->
                    irc:say(Socket, Channel, TsMessage)
            end;
        _ ->
            parse(Socket, Tail)
    end,
    parse(Socket, Tail);
parse(Socket, [[_Server, "376"|_Message]|Tail]) ->
    io:format("Received end of MOTD, joining channel list~n"),
    irc:join(Socket, ?CHANNELS),
    parse(Socket, Tail);
parse(Socket, [[_Server, "433", _Asterix, Nick|Message]|Tail]) ->
    io:format("~p~nAdding underscore~n", [string:join(Message, " ")]),
    irc:nick(Socket, Nick ++ "_"),
    parse(Socket, Tail);
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
