-module(otp_bot).
-author('Hardy Jones <jones3.hardy@gmail.com>').

-behavior(gen_server).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
         terminate/2]).
-export([start_link/0, connect/0, loop/1]).

-include("../include/otp_bot.hrl").

%%% bot api
start_link() ->
    gen_server:start_link(?MODULE, connect, []).

connect() ->
    inets:start(),
    case gen_tcp:connect(?HOST, ?PORT, [{active, false}]) of
        {ok, Socket} ->
            % Set up the nick and user.
            nick(Socket, ?NICK),
            user(Socket, ?USER),
            io:format("Got a socket: ~p~n", [Socket]),
            loop(Socket);
        {error, Error} ->
            io:format("Error: ~p~n", [Error])
    end.

help(["ts"|_Rest]) ->
    "Search for music by album, artist, or song";
help(_) ->
    "Here's what I can do: " ++ string:join(?COMMANDS, ", ").

%%% private
join(Socket, ChannelList) ->
    io:format("Joining: ~p~n", [ChannelList]),
    send(Socket, "JOIN " ++ string:join(ChannelList, ",")).

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

nick(Socket, Nick) ->
    io:format("Setting nickname to ~p~n", [Nick]),
    send(Socket, "NICK " ++ Nick).

parse(_Socket, []) ->
    done_parsing;
parse(Socket, [["PING", Reply]|Tail]) ->
    io:format("Received: PING ~p~nReplying with: PONG ~p~n", [Reply, Reply]),
    pong(Socket, string:sub_string(Reply, 2)),
    parse(Socket, Tail);
parse(Socket, [[Hostmask, "PRIVMSG", Channel|Message]|Tail]) ->
    [$:|Nick] = string:sub_word(Hostmask, 1, $!),
    io:format("~s - ~s: ~s~n", [Channel, Nick, string:join(Message, " ")]),
    case Message of
        [":>help"|Rest] ->
            HelpMessage = help(Rest),
            say(Socket, Channel, HelpMessage);
        [":>ts"|Query] ->
            TsMessage = tiny_song(Query),
            case TsMessage of
                {list, Songs} ->
                    lists:foreach(fun(S) -> say(Socket, Channel, S) end,
                                  lists:sublist(Songs, 3));
                _ ->
                    say(Socket, Channel, TsMessage)
            end;
        _ ->
            parse(Socket, Tail)
    end,
    parse(Socket, Tail);
parse(Socket, [[_Server, "376"|_Message]|Tail]) ->
    io:format("Received end of MOTD, joining channel list~n"),
    join(Socket, ?CHANNELS),
    parse(Socket, Tail);
parse(Socket, [[_Server, "433", _Asterix, Nick|Message]|Tail]) ->
    io:format("~p~nAdding underscore~n", [string:join(Message, " ")]),
    nick(Socket, Nick ++ "_"),
    parse(Socket, Tail);
parse(Socket, [Line|Tail]) ->
    io:format("~p~n", [string:join(Line, " ")]),
    parse(Socket, Tail).

pong(Socket, Message) ->
    send(Socket, "PONG " ++ Message).

say(Socket, Channel, Message) ->
    io:format("Sending: PRIVMSG ~p :~p~n", [Channel, Message]),
    send(Socket, "PRIVMSG " ++ Channel ++ " :" ++ Message).

send(Socket, Message) ->
    gen_tcp:send(Socket, Message ++ "\r\n").

tiny_song(Query) ->
    QuotedQuery = mochiweb_util:quote_plus(string:join(Query, " ")),
    Url = string:join(["http://tinysong.com/s/", QuotedQuery,
                       "?format=json&key=", ?TS_API_KEY], ""),
    io:format("Query: ~p~n", [Url]),
    io:format("Better? ~p~n", [mochijson:encode(QuotedQuery)]),
    {ok, RequestId} = httpc:request(get, {Url, []}, [], [{sync, false}]),
    receive
        {http, {RequestId, {_Resp, _Header, TsJson}}} ->
            case mochijson:decode(TsJson) of
                {array, []} ->
                    "No matches found";
                {array, Songs} ->
                    {list, [Song ++ " by " ++ Artist ++ " - " ++ Link ||
                               {struct, [{_L, Link}, _Sid, {_S, Song}, _Artid,
                                         {_A, Artist}, _Albid, _Alb]}
                                   <- Songs]};
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

user(Socket, User) ->
    send(Socket, "USER " ++ User).

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
