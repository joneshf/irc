-module(rpn).
-author('Hardy Jones <jones3.hardy@gmail.com>').

-export([parse/1]).

parse(Line) ->
    parse(Line, []).

%% Successful parsing.
parse([], [Acc]) ->
    io_lib:format("~p", [Acc]);
%% Operators.
parse(["+"|Rest], [Op1, Op2|Acc]) ->
    parse(Rest, [Op2 + Op1|Acc]);
parse(["-"|Rest], [Op1, Op2|Acc]) ->
    parse(Rest, [Op2 - Op1|Acc]);
parse(["*"|Rest], [Op1, Op2|Acc]) ->
    parse(Rest, [Op2 * Op1|Acc]);
parse(["/"|Rest], [Op1, Op2|Acc]) ->
    if Op1 == 0 ->
            "Division by Zero";
       true ->
            parse(Rest, [Op2 / Op1|Acc])
    end;
%% Operands.
parse([Token|Rest], Acc) ->
    case string:to_float(Token) of
        {error, no_float} ->
            {Int, _} = string:to_integer(Token),
            case Int of
                error ->
                    "Insufficient operands or bad operators";
                _ ->
                    parse(Rest, [Int|Acc])
            end;
        {Float, _} ->
            parse(Rest, [Float|Acc])
    end;
%% Errors.
parse([], [_Op,_Acc]) ->
    "Insufficient operators".
