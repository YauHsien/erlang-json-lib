%% -*- coding: utf-8 -*-
-module(ecjson).
%% Goal
%% - Make JSON being retrieved easily by deserailize it gracefully.
%% - Be informatical to end user.

-include_lib("eunit/include/eunit.hrl").
-include("include/ecjson.hrl").
-export([check_and_condense/1,
         condense/1]).

-spec check_and_condense(JSON_string :: binary()) ->
          {ok, Condensed_JSON_string :: binary()} |
          {badsymbol, Fun :: atom(), Args :: list()}.
check_and_condense(JSON_string) ->
    check_and_condense(JSON_string, [element], 1, 1, <<>>).

-spec check_and_condense(
        JSON_string :: binary(),
        States :: [Non_terminal :: atom()],
        Column :: integer(),
        Row :: integer(),
        Condensed :: binary()) ->
          {ok, Condensed_JSON_string :: binary()} |
          {badsymbol, Fun :: atom(), Args :: list()}.

check_and_condense(<<>>, [element_value], _Column, _Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{1, check_and_condense, [<<>>, [element_value], _Column, _Row, Condensed]}]),
    {ok, Condensed};

%% -- Value ----------------------------------------------------------
check_and_condense(<<${/utf8,_/binary>> = JSON_string, [value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{2, check_and_condense, [JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [object|States], Column, Row, Condensed);

check_and_condense(<<$[/utf8,_/binary>> = JSON_string, [value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{3, check_and_condense, [JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [array|States], Column, Row, Condensed);

check_and_condense(<<$"/utf8,_/binary>> = JSON_string, [value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{4, check_and_condense, [JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [string|States], Column, Row, Condensed);

check_and_condense(<<Digit/utf8,_/binary>> = JSON_string, [value|States], Column, Row, Condensed)
  when ?isDigit(Digit) orelse
       Digit =:= $- ->
    _DEBUG = debug, io:fwrite("~n~p", [{5, check_and_condense, [JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [number|States], Column, Row, Condensed);

check_and_condense(<<"true",Rest/binary>> = _JSON_string, [value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{6, check_and_condense, [_JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+4, Row, binary:list_to_bin([Condensed,"true"]));

check_and_condense(<<"false",Rest/binary>> = _JSON_string, [value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{7, check_and_condense, [_JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+5, Row, binary:list_to_bin([Condensed,"false"]));

check_and_condense(<<"null",Rest/binary>> = _JSON_string, [value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{8, check_and_condense, [_JSON_string, [value|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+4, Row, binary:list_to_bin([Condensed,"null"]));

%% -- Object ---------------------------------------------------------
check_and_condense(<<${/utf8,Rest/binary>> = _JSON_string, [object|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{9, check_and_condense, [_JSON_string, [object|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, ['object_${'|States], Column+1, Row, binary:list_to_bin([Condensed,${]));

check_and_condense(<<WS/utf8,Rest/binary>> = _JSON_string, [Non_terminal|_] = States, Column, Row, Condensed)
  when ?isWS(WS) andalso
       (Non_terminal =:= 'object_${' orelse
        Non_terminal =:= 'object_${_members') ->
    _DEBUG = debug, io:fwrite("~n~p", [{10, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    {Column1, Row1} = next_coordinates(WS, Column, Row),
    check_and_condense(Rest, States, Column1, Row1, Condensed);

check_and_condense(<<$"/utf8,_/binary>> = JSON_string, ['object_${'|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{11, check_and_condense, [JSON_string, ['object_${'|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [members,'object_${_members'|States], Column, Row, Condensed);

check_and_condense(<<$}/utf8,Rest/binary>> = _JSON_string, [Non_terminal|States], Column, Row, Condensed)
  when (Non_terminal =:= 'object_${' orelse
        Non_terminal =:= 'object_${_members') ->
    _DEBUG = debug, io:fwrite("~n~p", [{12, check_and_condense, [_JSON_string, [Non_terminal|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,$}]));

%% -- Members --------------------------------------------------------
check_and_condense(<<$"/utf8,_/binary>> = JSON_string, [members|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{13, check_and_condense, [JSON_string, [members|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [member,members_member|States], Column, Row, Condensed);

check_and_condense(<<WS/utf8,Rest/binary>> = _JSON_string, [members|_] = States, Column, Row, Condensed)
  when ?isWS(WS) ->
    _DEBUG = debug, io:fwrite("~n~p", [{14, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    {Column1, Row1} = next_coordinates(WS, Column, Row),
    check_and_condense(Rest, States, Column1, Row1, Condensed);

check_and_condense(<<$,/utf8,Rest/binary>> = _JSON_string, [members_member|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{15, check_and_condense, [_JSON_string, [members_member|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [member,members_member|States], Column+1, Row, binary:list_to_bin([Condensed,$,]));

check_and_condense(JSON_string, [members_member|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{16, check_and_condense, [JSON_string, [members_member|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Member ---------------------------------------------------------
check_and_condense(<<WS/utf8,Rest/binary>> = _JSON_string, [Non_terminal|_] = States, Column, Row, Condensed)
  when ?isWS(WS) andalso
       (Non_terminal =:= member orelse
        Non_terminal =:= member_string) ->
    _DEBUG = debug, io:fwrite("~n~p", [{17, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    {Column1, Row1} = next_coordinates(WS, Column, Row),
    check_and_condense(Rest, States, Column1, Row1, Condensed);

check_and_condense(<<$"/utf8,_/binary>> = JSON_string, [member|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{18, check_and_condense, [JSON_string, [member|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [string,member_string|States], Column, Row, Condensed);

check_and_condense(<<$:/utf8,Rest/binary>> = _JSON_string, [member_string|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{19, check_and_condense, [_JSON_string, [member_string|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [element|States], Column+1, Row, binary:list_to_bin([Condensed,$:]));

%% -- Array ----------------------------------------------------------
check_and_condense(<<$[/utf8,Rest/binary>> = _JSON_string, [array|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{20, check_and_condense, [_JSON_string, [array|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, ['array_$['|States], Column+1, Row, binary:list_to_bin([Condensed,$[]));

check_and_condense(<<WS/utf8,Rest/binary>> = _JSON_string, [Non_terminal|_] = States, Column, Row, Condensed)
  when ?isWS(WS) andalso
       (Non_terminal =:= 'array_$[' orelse
        Non_terminal =:= 'array_$[_elements') ->
    _DEBUG = debug, io:fwrite("~n~p", [{21, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    {Column1, Row1} = next_coordinates(WS, Column, Row),
    check_and_condense(Rest, States, Column1, Row1, Condensed);

check_and_condense(<<$]/utf8,Rest/binary>> = _JSON_string, [Non_terminal|States], Column, Row, Condensed)
  when (Non_terminal =:= 'array_$[' orelse
        Non_terminal =:= 'array_$[_elements') ->
    _DEBUG = debug, io:fwrite("~n~p", [{23, check_and_condense, [_JSON_string, [Non_terminal|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,$]]));

check_and_condense(JSON_string, ['array_$['|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{22, check_and_condense, [JSON_string, ['array_$['|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [elements,'array_$[_elements'|States], Column, Row, Condensed);

%% -- Elements -------------------------------------------------------
check_and_condense(JSON_string, [elements|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{24, check_and_condense, [JSON_string, [elements|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [element,elements_element|States], Column, Row, Condensed);

check_and_condense(<<$,/utf8,Rest/binary>> = _JSON_string, [elements_element|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{25, check_and_condense, [_JSON_string, [elements_element|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [element,elements_element|States], Column+1, Row, binary:list_to_bin([Condensed,$,]));

check_and_condense(JSON_string, [elements_element|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{26, check_and_condense, [JSON_string, [elements_element|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Element --------------------------------------------------------
check_and_condense(<<WS/utf8,Rest/binary>> = _JSON_string, [Non_terminal|_] = States, Column, Row, Condensed)
  when ?isWS(WS) andalso
       (Non_terminal =:= element orelse
        Non_terminal =:= element_value)->
    _DEBUG = debug, io:fwrite("~n~p", [{27, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    {Column1, Row1} = next_coordinates(WS, Column, Row),
    check_and_condense(Rest, States, Column1, Row1, Condensed);

check_and_condense(JSON_string, [element|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{28, check_and_condense, [JSON_string, [element|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [value,element_value|States], Column, Row, Condensed);

check_and_condense(JSON_string, [element_value|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{28.1, check_and_condense, [JSON_string, [element_value|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- String ---------------------------------------------------------
check_and_condense(<<$"/utf8,Rest/binary>> = _JSON_string, [string|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{29, check_and_condense, [_JSON_string, [string|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [characters,'string_$"_characters'|States], Column+1, Row, binary:list_to_bin([Condensed,$"]));

check_and_condense(<<$"/utf8,Rest/binary>> = _JSON_string, ['string_$"_characters'|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{30, check_and_condense, [_JSON_string, ['string_$"_characters'|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,$"]));

%% -- Characters -----------------------------------------------------
check_and_condense(<<Char/utf8,_/binary>> = JSON_string, [characters|_] = States, Column, Row, Condensed)
  when ?isCharNotEscape(Char) ->
    _DEBUG = debug, io:fwrite("~n~p", [{31, check_and_condense, [JSON_string, States, Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [character|States], Column, Row, Condensed);

check_and_condense(<<$\\/utf8,Char/utf8,_/binary>> = JSON_string, [characters|_] = States, Column, Row, Condensed)
  when ?isEscapeCharNotHex(Char) ->
    _DEBUG = debug, io:fwrite("~n~p", [{32, check_and_condense, [JSON_string, States, Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [character|States], Column, Row, Condensed);

check_and_condense(<<"\\u"/utf8,Hex1/utf8,Hex2/utf8,Hex3/utf8,Hex4/utf8,_/binary>> = JSON_string, [characters|_] = States, Column, Row, Condensed)
  when ?areAllHex(Hex1,Hex2,Hex3,Hex4) ->
    _DEBUG = debug, io:fwrite("~n~p", [{33, check_and_condense, [JSON_string, States, Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [character|States], Column, Row, Condensed);

check_and_condense(JSON_string, [characters|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{34, check_and_condense, [JSON_string, [characters|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Character ------------------------------------------------------
check_and_condense(<<Char/utf8,Rest/binary>> = _JSON_string, [character|States], Column, Row, Condensed)
  when ?isCharNotEscape(Char) ->
    _DEBUG = debug, io:fwrite("~n~p", [{35, check_and_condense, [_JSON_string, [character|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,<<Char/utf8>>]));

check_and_condense(<<$\\/utf8,Char/utf8,Rest/binary>> = _JSON_string, [character|States], Column, Row, Condensed)
  when ?isEscapeCharNotHex(Char) ->
    _DEBUG = debug, io:fwrite("~n~p", [{36, check_and_condense, [_JSON_string, [character|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+2, Row, binary:list_to_bin([Condensed,<<$\\/utf8,Char/utf8>>]));

check_and_condense(<<"\\u"/utf8,Hex1/utf8,Hex2/utf8,Hex3/utf8,Hex4/utf8,Rest/binary>> = _JSON_string, [character|States], Column, Row, Condensed)
  when ?areAllHex(Hex1,Hex2,Hex3,Hex4) ->
    _DEBUG = debug, io:fwrite("~n~p", [{37, check_and_condense, [_JSON_string, [character|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+6, Row, binary:list_to_bin([Condensed,<<"\\u",Hex1/utf8,Hex2/utf8,Hex3/utf8,Hex4/utf8>>]));

%% -- Number ---------------------------------------------------------
check_and_condense(JSON_string, [number|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{38, check_and_condense, [JSON_string, [number|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [integer,number_integer|States], Column, Row, Condensed);

check_and_condense(JSON_string, [number_integer|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{39, check_and_condense, [JSON_string, [number_integer|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [fraction,number_integer_fraction|States], Column, Row, Condensed);

check_and_condense(JSON_string, [number_integer_fraction|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{40, check_and_condense, [JSON_string, [number_integer_fraction|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [exponent|States], Column, Row, Condensed);

%% -- Integer --------------------------------------------------------
check_and_condense(<<$-/utf8,Rest/binary>> = _JSON_string, [integer|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{41, check_and_condense, [_JSON_string, [integer|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [integer_sign|States], Column+1, Row, binary:list_to_bin([Condensed,$-]));

check_and_condense(<<Digit/utf8,Rest/binary>> = _JSON_string, [Non_terminal|States], Column, Row, Condensed)
when ?isDigit(Digit) andalso
     (Non_terminal =:= integer orelse
      Non_terminal =:= integer_sign) ->
    _DEBUG = debug, io:fwrite("~n~p", [{42, check_and_condense, [_JSON_string, [Non_terminal|States], Column, Row, Condensed]}]),
    case Digit of
        $0 ->
            check_and_condense(Rest, [integer_zero|States],    Column+1, Row, binary:list_to_bin([Condensed,Digit]));
        _ ->
            check_and_condense(Rest, [integer_onenine|States], Column+1, Row, binary:list_to_bin([Condensed,Digit]))
    end;

check_and_condense(<<Digit/utf8,Rest/binary>> = _JSON_string, [integer_onenine|_] = States, Column, Row, Condensed)
  when ?isDigit(Digit) ->
    _DEBUG = debug, io:fwrite("~n~p", [{43, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,Digit]));

check_and_condense(JSON_string, [Non_terminal|States], Column, Row, Condensed)
  when (Non_terminal =:= integer_zero orelse
        Non_terminal =:= integer_onenine) ->
    _DEBUG = debug, io:fwrite("~n~p", [{44, check_and_condense, [JSON_string, [Non_terminal|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Digits ---------------------------------------------------------
check_and_condense(<<Digit/utf8,Rest/binary>> = _JSON_string, [digits|_] = States, Column, Row, Condensed)
  when (Digit >= $0 andalso Digit =< $9) ->
    _DEBUG = debug, io:fwrite("~n~p", [{45, check_and_condense, [_JSON_string, States, Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,Digit]));

check_and_condense(JSON_string, [digits|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{46, check_and_condense, [JSON_string, [digits|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Fraction -------------------------------------------------------
check_and_condense(<<$./utf8,Rest/binary>> = _JSON_string, [fraction|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{47, check_and_condense, [_JSON_string, [fraction|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [digits|States], Column, Row, binary:list_to_bin([Condensed,$.]));

check_and_condense(JSON_string, [fraction|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{48, check_and_condense, [JSON_string, [fraction|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Exponent -------------------------------------------------------
check_and_condense(<<E/utf8,Rest/binary>> = _JSON_string, [exponent|States], Column, Row, Condensed)
  when (E =:= $E orelse E =:= $e) ->
    _DEBUG = debug, io:fwrite("~n~p", [{49, check_and_condense, [_JSON_string, [exponent|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, [sign,exponent_sign|States], Column+1, Row, binary:list_to_bin([Condensed,E]));

check_and_condense(JSON_string, [exponent|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{50, check_and_condense, [JSON_string, [exponent|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

check_and_condense(JSON_string, [exponent_sign|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{51, check_and_condense, [JSON_string, [exponent_sign|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, [digits|States], Column, Row, Condensed);

%% -- Sign ----------------------------------------------------------
check_and_condense(<<Sign/utf8,Rest/binary>> = _JSON_string, [sign|States], Column, Row, Condensed)
  when (Sign =:= $+ orelse Sign =:= $-) ->
    _DEBUG = debug, io:fwrite("~n~p", [{52, check_and_condense, [_JSON_string, [sign|States], Column, Row, Condensed]}]),
    check_and_condense(Rest, States, Column+1, Row, binary:list_to_bin([Condensed,Sign]));

check_and_condense(JSON_string, [sign|States], Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{53, check_and_condense, [JSON_string, [sign|States], Column, Row, Condensed]}]),
    check_and_condense(JSON_string, States, Column, Row, Condensed);

%% -- Line break -----------------------------------------------------
check_and_condense(<<$\x{000A}/utf8,Rest/binary>> = _JSON_string, [Non_terminal|_] = States, _Column, Row, Condensed)
  when Non_terminal =/= character andalso
       Non_terminal =/= characters andalso
       Non_terminal =/= 'string_$"_characters' ->
    _DEBUG = debug, io:fwrite("~n~p", [{54, check_and_condense, [_JSON_string, States, _Column, Row, Condensed]}]),
    check_and_condense(Rest, States, 1, Row+1, Condensed);

%% -- Bad result conclusion -----------------------------------------
check_and_condense(JSON_string, States, Column, Row, Condensed) ->
    _DEBUG = debug, io:fwrite("~n~p", [{55, check_and_condense, [JSON_string, States, Column, Row, Condensed]}]),
    {badsymbol, check_and_condense, [JSON_string, States, Column, Row, Condensed]}.

cnc_object_test() ->
    ?assertMatch({ok, <<"{\"object\":{\"hello\":\"world\"},\"array\":[\"hello,world\",{\"hello\":\"world\"},true,false,null],\"string\":\"hello,world\",\"true\":true,\"false\":false,\"null\":null}">>}, check_and_condense(<<" { 
           \"object\" : {
              \"hello\": \"world\"
           } , 
         \"array\": [ \"hello,world\", { \"hello\": \"world\" }, true, false, null ],
           \"string\": \"hello,world\",
           \"true\": true,
           \"false\": false,
           \"null\": null
    } ">>)).

cnc_object_failed_1_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<$},_/binary>>, _States, 5, 10, _Condensed]},
                 check_and_condense(<<" { 
           \"object\" : {
              \"hello\": \"world\"
           } , 
         \"array\": [ \"hello,world\", { \"hello\": \"world\" }, true, false, null ],
           \"string\": \"hello,world\",
           \"true\": true,
           \"false\": false,
           \"null\": null,
    } ">>)).

cnc_object_failed_2_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<${,_/binary>>, _States, 2, 1, _Condensed]},
                check_and_condense(<<"{{\"hello\":\"world\"}}">>)).

cnc_object_failed_3_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<$1,_/binary>>, _States, 2, 1, _Condensed]},
                 check_and_condense(<<"{1}">>)).

cnc_object_failed_4_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<$T,_/binary>>, _States, 10, 1, _Condensed]},
                 check_and_condense(<<"{\"hello\":TRUE}">>)).

cnc_object_failed_5_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<>>, [value,element_value], 2, 1, <<>>]},
                 check_and_condense(<<" ">>)).

cnc_object_failed_6_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<>>, _States, 2, 1, _Condensed]},
                 check_and_condense(<<"{">>)).

cnc_object_failed_7_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<$}>>, _States, 3, 1, _Condensed]},
                 check_and_condense(<<"{}}">>)).

cnc_array_test() ->
    ?assertMatch({ok, <<"[{\"hello\":\"world\",\"world\":[1,true,false,null]},\"hello,world\",2,true,false,null]">>},
                 check_and_condense(<<" [
         { \"hello\"  : \"world\", \"world\":[ 1, true, false, null ] },
         \"hello,world\",
        2,
          true , 
        false,
        null
    ] ">>)).

cnc_array_failed_1_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<>>, _States, 2, 1, _Condensed]},
                 check_and_condense(<<"[">>)).

cnc_array_failed_2_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<$]>>, _States, 3, 1, _Condensed]},
                 check_and_condense(<<"[]]">>)).

cnc_array_failed_3_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<$:,_/binary>>, _States, 10, 1, _Condensed]},
                 check_and_condense(<<"[\"hello\" :\"world\"]">>)).

cnc_integer_test() ->
    %% Number test
    ?assertMatch({ok, <<"13">>},       check_and_condense(<<" 13 ">>)),
    ?assertMatch({ok, <<"13.14">>},    check_and_condense(<<" 13.14 ">>)),
    ?assertMatch({ok, <<"0.1314E10">>}, check_and_condense(<<" 0.1314E10 ">>)),
    ?assertMatch({ok, <<"0.1314e0">>},  check_and_condense(<<" 0.1314e0 ">>)),
    ?assertMatch({ok, <<"0.1314E+2">>}, check_and_condense(<<" 0.1314E+2 ">>)),
    ?assertMatch({ok, <<"0.1314e-0">>}, check_and_condense(<<" 0.1314e-0 ">>)),
    ?assertMatch({ok, <<"1314e04">>}, check_and_condense(<<" 1314e04 ">>)),
    ?assertMatch({ok, <<"-1314E-04">>}, check_and_condense(<<" -1314E-04 ">>)).

cnc_integer_failed_test() ->
    ?assertMatch({badsymbol, check_and_condense, [<<"3">>, _States, 2, 1, _Condensed]},
                 check_and_condense(<<"03">>)),
    ?assertMatch({badsymbol, check_and_condense, [<<$\x{0020},_/binary>>, [integer_sign|_], 3, 1, <<"-">>]},
                 check_and_condense(<<" - 1314E-04 ">>)),
    ?assertMatch({badsymbol, check_and_condense, [<<"E-04 ">>, _States, 8, 1, <<"-1314">>]},
                 check_and_condense(<<" -1314 E-04 ">>)).

cnc_string_test() ->
    %% String test
    ?assertMatch({ok, <<"\"   hello,world 我要 \\n \\u10ffFF \""/utf8>>},
                 check_and_condense(<<" \n\"   hello,world 我要 \\n \\u10ffFF \" "/utf8>>)),
    ?assertMatch({badsymbol, check_and_condense, [<<$\x{000A},_/binary>>, _States, 3, 2, _Condensed]},
                 check_and_condense(<<" \n\" \nhello,world \" ">>)).

cnc_true_test() ->
    ?assertMatch({ok, <<"true">>}, check_and_condense(<<"true">>)).

cnc_false_test() ->
    ?assertMatch({ok, <<"false">>}, check_and_condense(<<"false">>)).

cnc_null_test() ->
    ?assertMatch({ok, <<"null">>}, check_and_condense(<<"null">>)).

next_coordinates($\x{000A}, _Column, Row) ->
    {1, Row+1};
next_coordinates(WS, Column, Row) when is_integer(WS) ->
    {Column+1, Row}.

-spec condense(Json_string :: binary()) -> Json_string :: binary().
condense(JSON_string) ->
    condense(JSON_string, <<>>, nil).

%% condense/3 condenses whitespaces not in string.
-spec condense(Json_string :: binary(), Accumulated_string :: binary(), On_off :: in_string | nil) -> Json_string :: binary().
condense(<<>>, <<Acc/binary>>, _) ->
    Acc;
condense(<<$\"/utf8, Rest/binary>>, <<Acc/binary>>, nil) ->
    condense(Rest, <<Acc/binary, $\"/utf8>>, in_string);
condense(<<$\"/utf8, Rest/binary>>, <<Acc/binary>>, in_string) ->
    condense(Rest, <<Acc/binary, $\"/utf8>>, nil);
condense(<<WS/utf8, Rest/binary>>, <<Acc/binary>>, nil) when WS == $\x20 orelse WS == $\x0A orelse WS == $\x0D orelse WS == $\x09 ->
    condense(Rest, Acc, nil);
condense(<<Word/utf8, Rest/binary>>, <<Acc/binary>>, On_off) ->
    condense(Rest, <<Acc/binary, Word/utf8>>, On_off).

condense_test() ->
    ?assert(<<"H\"e l \"lo">> == ecjson:condense(<<"H \"e l \" l o">>)).

