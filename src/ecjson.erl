%% -*- coding: utf-8 -*-
-module(ecjson).
%% Goal
%% - Make JSON being retrieved easily by deserailize it gracefully.
%% - Be informatical to end user.

-include_lib("eunit/include/eunit.hrl").
-export([condense/1,
         quickcheck/1]).

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

%% quickcheck/1 check if layout of delimiters is valid.
-spec quickcheck(Json_string :: binary()) -> ok | {no_match, Column :: integer(), Row :: integer(), Char :: integer()};
                ({condensed, Json_string :: binary()}) -> ok | {no_match, Nth :: integer(), Row :: integer(), Char :: integer()}.
quickcheck(<<Json_string/binary>>) ->
    quickcheck(Json_string, 0, 0);
quickcheck({condensed, <<Json_string/binary>>}) ->
    quickcheck_condensed(Json_string, [], 0).

-spec quickcheck(Json_string :: binary(), Column :: integer(), Row :: integer()) -> ok | {no_match, Column :: integer(), Row :: integer(), Char :: integer()}.
quickcheck(<<Json_string/binary>>, Column, Row) ->
    {no_match, Column, Row, 0}.

-spec quickcheck_condensed(Json_string :: binary(), Delims :: [{ Nth :: integer(), Char :: integer() }], Row :: integer()) -> ok | {no_match, Nth :: integer(), Row :: integer(), Char :: integer()}.
quickcheck_condensed(<<Json_string/binary>>, [{Nth, Char}], Row) ->
    {no_match, Nth, Row, Char}.

quickcheck_test() ->
    ?assertEqual(ok, quickcheck(<<"Hello">>, 0, 0)).

quickcheck_failed_test() ->
    ?assertEqual({no_match, 1, 2, $\"}, quickcheck(<<"{\"hello\":\n\"world}">>, 0, 0)).

quickcheck_condensed_test() ->
    ?assertEqual(ok, quickcheck_condensed(<<"Hello">>, [{0,0}], 0)).
