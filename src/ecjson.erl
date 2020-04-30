%% -*- coding: utf-8 -*-
-module(ecjson).
-include_lib("eunit/include/eunit.hrl").
-export([condense/1]).

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
