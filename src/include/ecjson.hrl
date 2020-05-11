-define(isWS(X), (X =:= $\x{0020} orelse
                  X =:= $\x{000A} orelse
                  X =:= $\x{000D} orelse
                  X =:= $\x{0009} orelse
                  X =:= "")).
-define(isDigit(X), (X >= $0 andalso
                     X =< $9)).
-define(isDigitNotZero(X), (X >= $1 andalso
                            X =< $9)).
-define(isTrueOrFalseOrNumm(X), (X =:= "true" orelse
                                 X =:= "false" orelse
                                 X =:= "null")).
-define(isCharNotEscape(X), (X =/= $" andalso
                             X =/= $\\ andalso
                             X >= $\x{0020} andalso
                             X =< $\x{10FFFF})).
-define(isEscapeCharNotHex(X), (X =:= $" orelse
                          X =:= $\\ orelse
                          X =:= $/ orelse
                          X =:= $b orelse
                          X =:= $f orelse
                          X =:= $n orelse
                          X =:= $r orelse
                          X =:= $t)).
-define(isHex(X), ((X >= $0 andalso X =< $9)
                   orelse
                     (X >= $A andalso X =< $F)
                   orelse
                     (X >= $a andalso X =< $f))).
-define(areAllHex(A,B,C,D), (?isHex(A) andalso
                             ?isHex(B) andalso
                             ?isHex(C) andalso
                             ?isHex(D))).
