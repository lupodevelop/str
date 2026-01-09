-module(str_ffi).
-export([decompose_nfd/1, remove_combining_marks/1, translit_replacements/0]).

%% Decomposizione NFD usando unicode:characters_to_nfd_binary/1
decompose_nfd(S) when is_binary(S) ->
    unicode:characters_to_nfd_binary(S).

%% Rimozione combining marks (U+0300-U+036F, etc.)
remove_combining_marks(S) when is_binary(S) ->
    filter_combining(S, <<>>).

filter_combining(<<>>, Acc) -> Acc;
filter_combining(<<C/utf8, Rest/binary>>, Acc) ->
    case is_combining_mark(C) of
        true -> filter_combining(Rest, Acc);
        false -> filter_combining(Rest, <<Acc/binary, C/utf8>>)
    end.

is_combining_mark(C) when C >= 16#0300, C =< 16#036F -> true;
is_combining_mark(C) when C >= 16#1AB0, C =< 16#1AFF -> true;
is_combining_mark(C) when C >= 16#1DC0, C =< 16#1DFF -> true;
is_combining_mark(C) when C >= 16#20D0, C =< 16#20FF -> true;
is_combining_mark(C) when C >= 16#FE20, C =< 16#FE2F -> true;
is_combining_mark(_) -> false.

%% Placeholder: transliteration replacements could be returned as a list of tuples
translit_replacements() -> [] .
