-module(erlPass).

-export([generate/5]).

gen_number(B) ->
    integer_to_list(B rem 9).

gen_upper(B) ->
    [65 + (B rem 26)].

gen_lower(B) ->
    [97 + (B rem 26)].

gen_symbol(B, A) ->
    case B rem 4 of
        0 ->
            [32 + A rem 16];
        1 ->
            [58 + A rem 7];
        2 ->
            [91 + A rem 6];
        3 ->
            [123 + A rem 4]
        end.

seed() ->
    <<A:24, B:24>> = crypto:strong_rand_bytes(6),
    {A,B}.

generate(Length,_,_,_,_) when Length < 1 -> {error, invalid_length};
generate(Length, Upper, Lower, Number, Symbols) -> generate(Length, Upper, Lower, Number, Symbols, []).

generate(0,_,_,_,_, List) -> lists:flatten(List);
generate(Length, Upper, Lower, Number, Symbols, List) ->
    {A, B} = seed(),

    case A rem 4 of
        0 ->
            if
            Number ->
                generate(Length-1, Upper, Lower, Number, Symbols, [gen_number(B) | List]);
            true ->
                generate(Length, Upper, Lower, Number, Symbols, List)
            end;
        1 ->
            if
            Upper ->
                generate(Length-1, Upper, Lower, Number, Symbols, [gen_upper(B) | List]);
            true ->
                generate(Length, Upper, Lower, Number, Symbols, List)
            end;
        2 ->
            if
            Lower ->
                generate(Length-1, Upper, Lower, Number, Symbols, [gen_lower(B) | List]);
            true ->
                generate(Length, Upper, Lower, Number, Symbols, List)
            end;
        3 ->
            if
            Symbols ->
                generate(Length-1, Upper, Lower, Number, Symbols, [gen_symbol(B, A) | List]);
            true ->
                generate(Length, Upper, Lower, Number, Symbols, List)
            end
        end.