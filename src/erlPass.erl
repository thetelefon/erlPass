%% @doc A password generator.
%% @author Robert Lasu
%% @version 0.1.0
%% @copyright 2022 Robert Lasu

-module(erlPass).
-export([generate/5]).

gen_number(B) ->
    integer_to_list(B rem 9).

gen_upper(B) ->
    [65 + (B rem 26)].

gen_lower(B) ->
    [97 + (B rem 26)].

gen_symbol({B, A}) ->
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


%% @doc Generates a password.
%% @param Length: The length of password.
%% @end
-spec generate(Length, Upper, Lower, Number, Symbol) -> Pass when
    Length  ::integer(),
    Upper   ::boolean(),
    Lower   ::boolean(),
    Number  ::boolean(),
    Symbol  ::boolean(),
    Pass    ::list().
generate(Length,_,_,_,_) when Length < 1 -> {error, invalid_length};
generate(Length, Upper, Lower, Number, Symbol) -> generate(Length, Upper, Lower, Number, Symbol, []).

%% @doc Generate a password
%% see {@link generate/5}
%% @end
-spec generate(Length, Upper, Lower, Number, Symbol, Pass) -> RetPass when
    Length  ::integer(),
    Upper   ::boolean(),
    Lower   ::boolean(),
    Number  ::boolean(),
    Symbol  ::boolean(),
    Pass    ::list(),
    RetPass ::list().
generate(_,false,false,false,false,_) -> {error, no_char};
generate(0,_,_,_,_, Pass) -> lists:flatten(Pass);
generate(Length, Upper, Lower, Number, Symbol, Pass) ->
    {A, B} = seed(),

    case A rem 4 of
        0 -> generate(Number, Length, Upper, Lower, Number, Symbol, fun gen_number/1, B, Pass);
        1 -> generate(Upper, Length, Upper, Lower, Number, Symbol, fun gen_upper/1, B, Pass);
        2 -> generate(Lower, Length, Upper, Lower, Number, Symbol, fun gen_lower/1, B, Pass);
        3 -> generate(Symbol, Length, Upper, Lower, Number, Symbol, fun gen_symbol/1, {B,A}, Pass)
        end.

-spec generate(ToGen, Length, Upper, Lower, Number, Symbol, Fun, Args, Pass) -> RetPass when
    ToGen       :: boolean(),
    Length      :: integer(),
    Upper       :: boolean(),
    Lower       :: boolean(),
    Number      :: boolean(),
    Symbol      :: boolean(),
    Fun         :: function(),
    Args        :: term(),
    Pass        :: list(),
    RetPass     :: list().
generate(_, 0, _,_,_,_,_,_, Pass) -> Pass;
generate(true, Length, U, L, N, S, Fun, B, Pass) -> generate(Length-1, U, L, N, S, [ Fun(B) | Pass]);
generate(false, Length, U, L, N, S, _, _, Pass) -> generate(Length, U, L, N, S, Pass).