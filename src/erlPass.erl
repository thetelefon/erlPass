%% @doc A password generator.
%% @author Robert Lasu
%% @version 0.1.0
%% @copyright 2022 Robert Lasu

-module(erlPass).
-export([generate/5]).

gen_number(B) -> integer_to_list(B rem 9).

gen_upper(B) -> [65 + (B rem 26)].

gen_lower(B) -> [97 + (B rem 26)].

gen_symbol({A, B}) when B rem 4 == 0 -> [32 + A rem 16];
gen_symbol({A, B}) when B rem 4 == 1 -> [58 + A rem 7];
gen_symbol({A, B}) when B rem 4 == 2 -> [91 + A rem 6];
gen_symbol({A, B}) when B rem 4 == 3 -> [123 + A rem 4].

seed() ->
    <<A:24, B:24>> = crypto:strong_rand_bytes(6),
    {A,B}.


%% @doc Generates a password.
%% @end
-spec generate(Len, Up, Low, Num, Sym) -> Pass | {error, Reason} when
    Len    :: integer(),
    Up     :: boolean(),
    Low    :: boolean(),
    Num    :: boolean(),
    Sym    :: boolean(),
    Pass   :: list(),
    Reason :: atom().
generate(Len,_,_,_,_) when Len < 1 -> {error, invalid_length};
generate(Len, Up, Low, Num, Sym) -> generate(Len, Up, Low, Num, Sym, [], seed()).

%% @doc Generate a password
%% see {@link generate/5}
%% @end
-spec generate(Len, Up, Low, Num, Sym, Pass, Seed) -> RetPass when
    Len     :: integer(),
    Up      :: boolean(),
    Low     :: boolean(),
    Num     :: boolean(),
    Sym     :: boolean(),
    Pass    :: list(),
    Seed    :: tuple(),
    RetPass :: list().
generate(_,false,false,false,false,_,_) -> {error, no_char};
generate(0,_,_,_,_, Pass, _) -> lists:flatten(Pass);
generate(Len, Up, Low, Num, Sym, Pass, {A, B}) when A rem 4 == 0 -> generate(Num, Len, Up, Low, Num, Sym, fun gen_number/1, B, Pass);
generate(Len, Up, Low, Num, Sym, Pass, {A, B}) when A rem 4 == 1 -> generate(Up, Len, Up, Low, Num, Sym, fun gen_upper/1, B, Pass);
generate(Len, Up, Low, Num, Sym, Pass, {A, B}) when A rem 4 == 2 -> generate(Low, Len, Up, Low, Num, Sym, fun gen_lower/1, B, Pass);
generate(Len, Up, Low, Num, Sym, Pass, {A, _}) when A rem 4 == 3 -> generate(Sym, Len, Up, Low, Num, Sym, fun gen_symbol/1, seed(), Pass).

-spec generate(ToGen, Len, Up, Low, Num, Sym, Fun, Args, Pass) -> RetPass when
    ToGen    :: boolean(),
    Len      :: integer(),
    Up       :: boolean(),
    Low      :: boolean(),
    Num      :: boolean(),
    Sym      :: boolean(),
    Fun      :: function(),
    Args     :: term(),
    Pass     :: list(),
    RetPass  :: list().
generate(_, 0, _,_,_,_,_,_, Pass) -> Pass;
generate(true, Len, U, L, N, S, Fun, B, Pass) -> generate(Len-1, U, L, N, S, [ Fun(B) | Pass], seed());
generate(false, Len, U, L, N, S, _, _, Pass) -> generate(Len, U, L, N, S, Pass, seed()).