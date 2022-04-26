%% @doc A password generator.
%% @author Robert Lasu
%% @version 0.1.0
%% @copyright 2022 Robert Lasu <robert.lasu@gmail.com>

-module(erlPass).
-export([generate/5, generate/2]).

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
%% @deprecated May be removed at any time
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


%% @doc Generates a password.
%%
%% Instead of taking several arguments generate/2 takes an argument list.
%% The list must contaion atoms upper, lower, number or symbol
%% @end

-spec generate(Len, ListOps) -> Password | {error, Reason} when
    Len         :: integer(),
    ListOps     :: list(),
    Password    :: list(),
    Reason      :: atom().
generate(_,[]) -> {error, invalid_options};
generate(Len,_) when Len < 1 -> {error, invalid_length};
generate(Len, List) -> generate(Len, get_bool(upper, List), get_bool(lower, List), get_bool(number, List), get_bool(symbol, List), [], seed()).

get_bool(_, []) -> false;
get_bool(Expected,[Expected | _]) -> true;
get_bool(Expected, [_ | Tail]) -> get_bool(Expected, Tail).