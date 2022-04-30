%% @doc A password generator.
%% @author Robert Lasu
%% @copyright 2022 Robert Lasu <robert.lasu@gmail.com>

-module(erlPass).
-include_lib("eunit/include/eunit.hrl").

-export([generate/5, generate/2]).

gen_number(B) when not(is_integer(B)) -> {error, not_number};
gen_number(B) when is_integer(B) and not(B < 0) -> integer_to_list(B rem 9);
gen_number(_) -> {error, badargs}.

gen_upper(B) when not(is_integer(B)) -> {error, not_number};
gen_upper(B) when is_integer(B) and not(B < 0) -> [65 + (B rem 26)];
gen_upper(_) -> {error, badargs}.

gen_lower(B) when not(is_integer(B)) -> {error, not_number};
gen_lower(B) when is_integer(B) and not(B < 0) -> [97 + (B rem 26)];
gen_lower(_) -> {error, badargs}.

gen_symbol({A, B}) when not(is_integer(A)) and not(is_integer(B)) -> {error, not_number};
gen_symbol({A, B}) when (B rem 4 == 0) and not(A < 0) -> [32 + A rem 16];
gen_symbol({A, B}) when (B rem 4 == 1) and not(A < 0) -> [58 + A rem 7];
gen_symbol({A, B}) when (B rem 4 == 2) and not(A < 0) -> [91 + A rem 6];
gen_symbol({A, B}) when (B rem 4 == 3) and not(A < 0) -> [123 + A rem 4];
gen_symbol(_) -> {error, badargs}.

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
%% `generate/2' takes the length of password and an argument list.
%% 
%% The list must contaion atoms `upper', `lower', `number' or `symbol'
%% @end

-spec generate(Len, OpsList) -> Password | {error, Reason} when
    Len         :: integer(),
    OpsList     :: list(),
    Password    :: list(),
    Reason      :: atom().
generate(_,[]) -> {error, invalid_options};
generate(Len,_) when Len < 1 -> {error, invalid_length};
generate(Len, List) -> generate(Len, check(upper, List), check(lower, List), check(number, List), check(symbol, List), [], seed()).

check(_, []) -> false;
check(Expected,[Expected | _]) -> true;
check(Expected, [_ | Tail]) -> check(Expected, Tail).


%% ----------------------------
%%          Unit Tests
%% ----------------------------
%% @private
gen_number_test() ->
    ?assert(gen_number(-1) =:= {error, badargs}),
    ?assert(gen_number(ew) =:= {error, not_number}),
    ?assert(gen_number(123) =:= "6").
%% @private
gen_upper_test() ->
    ?assert(gen_upper(-1) =:= {error, badargs}),
    ?assert(gen_upper(ew) =:= {error, not_number}),
    ?assert(gen_upper(123) =:= "T").
%% @private
gen_lower_test() ->
    ?assert(gen_lower(-1) =:= {error, badargs}),
    ?assert(gen_lower(ew) =:= {error, not_number}),
    ?assert(gen_lower(123) =:= "t").
%% @private
gen_symbol_test() ->
    ?assert(gen_symbol(-1) =:= {error, badargs}),
    ?assert(gen_symbol(ew) =:= {error, badargs}),
    ?assert(gen_symbol({123, 321}) =:= ">").
%% @private
generate_test() ->
    Pass = generate(10, [upper, lower, symbol]),
    ?assert(length(Pass) =:= 10).
%% @private
check_test() ->
    ?assert(check(atom, []) =:= false),
    ?assert(check(atom, [atom,atom2])),
    ?assert(check(atom, [atom2, atom3]) =:= false).


