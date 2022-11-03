%% @doc A password generator.
%% @author Robert Lasu
%% @copyright 2022 Robert Lasu <robert.lasu@gmail.com>

-module(erlPass).
-include_lib("eunit/include/eunit.hrl").

-define(TBI(Msg), exit(self(), {tbi, {?MODULE, ?LINE, Msg}})).
-define(ERR(Msg), {error, {?MODULE, ?LINE, Msg}}).

-export([generate/1, generate/2]).

gen_number({_A,B}) when not(is_integer(B)) -> ?ERR(not_number);
gen_number({_A,B}) when is_integer(B) and not(B < 0) -> integer_to_list(B rem 10);
gen_number(_) -> ?ERR(badargs).

gen_upper({_A,B}) when not(is_integer(B)) -> ?ERR(not_number);
gen_upper({_A,B}) when is_integer(B) and not(B < 0) -> [65 + (B rem 26)];
gen_upper(_) -> ?ERR(badargs).

gen_lower({_A,B}) when not(is_integer(B)) -> ?ERR(not_number);
gen_lower({_A,B}) when is_integer(B) and not(B < 0) -> [97 + (B rem 26)];
gen_lower(_) -> ?ERR(badargs).

gen_symbol({A, B}) when not(is_integer(A)) and not(is_integer(B)) -> ?ERR(not_number);
gen_symbol({A, B}) when (B rem 4 == 0) and not(A < 0) -> [32 + A rem 16];
gen_symbol({A, B}) when (B rem 4 == 1) and not(A < 0) -> [58 + A rem 7];
gen_symbol({A, B}) when (B rem 4 == 2) and not(A < 0) -> [91 + A rem 6];
gen_symbol({A, B}) when (B rem 4 == 3) and not(A < 0) -> [123 + A rem 4];
gen_symbol(_) -> ?ERR(badargs).

seed() ->
    <<A:24, B:24>> = crypto:strong_rand_bytes(6),
    {A,B}.

ops() -> #{upper => 0, lower => 0, number => 0, symbol => 0, exclude => ""}.

get_amount(Map) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, [X || {V,X} <- maps:to_list(Map), not(V == exclude)]).

validate_ops(Len, Map, Nr) when Nr >= Len -> Map;
validate_ops(_, _, _) -> ?ERR(to_few_chars).

create_ops_list(Len, [], Map) -> validate_ops(Len, Map, get_amount(Map));
create_ops_list(Len, [H | Tail], Map) when is_atom(H)-> 
    case maps:is_key(H, Map) of
        true ->
            create_ops_list(Len, Tail, maps:update(H, Len, Map));
        false ->
            ?ERR(invalid_options)
        end;
create_ops_list(Len, [{Ops, Nr} | Tail], Map) when (is_number(Nr)) and (Nr >= 0) ->
    case maps:is_key(Ops, Map) of
        true ->
            create_ops_list(Len, Tail, maps:update(Ops, Nr, Map));
        false ->
            ?ERR(invalid_options)
        end;
create_ops_list(Len, [{exclude, ExcChar} | Tail], Map) ->
    create_ops_list(Len, Tail, maps:update(exclude, ExcChar, Map));
create_ops_list(_, [{_,Nr} | _], _) when not(is_number(Nr))-> ?ERR(bad_args);
create_ops_list(_,_,_) -> ?ERR(badargs).


check_excluded([], _) -> true;
check_excluded([NewChar | _], [NewChar]) -> false;
check_excluded([_| List], NewChar) -> check_excluded(List, NewChar).

generate(Len, OpsMap, Pass, Fun, Op) ->
    case maps:get(Op, OpsMap) of
        0 ->
            generate(Len, OpsMap, Pass, seed());
        Nr ->
            NewChar = Fun(seed()),
            Bool = check_excluded(maps:get(exclude, OpsMap), NewChar),

            if
                Bool ->
                    NewMap = maps:update(Op, Nr-1, OpsMap),            
                    generate(Len-1, NewMap, [NewChar | Pass], seed());
                true ->
                    generate(Len, OpsMap, Pass, seed())
                end
        end.

generate(_, Err = {error, _}, _, _) -> Err;
generate(0, _, Pass, _) -> lists:flatten(Pass);
generate(Len, OpsMap, Pass, {A,_B}) when A rem 4 == 0 -> generate(Len, OpsMap, Pass, fun gen_number/1, number);
generate(Len, OpsMap, Pass, {A,_B}) when A rem 4 == 1 -> generate(Len, OpsMap, Pass, fun gen_upper/1, upper);
generate(Len, OpsMap, Pass, {A,_B}) when A rem 4 == 2 -> generate(Len, OpsMap, Pass, fun gen_lower/1, lower);
generate(Len, OpsMap, Pass, {A,_B}) when A rem 4 == 3 -> generate(Len, OpsMap, Pass, fun gen_symbol/1, symbol).

%% @doc Generates a password with length Len and options OpsList
%%
%% `generate/2' takes the length of password and a list of options.
%% OpsList must contaion the atoms `upper', `lower', `number', `symbol' and/or
%% tuples on the format `{upper, 2}', representing one of the atoms above and the maximum number of occurrences.
%% @end
-spec generate(Len, OpsList) -> Password | {error, Reason} when
    Len         :: integer(),
    OpsList     :: list(),
    Password    :: list(),
    Reason      :: term().
generate(_,[]) -> ?ERR(no_options);
generate(Len,_) when Len < 1 -> ?ERR(invalid_length);
generate(Len, List) -> generate(Len, create_ops_list(Len, List, ops()), [], seed()).


%% @doc Generates a password with length Len.
%% 
%% Calling `generate(10)'
%% is the same as calling
%%
%% `generate(10, [upper, lower, number, symbol])'.
%% 
%% @see generate/2
%% @end
generate(Len) -> generate(Len, [upper, lower, number, symbol]).

%% ----------------------------
%%          Unit Tests
%% ----------------------------
%% @private
gen_number_test() ->
    {error, {_,_,R1}} = gen_number({-1,-3}),
    ?assert(R1 =:= badargs),
    {error, {_,_,R2}} = gen_number({sd,tq}),
    ?assert(R2 =:= not_number),
    ?assert(gen_number({0, 126}) =:= "6").
%% @private
gen_upper_test() ->
    {error, {_, _, R1}} = gen_upper({-1,-1}),
    ?assert(R1 =:= badargs),
    {error, {_, _, R2}} = gen_upper({w,d}),
    ?assert(R2 =:= not_number),
    ?assert(gen_upper({32,123}) =:= "T").
%% @private
gen_lower_test() ->
    {error, {_,_,R1}} = gen_lower({-32,-4}),
    ?assert(R1 =:= badargs),
    {error, {_,_,R2}} = gen_lower({as,fas}),
    ?assert(R2 =:= not_number),
    ?assert(gen_lower({12,123}) =:= "t").
%% @private
gen_symbol_test() ->
    {error, {_,_,R1}} = gen_symbol({-32,-1}),
    ?assert(R1 =:= badargs),
    {error, {_,_,R2}} = gen_symbol({dsa,d}),
    ?assert(R2 =:= not_number),
    ?assert(gen_symbol({123, 321}) =:= ">"),
    {error, {_,_,R3}} = gen_symbol({"Warden", clyff}),
    ?assert(R3 =:= not_number).
%% @private
create_ops_list_test() ->
    Ops1 = create_ops_list(10, [upper], ops()),
    ?assert(maps:get(upper, Ops1) =:= 10),
    {error, {_,_,R1}} = create_ops_list(10, [this_is_wrong], ops()),
    ?assert(R1 =:= invalid_options),
    {error, {_,_,R2}} = create_ops_list(10, [{fah,10}], ops()),
    ?assert(R2 =:= invalid_options),
    {error, {_,_,R3}} = create_ops_list(32, [{upper, "32"}], ops()),
    ?assert(R3 =:= bad_args).

%% ----------------------------
%%      Integration Tests
%% ----------------------------
%% @private
generate_test() ->
    Pass = generate(100, [upper, lower, symbol, number]),
    generate(100, [lower, number]),
    ?assert(length(Pass) =:= 100),
    {error, {_,_,R1}} = generate(5,[]),
    ?assert(R1 =:= no_options),
    {error, {_,_,R2}} = generate(40, {upper,2}),
    ?assert(R2 =:= badargs),
    _Pass2 = generate(50, [{upper,1}, lower, {number,2}, {symbol,9}, {exclude, " !-.fdw"}]),
    {error, {_,_,R3}} = generate(0),
    ?assert(R3 =:= invalid_length),
    {error, {_,_,R4}} = generate(10, [{exclude, "ds"}]),
    ?assert(R4 =:= to_few_chars).
