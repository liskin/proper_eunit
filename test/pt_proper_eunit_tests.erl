-module(pt_proper_eunit_tests).
-export([tests/0]).

-define(EUNIT_NOAUTO, 1).
-include("pt_proper_eunit.hrl").

tests() ->
	Opts = [no_tty],
	[ ?_assertMatch(error, eunit:test({generator, fun prop_1_test_/0}, Opts))
	, ?_assertMatch(ok   , eunit:test({generator, fun prop_2_test_/0}, Opts))
	, ?_assertMatch(ok   , eunit:test({generator, fun prop_3_test_/0}, Opts))
	, ?_assertMatch(ok   , eunit:test({generator, fun prop_4_test_/0}, Opts))
	].

prop_1() ->
	?FORALL(X, integer(0, 10), X =:= X + 1).

prop_2() ->
	?FORALL(X, integer(0, 10), X =:= X).

-proper_opts([{prop_3, [any_to_integer]}]).
prop_3() ->
	?FORALL(X, any(), is_integer(X)).

-proper_opts([{prop_4, {eunit_env, [setup_ets/1, {timeout, 1}]}}]).
setup_ets(Test) ->
	Setup = fun() ->
		T = ets:new(?MODULE, [public, named_table]),
		ets:insert(T, [{X,X} || X <- lists:seq(0, 10)]),
		T
	end,
	{setup, Setup, fun ets:delete/1, Test}.

prop_4() ->
	?FORALL(X, integer(0, 10), equals([{X, X}], ets:lookup(?MODULE, X))).
