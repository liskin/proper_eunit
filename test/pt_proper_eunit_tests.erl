-module( pt_proper_eunit_tests ).
-export( [ tests/0 ] ).

-define( EUNIT_NOAUTO, 1 ).
-include( "pt_proper_eunit.hrl" ).

tests() ->
	Opts = [ no_tty ],
	[ ?_assertMatch( error, eunit:test( { generator, fun prop_1_test_/0 }, Opts ) )
	, ?_assertMatch( ok   , eunit:test( { generator, fun prop_2_test_/0 }, Opts ) )
	].

prop_1() ->
	?FORALL( X, integer( 0, 10 ), X =:= X + 1 ).

prop_2() ->
	?FORALL( X, integer( 0, 10 ), X =:= X ).
