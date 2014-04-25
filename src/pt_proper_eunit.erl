-module( pt_proper_eunit ).
-export( [ parse_transform/2 ] ).

parse_transform( AST, _Options ) ->
	Forms = erl_syntax:form_list( AST ),
	Props = erl_syntax_lib:fold( fun find_props/2, [], Forms ),
	PropForms = lists:map( fun make_test/1, Props ),
	erl_syntax:revert_forms( [ Forms | PropForms ] ).

find_props( Node, Props ) ->
	try
		{ Name, 0 } = erl_syntax_lib:analyze_function( Node ),
		NameStr = atom_to_list( Name ),
		true = lists:prefix( "prop_", NameStr ),
		[ { NameStr, pos( Node ) } | Props ]
	catch
		throw:syntax_error    -> Props;
		error:{ badmatch, _ } -> Props
	end.

make_test( { Name, Pos } ) ->
	TestName = Name ++ "_test_",
	IsTrue = assert( call_quickcheck( call_prop( Name ), erl_syntax:nil() ) ),
	Fun = erl_syntax:fun_expr( [ erl_syntax:clause( none, [ IsTrue ] ) ] ),
	Tuple = erl_syntax:tuple( [ Pos, Fun ] ),
	erl_syntax:function( erl_syntax:atom( TestName ),
		[ erl_syntax:clause( none, [ Tuple ] ) ] ).

call_prop( Name ) ->
	erl_syntax:application( erl_syntax:atom( Name ), [] ).

call_quickcheck( Test, Opts ) ->
	erl_syntax:application( erl_syntax:atom( proper ),
		erl_syntax:atom( quickcheck ), [ Test, Opts ] ).

assert( X ) ->
	erl_syntax:match_expr( erl_syntax:atom( true ), X ).

pos( X ) ->
	erl_syntax:abstract( erl_syntax:get_pos( X ) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

x_test_() ->
	pt_proper_eunit_tests:tests().

-endif. % TEST
