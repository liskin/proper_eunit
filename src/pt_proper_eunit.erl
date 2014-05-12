-module( pt_proper_eunit ).
-export( [ parse_transform/2 ] ).

parse_transform( AST, _Options ) ->
	Forms = erl_syntax:form_list( AST ),
	% TODO: use analyze_forms
	Opts = find_opts( Forms ),
	Props = find_props( Forms ),
	PropForms = [ test_generator( Prop, Opts ) || Prop <- Props ],
	erl_syntax:revert_forms( [ strip_opts( Forms ) | PropForms ] ).

find_opts( Forms ) ->
	erl_syntax_lib:fold( fun find_opts/2, [], Forms ).

find_opts( Node, Opts ) ->
	try
		{ proper_opts, Value } = erl_syntax_lib:analyze_wild_attribute( Node ),
		lists:map( fun opt_to_property/1, Value ) ++ Opts
	catch
		throw:syntax_error    -> Opts;
		error:{ badmatch, _ } -> Opts
	end.

strip_opts( Forms ) ->
	erl_syntax_lib:map( fun strip_opt/1, Forms ).

strip_opt( Node ) ->
	try
		{ proper_opts, _ } = erl_syntax_lib:analyze_wild_attribute( Node ),
		erl_syntax:comment( "stripped proper_opts" )
	catch
		throw:syntax_error    -> Node;
		error:{ badmatch, _ } -> Node
	end.

opt_to_property( X ) ->
	case is_prop_opt( X ) of
		true  -> X;
		false -> { global, X }
	end.

is_prop_opt( { Name, _ } ) ->
	is_prop_name( Name );
is_prop_opt( _ ) ->
	false.

is_prop_name( Name ) when is_atom( Name ) ->
	is_prop_name( atom_to_list( Name ) );
is_prop_name( Name ) when is_list( Name ) ->
	lists:prefix( "prop_", Name ).

find_props( Forms ) ->
	erl_syntax_lib:fold( fun find_props/2, [], Forms ).

find_props( Node, Props ) ->
	try
		{ Name, 0 } = erl_syntax_lib:analyze_function( Node ),
		true = is_prop_name( Name ),
		[ { Name, pos( Node ) } | Props ]
	catch
		throw:syntax_error    -> Props;
		error:{ badmatch, _ } -> Props
	end.

pos( X ) ->
	erl_syntax:abstract( erl_syntax:get_pos( X ) ).

test_generator( { Name, Pos }, AllOpts ) ->
	TestName = atom_to_list( Name ) ++ "_test_",
	Opts = proplists:append_values( Name, AllOpts )
		++ proplists:append_values( global, AllOpts ),
	Tuple = erl_syntax:tuple( [ Pos, test_fun_expr( Name, Opts ) ] ),
	erl_syntax:function( erl_syntax:atom( TestName ),
		[ erl_syntax:clause( none, [ Tuple ] ) ] ).

test_fun_expr( Name, Opts ) ->
	Assert = assert( call_quickcheck( call_prop( Name ), erl_syntax:abstract( Opts ) ) ),
	erl_syntax:fun_expr( [ erl_syntax:clause( none, [ Assert ] ) ] ).

assert( X ) ->
	erl_syntax:match_expr( erl_syntax:atom( true ), X ).

call_prop( Name ) ->
	erl_syntax:application( erl_syntax:atom( Name ), [] ).

call_quickcheck( Test, Opts ) ->
	erl_syntax:application( erl_syntax:atom( proper ),
		erl_syntax:atom( quickcheck ), [ Test, Opts ] ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

x_test_() ->
	pt_proper_eunit_tests:tests().

-endif. % TEST
