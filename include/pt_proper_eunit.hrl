-ifndef( PT_PROPER_EUNIT_HRL ).
-define( PT_PROPER_EUNIT_HRL, true ).

-include_lib( "proper/include/proper.hrl" ).
-compile( { parse_transform, pt_proper_eunit } ).
-include_lib( "eunit/include/eunit.hrl" ).

-endif. % PT_PROPER_EUNIT_HRL
