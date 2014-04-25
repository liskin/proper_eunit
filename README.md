# proper\_eunit

A simple parse transformation that creates an [EUnit][] test generator for every
[proper][] property.

[EUnit]: http://www.erlang.org/doc/apps/eunit/chapter.html
[proper]: https://github.com/manopapad/proper

## Usage example

ex1.erl:

    -module( ex1 ).
    -include_lib( "proper_eunit/include/pt_proper_eunit.hrl" ).

    prop_1() ->
        ?FORALL( X, integer( 0, 10 ), X =:= X ).

erl:

    Eshell V5.10.4  (abort with ^G)
    1> eunit:test( ex1, [verbose] ).
    ======================== EUnit ========================
    ex1:4: prop_1_test_ (module 'ex1')...[0.028 s] ok
    =======================================================
      Test passed.
    ok
