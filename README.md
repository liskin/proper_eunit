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

## Specifying options

Options for `proper:quickcheck/2` may be specified using the `proper_opts`
attribute:

    -proper_opts([500, any_to_integer]).

Options specific to one property may be specified as well:

    -proper_opts([{prop_1, [500, any_to_integer]}]).

To customize the eunit test object, `proper_eunit` understands the `eunit_env`
option:

    -proper_opts([{eunit_env, foo/1}])
    foo(Test) ->
        {setup, …, Test}.

Note that `foo/1` is equivalent to `{foo, 1}`, and `{M, F, A}` is supported as
well.

To just change the timeout, timeout may be specified directly (one doesn't
need to create a function that wraps `Test` in `{timeout, …}`):

    -proper_opts([{prop_1, {eunit_env, {timeout, 10}}}]).
