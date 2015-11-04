# Runtime determinacy checker for SWI-Prolog

Many predicates dealing with databases and external systems
are deterministic. Deterministic (det) predicates have the following
properties:

 * They should not fail.
 * The first solution is the right solution.
 * Errors are handled through exceptions.

This library attempts to provide runtime check for these conditions.

[![Build Status](https://travis-ci.org/rla/rdet.svg)](https://travis-ci.org/rla/rdet)

## API

Use directive `rdet/1` to annotate predicates that are supposed to be
deterministic:

    :- rdet(somepred/arity).

When annotated predicate call fails, an error is thrown:

    error(goal_failed(PredicateIndicator, Module:Line))

Where `Module` refers to module in which the call was made and
`Line` the location of the call inside the module.

## Example

    :- use_module(library(rdet)).

    :- rdet(insert_sort/2).
    :- rdet(i_sort/3).
    :- rdet(insert/3).

    insert_sort(List, Sorted):-
        i_sort(List, [], Sorted).

    i_sort([], Acc, Acc).
    i_sort([H|T], Acc, Sorted):-
        insert(H,Acc,NAcc),
        i_sort(T,NAcc,Sorted).

    insert(X,[Y|T],[Y|NT]):-
        X>Y,
        insert(X,T,NT).

    insert(X,[Y|T],[X,Y|T]):-
        X=<Y.

    insert(X,[a],[X]).

Running the sorting procedure will throw an error (the last clause of `insert/3` is
intentionally made to fail with numbers):

    ?- insert_sort([2,4,1,3], Sorted).
    ERROR: Unhandled exception: Goal insert/3 failed in module user on line 11.

## Compatibility

The checker is not compatible with all extensions based on
`goal_expansion` or `call/N`. This means that calls in some
places are not enhanced and determinism errors is not reported
for them.

### Call/N

As goal expansion is not called for terms inside (dynamic)
`call/1` and such, the predicate calls as `call/1` arguments
are not checked.

### DCG support

Checks work for DCG predicates, except on direct
calls in `phrase/2` and `phrase/3`. It seems likely that
`goal_expansion` is not applied for those. DCG calls in body
of another DCG predicate are suitable. Example:

    :- rdet(one_c//0).

    top   --> one_c.
    one_c --> `c`.

    wrap:-
        phrase(top, ``).

Calling `wrap` on the console will produce an error:

    ERROR: Unhandled exception: Goal one_c/2 failed in module user on line 5.

### Dict support

Dict functions are not supported. `goal_expansion` is not run on
the expanded calls.

### PlUnit support

PlUnit does its own term expansion and goals inside test bodies
cannot be enhanced. This seems to have effect on everything between
`begin_tests/1` and `end_tests/1`.

### Semantic web (RDF) support

Checks works together with `rdf_meta` declarations and prefix
expansion.

### Lambda pack support

Lambda bodies use `call/1` and calls inside there are thus not supported.

### Optimizations

Supports (tests run and checks are done) with `-O`.

## Overhead

Overhead on tight code is can be interesting. On the insert_sort
benchmark it makes the code run about 40%-50% **faster**.

With the `insert/3` fixed and

with annotations commented out:

    ?- findall(X, between(1, 1000, X), Xs), time(insert_sort(Xs, Sorted)).
    % 1,001,001 inferences, 0.386 CPU in 0.391 seconds (98% CPU, 2596612 Lips)
    Xs = Sorted, Sorted = [1, 2, 3, 4, 5, 6, 7, 8, 9|...] .

with annotations enabled:

    ?- findall(X, between(1, 1000, X), Xs), time(insert_sort(Xs, Sorted)).
    % 1,001,002 inferences, 0.217 CPU in 0.218 seconds (100% CPU, 4613383 Lips)
    Xs = Sorted, Sorted = [1, 2, 3, 4, 5, 6, 7, 8, 9|...].

These numbers were obtained with SWI 7.3.10 on a 2.4Ghz Q6600 CPU.

## How does it work?

Every annotated predicate call in predicate bodies gets wrapped into
an if-then-else.

Original code:

    ...
    somepred(Args),
    ...

Enhanced code:

    ...
    (   somepred(Args)
    ->  true
    ;   throw(error(goal_failed(...)))),

## Debugging

List of predicate calls that get enhanced:

    rdet:det(PredicateIndicator).

See if some predicate body has enhanced call:

    listing(somemodule:predicate/arity).

Enable `rdet` debug statements (before loading code to be enhanced):

    ?- debug(rdet).
    Warning: rdet: no matching debug topic (yet)
    true.

    ?- [insert].
    % rdet: adding goal: user:insert_sort/2
    % rdet: adding goal: user:i_sort/3
    % rdet: adding goal: user:insert/3
    % rdet: rewriting goal i_sort/3 at user:7
    % rdet: rewriting goal insert/3 at user:11
    % rdet: rewriting goal insert/3 at user:15
    true.

## Module named user?

Module named `user` usually contains the code that is not explicitly
put into a module.

## Why not a static system?

Good luck building a static analysis system that includes the following:

 * Instantiatedness (unbound, ground, partial)
 * Type information (list vs. atom, higher order types)
 * Module system support
 * Supports goal_expansion and other extensions
 * Higher order calls (maplist/call/etc)
 * Static exceptions
 * Different determinism modes: det/semidet/nondet/comitted choice
 * Determinism dependence on all previous

And has all of this optional: you can combine your code easily
with 3rd party untyped/unmoded libraries.

## Installation

To install as a package:

    pack_install(rdet).

Tested with SWI-Prolog 7.x but should work with earlier versions too.

## Running tests

In the package root, insert into swipl:

    [tests/tests].
    run_tests.

Or if you cloned the repo:

    make test

## Bug reports/feature requests

Please send bug reports/feature request through the GitHub
project [page](https://github.com/rla/rdet).

## License

The MIT license. See the LICENSE file.
