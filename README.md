# Runtime non-failure checker for SWI-Prolog

Many predicates dealing with databases and external systems
are non-failure. These predicates have the following
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

    error(goal_failed(PredicateIndicator), _)

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
    ERROR: Unhandled exception: Goal insert/3 failed.

## How does it work?

The library uses SWI-Prolog built-in predicate `wrap_predicate` to insert
necessary checks.

Previous version of this library was using goal expansion.

## Debugging

Enable `rdet` debug statements (before loading code to be enhanced):

    ?- debug(rdet).
    Warning: rdet: no matching debug topic (yet)
    true.

    ?- [insert].
    % rdet: adding goal: user:insert_sort/2
    % rdet: adding goal: user:i_sort/3
    % rdet: adding goal: user:insert/3
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

It requires a recent SWI-Prolog version 8.x or newer.

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
