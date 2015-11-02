:- module(wrap_lambda, [
    wrap_lambda/0
]).

:- use_module(library(lambda)).
:- use_module(prolog/rdet).

:- rdet(fails/0).

fails(_, _):-
    fail.

wrap_lambda:-
    maplist(X+\fails(X), [1,2,3]).
