:- use_module(prolog/rdet).

:- maplist(rdet, [t1/2, t2/2, t3/2]).

t1(A, B) :-
    t2(A, X),
    t3(X, B).

t2(A, B) :-
    t3(A, B).
