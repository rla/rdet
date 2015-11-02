:- module(wrap_dict, [
    wrap_dict/0
]).

:- use_module(prolog/rdet).

:- rdet(fun/3).

M.fun(F) := Z:-
    Z is M.x*F + M.y*F,
    (   2 =:= floor(Z)
    ->  fail
    ;   true).

wrap_dict:-
    D = wrap_dict{x: 1, y: 1},
    _ = D.fun(1).
