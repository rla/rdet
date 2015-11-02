:- module(wrap_dcg, [
    wrap_dcg/0
]).

:- use_module(prolog/rdet).

:- rdet(one_c//0).

top --> one_c.

one_c --> `c`.

wrap_dcg:-
    phrase(top, ``).
