:- module(wrap_module, [
    det/0,
    module_implicit/0,
    module_explicit/0,
    module_transitive/0
]).

:- use_module(prolog/rdet).
:- use_module(tests/module_trans).

:- rdet(det/0).

module_implicit:-
    det.

module_explicit:-
    wrap_module:det.

det:-
    fail.

module_transitive:-
    module_trans.
