:- module(wrap_rdf, [
    wrap_rdf/0
]).

:- use_module(library(semweb/rdf_db)).
:- use_module(prolog/rdet).

:- rdf_register_prefix(ex, 'http://example.com/').

:- rdet(rdf_test/1).

:- rdf_meta(rdf_test(r)).

rdf_test(S):-
    rdf(S, rdf:type, ex:something).

wrap_rdf:-
    rdf_test(ex:thing).
