:- begin_tests(rdet).
:- use_module(prolog/rdet).

:- use_module(wrap_module).
:- use_module(wrap_dcg).
:- use_module(wrap_dict).
:- use_module(wrap_lambda).
:- use_module(wrap_rdf).

test(module_qualifier_implicit):-
    catch(module_implicit, E, true),
    assertion(E = error(goal_failed(det/0, wrap_module:_))).

test(module_qualifier_explicit):-
    catch(module_explicit, E, true),
    assertion(E = error(goal_failed(det/0, wrap_module:_))).

test(module_transitive):-
    catch(module_transitive, E, true),
    assertion(E = error(goal_failed(module_trans/0, wrap_module:_))).

test(dcg):-
    catch(wrap_dcg, E, true),
    assertion(E = error(goal_failed(one_c/2, wrap_dcg:_))).

test(dict):-
    \+ wrap_dict.

test(lambda):-
    \+ wrap_lambda.

test(rdf):-
    catch(wrap_rdf, E, true),
    assertion(E = error(goal_failed(rdf_test/1, wrap_rdf:_))).

:- end_tests(rdet).
