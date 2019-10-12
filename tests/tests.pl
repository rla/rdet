:- begin_tests(rdet).
:- use_module(prolog/rdet).

:- use_module(wrap_module).
:- use_module(wrap_dcg).
:- use_module(wrap_dict).
:- use_module(wrap_lambda).
:- use_module(wrap_rdf).

test(module_qualifier_implicit):-
    catch(module_implicit, E, true),
    assertion(E = error(goal_failed(wrap_module:det/0), _)).

test(module_qualifier_explicit):-
    catch(module_explicit, E, true),
    assertion(E = error(goal_failed(wrap_module:det/0), _)).

test(module_transitive):-
    catch(module_transitive, E, true),
    assertion(E = error(goal_failed(module_trans:module_trans/0), _)).

test(dcg):-
    catch(wrap_dcg, E, true),
    assertion(E = error(goal_failed(wrap_dcg:one_c//0), _)).

test(dict):-
    catch(wrap_dict, E, true),
    assertion(E = error(goal_failed(wrap_dict:fun/3), _)).

test(lambda):-
    catch(wrap_lambda, E, true),
    assertion(E = error(goal_failed(wrap_lambda:fails/2), _)).

test(rdf):-
    catch(wrap_rdf, E, true),
    assertion(E = error(goal_failed(wrap_rdf:rdf_test/1), _)).

:- end_tests(rdet).
