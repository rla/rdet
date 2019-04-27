:- module(rdet, [
    rdet/1  % +PredicateIndicator
]).

:- use_module(library(error)).

:- dynamic(det/1).

:- meta_predicate(rdet(:)).

rdet(PredicateIndicator):-
    must_be(ground, PredicateIndicator),
    (   (   PredicateIndicator = Module:Name/Arity
        ;   PredicateIndicator = Module:Name//Arity)
    ->  must_be(atom, Module),
        must_be(atom, Name),
        must_be(integer, Arity)
    ;   throw(error(invalid_rdet_pi(PredicateIndicator), _))),
    (   det(PredicateIndicator)
    ->  true
    ;   debug(rdet, 'rdet: adding goal: ~w', [PredicateIndicator]),
        assertz(det(PredicateIndicator))).

% Checks whether the predicate
% is annotated for determinism checks.

is_annotated(Module, Name, Arity):-
    det(Module:(Name/Arity)), !.

is_annotated(Module, Name, Arity):-
    DcgArity is Arity - 2,
    det(Module:(Name//DcgArity)).

% Goal comes with its own module
% qualifier attached. This qualifier will be preserved in
% the newly generated aux predicate name.

handle_goal(CallModule:Goal, Out):- !,
    nonvar(Goal),
    try_rewrite(Goal, CallModule, Out).

% Goal is implemented in some
% other module and then imported.

handle_goal(Goal, Out):-
    prolog_load_context(module, ContextModule),
    predicate_property(ContextModule:Goal,
        imported_from(CallModule)), !,
    try_rewrite(Goal, CallModule, Out).

% Other cases. Goal import is not
% found. Using same term as given. Checking annotation
% for the current module.

handle_goal(Goal, Out):-
    prolog_load_context(module, ContextModule),
    try_rewrite(Goal, ContextModule, Out).

try_rewrite(Goal, CallModule, Out):-
    functor(Goal, Name, Arity),
    is_annotated(CallModule, Name, Arity),
    aux_name(CallModule, Name, AuxName),
    ensure_aux(AuxName, Name, Arity),
    prolog_load_context(term_position, Pos),
    stream_position_data(line_count, Pos, Line),
    prolog_load_context(module, ContextModule),
    Goal =.. [_|Args],
    debug(rdet, 'rdet: rewriting goal ~w to ~w at ~w',
        [Name/Arity, AuxName/Arity, ContextModule:Line]),
    Out =.. [AuxName|Args].

% Provides aux name in the currently loaded module
% for the predicate defined in CallModule.

aux_name(CallModule, Name, AuxName):-
    format(atom(AuxName), '__aux_det_~w;~w', [CallModule, Name]).

% Makes sure that there is a generated aux predicate
% in the currently loaded module.

ensure_aux(AuxName, _, Arity):-
    prolog_load_context(module, ContextModule),
    PredicateIndicator = ContextModule:AuxName/Arity,
    current_predicate(PredicateIndicator), !.

ensure_aux(AuxName, Name, Arity):-
    length(Args, Arity),
    Head =.. [AuxName|Args],
    Goal =.. [Name|Args],
    Failure = throw(error(goal_failed(Name/Arity), _)),
    Body = (Goal -> true ; Failure),
    compile_aux_clauses([Head :- Body]).

% The actual expansion hook.

user:goal_expansion(In, Out):-
    nonvar(In),
    handle_goal(In, Out).

:- multifile(prolog:message//1).

% Provides messages to terminal.

prolog:message(error(goal_failed(Name/Arity, Module:Line), _)) -->
    ['Goal ~w failed in module ~w on line ~w.'-[Name/Arity, Module, Line]].

prolog:message(error(goal_failed(Name/Arity), _)) -->
    ['Goal ~w failed.'-[Name/Arity]].

prolog:message(error(invalid_rdet_pi(PredicateIndicator), _)) -->
    ['Invalid rdet annotation: ~w (not a predicate indicator).'-[PredicateIndicator]].
