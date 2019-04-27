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
% qualifier attached. Use the given
% module for checking annotation.

handle_goal(Module:Goal, Out):-
    functor(Goal, Name, Arity), !,
    is_annotated(Module, Name, Arity),
    prolog_load_context(module, ContextModule),
    try_rewrite(Goal, ContextModule, Name, Arity, Out).

% Goal is implemented in some
% other module and then imported.

handle_goal(Goal, Out):-
    prolog_load_context(module, ContextModule),
    predicate_property(ContextModule:Goal,
        imported_from(FromModule)), !,
    functor(Goal, Name, Arity),
    is_annotated(FromModule, Name, Arity),
    try_rewrite(Goal, ContextModule, Name, Arity, Out).

% Other cases. Goal import is not
% found. Using same term as given.

handle_goal(Goal, Out):-
    functor(Goal, Name, Arity),
    prolog_load_context(module, ContextModule),
    is_annotated(ContextModule, Name, Arity),
    try_rewrite(Goal, ContextModule, Name, Arity, Out).

% Attempts goal rewrite in the given
% module. Does not apply rewrite when
% the goal has been rewritten already.
% Marks the goal rewritten.

try_rewrite(Goal, ContextModule, Name, Arity, Out):-
    goal_marker(ContextModule, Goal, Marker),
    \+ current_predicate(Marker),
    Marker = _:Aux/0,
    compile_aux_clauses([Aux]),
    rewrite_goal(Goal, Name, Arity, ContextModule, Out).

rewrite_goal(Goal, Name, Arity, ContextModule, Out):-
    prolog_load_context(term_position, Pos),
    stream_position_data(line_count, Pos, Line),
    At = ContextModule:Line,
    Functor = Name/Arity,
    debug(rdet, 'rdet: rewriting goal ~w at ~w', [Functor, At]),
    Out = ( Goal -> true
          ; throw(error(goal_failed(Functor, At), _))).

% Produces goal marker which is the byte
% position of the last read term.

goal_marker(ContextModule, Goal, Marker):-
    prolog_load_context(term_position, Pos),
    prolog_load_context(variable_names, Vars),
    stream_position_data(byte_count, Pos, Byte),
    with_output_to(string(GoalStr),
                   write_term(Goal, [variable_names(Vars), quoted(true)])),
    format(atom(Aux), '__aux_det_~w ~w', [Byte, GoalStr]),
    Marker = ContextModule:Aux/0,
    debug(rdet, 'rdet: marker: ~q', [Marker]).

% The actual expansion hook.

user:goal_expansion(In, Out):-
    nonvar(In),
    handle_goal(In, Out).

:- multifile(prolog:message//1).

% Provides messages to terminal.

prolog:message(error(goal_failed(Name/Arity, Module:Line), _)) -->
    ['Goal ~w failed in module ~w on line ~w.'-[Name/Arity, Module, Line]].

prolog:message(error(invalid_rdet_pi(PredicateIndicator), _)) -->
    ['Invalid rdet annotation: ~w (not a predicate indicator).'-[PredicateIndicator]].
