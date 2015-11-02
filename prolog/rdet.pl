:- module(rdet, [
    rdet/1  % +Functor
]).

:- dynamic(det/1).

:- meta_predicate(rdet(:)).

% TODO: throw on invalid Functor.

rdet(Functor):-
    (   det(Functor)
    ->  true
    ;   assertz(det(Functor))).

% Checks whether the predicate
% is annotated for determinism checks.

is_annotated(Module, Name, Arity):-
    det(Module:(Name/Arity)), !.

is_annotated(Module, Name, Arity):-
    DcgArity is Arity - 2,
    det(Module:(Name//DcgArity)).

user:goal_expansion(In, Out):-
    functor(In, Name, Arity),
    prolog_load_context(module, Module),
    (   predicate_property(Module:In, imported_from(From))
    ->  is_annotated(From, Name, Arity)
    ;   is_annotated(Module, Name, Arity)),
    prolog_load_context(term_position, Pos),
    stream_position_data(line_count, Pos, Line),
    stream_position_data(byte_count, Pos, Byte),
    format(atom(Aux), '__aux_det_~w', [Byte]),
    (   current_predicate(Module:Aux/0)
    ->  Out = In
    ;   Out = ( In -> true
              ; throw(error(goal_failed(Name/Arity, Module:Line)))),
        compile_aux_clauses([Aux])).

:- multifile(prolog:message//1).

% Provides message to terminal.

prolog:message(error(goal_failed(Name/Arity, Module:Line))) -->
    ['Goal ~w failed in module ~w on line ~w.'-[Name/Arity, Module, Line]].
