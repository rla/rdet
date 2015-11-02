:- module(insert, [insert_sort/2]).

:- use_module(run_det).

:- run_det(insert_sort/2).
:- run_det(i_sort/3).
:- run_det(insert/3).

insert_sort(List, Sorted):-
    i_sort(List, [], Sorted).

i_sort([], Acc, Acc).
i_sort([H|T], Acc, Sorted):-
    insert(H,Acc,NAcc),
    i_sort(T,NAcc,Sorted).

insert(X,[Y|T],[Y|NT]):-
    X>Y,
    insert(X,T,NT).

insert(X,[Y|T],[X,Y|T]):-
    X=<Y.

insert(X,[a],[X]).
