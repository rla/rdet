% Insertion sort benchmark with rdet annotations.

:- include(benchmark/conf).

:- use_module(prolog/rdet).

:- rdet(insert_sort/2).
:- rdet(i_sort/3).
:- rdet(insert/3).

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

insert(X,[],[X]).

benchmark:-
    list_size(Size),
    format('Insertion sort benchmark of ~w items with rdet annotations.~n', [Size]),
    findall(X, between(1, Size, X), Xs), time(insert_sort(Xs, _)).
