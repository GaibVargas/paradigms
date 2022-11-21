:- use_module(library(clpfd)).

board(6, [[[_,0],[_,1],[4,1],[_,1],[2,3],[_,4]],
          [[_,0],[_,2],[3,1],[_,3],[_,3],[_,3]],
          [[1,0],[4,0],[_,5],[4,3],[_,10],[_,10]],
          [[_,6],[5,7],[_,5],[_,9],[_,9],[2,10]],
          [[_,6],[_,7],[_,7],[_,8],[3,8],[_,10]],
          [[6,7],[2,7],[_,7],[2,8],[_,8],[5,8]]]).

groupSizeList(_, [], 0).
groupSizeList(X, [H|T], C) :- nth0(1, H, X), groupSizeList(X, T, CT), C is CT + 1.
groupSizeList(X, [H|T], C) :- nth0(1, H, Id), Id \== X, groupSizeList(X, T, C).

groupSize(_, [], 0).
groupSize(X, [H|T], C) :- groupSizeList(X, H, CT1), groupSize(X, T, CT2), C is CT1 + CT2.

isSameGroup(X, Elem) :- nth0(1, Elem, X).
groupElementsList(X, L, R) :- include(isSameGroup(X), L, R).

groupElements(_, [], []).
groupElements(X, [H|T], L) :- groupElementsList(X, H, L1), groupElements(X, T, L2), append(L1, L2, L).

extractElementValue(Elem, Value) :- nth0(0, Elem, Value).

groupValues(L, R) :- maplist(extractElementValue, L, R).