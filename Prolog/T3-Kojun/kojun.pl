:- use_module(library(clpfd)).

board(6, [[0-_, 1-_, 1-4, 1-_, 3-2,  4-_],
          [0-_, 2-_, 1-3, 3-_, 3-_,  3-_],
          [0-1, 0-4, 5-_, 3-4, 10-_, 10-_],
          [6-_, 7-5, 5-_, 9-_, 9-_,  10-2],
          [6-_, 7-_, 7-_, 8-_, 8-3,  10-_],
          [7-6, 7-2, 7-_, 8-2, 8-_,  8-5]]).

groupSizeList(_, [], 0).
groupSizeList(X, [H|T], C) :- pairs_keys([H], [X]), groupSizeList(X, T, CT), C is CT + 1.
groupSizeList(X, [H|T], C) :- pairs_keys([H], [Id]), Id \== X, groupSizeList(X, T, C).

groupSize(_, [], 0).
groupSize(X, [H|T], C) :- groupSizeList(X, H, CT1), groupSize(X, T, CT2), C is CT1 + CT2.

% isSameGroup(X, Elem) :- nth0(1, Elem, X).
% groupElementsList(X, L, R) :- include(isSameGroup(X), L, R).
% 
% groupElements(_, [], []).
% groupElements(X, [H|T], L) :- groupElementsList(X, H, L1), groupElements(X, T, L2), append(L1, L2, L).
% 
% extractElementValue(Elem, Value) :- nth0(0, Elem, Value).
% 
% groupValues(L, R) :- maplist(extractElementValue, L, R).

optionElement(Elem, R) :-
  pairs_keys([Elem], Id),
  pairs_values([Elem], Value),
  nth0(0, Value, V),
  number(V),
  nth0(0, Id, I),
  R = I-V.
optionElement(Elem, R) :-
  pairs_values([Elem], Value),
  not(number(Value)),
  pairs_keys([Elem], Id),
  nth0(0, Id, I),
  nb_getval(currentBoard, B),
  groupSize(I, B, CT),
  R = I-1..CT.

optionsBoard(B, Ob) :- maplist(maplist(optionElement), B, Ob).

solver6(R) :-
  board(6,B),
  nb_setval(currentBoard, B),
  optionsBoard(B,R).