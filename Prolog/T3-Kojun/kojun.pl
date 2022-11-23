:- use_module(library(clpfd)).

board(6, [[0-_, 1-_, 1-4, 1-_, 3-2,  4-_],
          [0-_, 2-_, 1-3, 3-_, 3-_,  3-_],
          [0-1, 0-4, 5-_, 3-4, 10-_, 10-_],
          [6-_, 7-5, 5-_, 9-_, 9-_,  10-2],
          [6-_, 7-_, 7-_, 8-_, 8-3,  10-_],
          [7-6, 7-2, 7-_, 8-2, 8-_,  8-5]]).

board(10, [
  [0-_,  0-_,  1-_,  2-_,  5-_,  6-_,  6-4,  7-_,  7-3,  8-5],
  [0-_,  0-4,  3-_,  2-_,  6-_,  6-_,  7-6,  7-_,  7-_,  8-_],
  [0-_,  4-_,  3-_,  2-_,  2-2,  6-_,  7-_,  7-4,  8-_,  8-_],
  [4-_,  4-_,  3-_,  2-3,  2-_,  16-_, 17-_, 8-3,  8-2,  20-_],
  [4-2,  4-5,  9-_,  9-1,  9-2,  16-_, 16-4, 18-_, 19-_, 20-_],
  [4-_,  4-_,  10-_, 16-_, 16-6, 16-_, 21-_, 22-4, 19-_, 20-_],
  [10-_, 10-_, 10-_, 14-6, 14-_, 21-_, 21-_, 22-_, 19-_, 20-_],
  [11-_, 10-5, 10-_, 14-2, 15-_, 22-5, 22-_, 22-_, 23-_, 20-_],
  [11-_, 12-_, 14-7, 14-_, 14-5, 14-_, 22-_, 23-_, 23-_, 20-_],
  [11-_, 12-2, 12-_, 13-_, 13-4, 13-1, 13-_, 23-_, 23-3, 23-6]
]).

% Retorna o número de itens numa lista que possuem a mesma chave (mesmo grupo)
groupSizeList(_, [], 0).
groupSizeList(X, [H|T], C) :- pairs_keys([H], [X]), groupSizeList(X, T, CT), C is CT + 1.
groupSizeList(X, [H|T], C) :- pairs_keys([H], [Id]), Id \== X, groupSizeList(X, T, C).

% Retorna o número de casas de um determinado grupo
groupSize(_, [], 0).
groupSize(X, [H|T], C) :- groupSizeList(X, H, CT1), groupSize(X, T, CT2), C is CT1 + CT2.

% Cria as opções possíveis para uma casa do tabuleiro
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
  Values in 1..CT,
  R = I-Values.

% Cria as opções possíveis para todas as casas do tabuleiro
optionsBoard(B, Ob) :- maplist(maplist(optionElement), B, Ob).

% Verifica na lista há itens adjacentes com mesmo valor
validNeighbours([_]).
validNeighbours([H1,H2|T]) :-
  pairs_values([H1],V1),
  pairs_values([H2],V2),
  append(V1,V2,Values),
  all_distinct(Values),
  validNeighbours([H2|T]).

% Verifica na lista se itens adjacentes de mesma chave (mesmo grupo) estão em ordem decrescente
decreaseByGroup([_]).
decreaseByGroup([H1,H2|T]) :-
  pairs_keys([H1],K),
  pairs_keys([H2],K),
  pairs_values([H1],[V1]),
  pairs_values([H2],[V2]),
  V1 #> V2,
  decreaseByGroup([H2|T]).
decreaseByGroup([H1,H2|T]) :-
  pairs_keys([H1],[K1]),
  pairs_keys([H2],[K2]),
  K1 \== K2,
  decreaseByGroup([H2|T]).

% Resolve tabuleiros kojun
solver(X,R) :-
  board(X,B),
  nb_setval(currentBoard, B),

  % Cria tabuleiro com opções possíveis para cada célula
  optionsBoard(B,R),

  % Verifica se os grupos não possuem valores repetidos
  flatten(R,Rflat),
  keysort(Rflat, RflatSorted),
  group_pairs_by_key(RflatSorted, RflatGrouped),
  pairs_values(RflatGrouped, RValues),
  maplist(all_distinct, RValues),

  % Verifica se vizinhos não possuem valores repetidos
  maplist(validNeighbours, R),
  transpose(R, Columns),
  maplist(validNeighbours, Columns),

  % Verifica se células na mesma coluna e de mesmo grupo tem valor decrescente
  maplist(decreaseByGroup, Columns).

% Printa as linhas do tabuleiro resposta
printLine([]).
printLine([E]) :-
  pairs_values([E], [V]),
  write(V), nl.
printLine([H|T]) :-
  pairs_values([H], [V]),
  write(V), write(', '),
  printLine(T).

% Ponto de entra para a solução dos tabuleiros kojun
kojun(X) :- solver(X,R), maplist(printLine, R).