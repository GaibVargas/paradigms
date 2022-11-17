posicao(X, [X|T], 0).
posicao(X, [_|T], P) :- posicao(X, T, P1), P is P1 + 1.