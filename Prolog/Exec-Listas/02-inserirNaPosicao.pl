inserirNaPosicao(X, P, [], [X]).
inserirNaPosicao(X, 0, [H1|T1], [X,H1|T1]).
inserirNaPosicao(X, P, [H1|T1], [H1|T2]) :- P1 is P - 1, inserirNaPosicao(X, P1, T1, T2).