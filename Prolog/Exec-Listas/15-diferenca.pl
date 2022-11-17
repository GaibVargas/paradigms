membro(X, [X|_]) :- !.
membro(X, [_|T]) :- membro(X, T), !.

diferenca([], _, []).
diferenca([H1|T1], L, [H1|R]) :- not(membro(H1, L)), diferenca(T1, L, R).
diferenca([H1|T1], L, R) :- membro(H1, L), diferenca(T1, L, R).
