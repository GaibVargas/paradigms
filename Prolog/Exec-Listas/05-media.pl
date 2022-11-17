soma([], 0).
soma([H|T], S) :- soma(T, S1), S is S1 + H.

comprimento([], 0).
comprimento([H|T], C) :- comprimento(T, CT), C is CT + 1.

media([], 0).
media(L,X) :- soma(L, S), comprimento(L, C), X is S / C.