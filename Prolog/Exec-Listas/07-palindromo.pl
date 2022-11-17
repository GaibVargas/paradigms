reverso(X,Y) :- reverso(X,[],Y).
reverso([],X,X).
reverso([X|Y],Z,T) :- reverso(Y,[X|Z],T).

palindromo(L) :- reverso(L, L).