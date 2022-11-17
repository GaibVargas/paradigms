menor([], 0).
menor([X], X).
menor([H1,H2|T], M) :- H1 < H2, menor([H1|T], M).
menor([H1,H2|T], M) :- H1 >= H2, menor([H2|T], M).