numero(0, 'zero').
numero(1, 'um').
numero(2, 'dois').
numero(3, 'tres').
numero(4, 'quatro').
numero(5, 'cinco').
numero(6, 'seis').
numero(7, 'sete').
numero(8, 'oito').
numero(9, 'nove').
numerosParaPalavras([], []).
numerosParaPalavras([H|T], [P|T2]) :- numero(H, P), numerosParaPalavras(T, T2).