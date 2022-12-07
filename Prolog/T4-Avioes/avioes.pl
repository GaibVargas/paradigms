% Resolve o puzzle do link: https://rachacuca.com.br/logica/problemas/esquadrilha-de-avioes-ii/
% Consulta: solucao(L).

nome(farfarelli).
nome(milton).
nome(nascimento).
nome(rui).
nome(walter).

gentilico(gaucho).
gentilico(baiano).
gentilico(fluminense).
gentilico(mineiro).
gentilico(paulista).

cor(amarela).
cor(azul).
cor(branca).
cor(verde).
cor(vermelha).

anomalia(altimetro).
anomalia(bussola).
anomalia(hidraulico).
anomalia(radio).
anomalia(temperatura).

bebida(agua).
bebida(cafe).
bebida(cha).
bebida(cerveja).
bebida(leite).

esporte(equitacao).
esporte(futebol).
esporte(natacao).
esporte(pesca).
esporte(tenis).

% X está à ao lado de Y
aoLado(X,Y,Lista) :- nextto(X,Y,Lista);nextto(Y,X,Lista).
                       
% X está à esquerda de Y (em qualquer posição à esquerda)
aEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), 
                        nth0(IndexY,Lista,Y), 
                        IndexX < IndexY.
                        
% X está à direita de Y (em qualquer posição à direita)
aDireita(X,Y,Lista) :- aEsquerda(Y,X,Lista).

% X está imediatamente à esquerda de Y
imediatamenteEsquerda(X, Y, List) :- nextto(X, Y, List).

% X está imediatamente à esquerda de Y
imediatamenteDireita(X, Y, List) :- nextto(Y, X, List).

% X é o primeiro da lista
primeiro(X,[X|_]).

todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).

solucao(ListaSolucao) :-

  ListaSolucao = [
    aviao(Nome1, Gentilico1, Cor1, Anomalia1, Bebida1, Esporte1),
    aviao(Nome2, Gentilico2, Cor2, Anomalia2, Bebida2, Esporte2),
    aviao(Nome3, Gentilico3, Cor3, Anomalia3, Bebida3, Esporte3),
    aviao(Nome4, Gentilico4, Cor4, Anomalia4, Bebida4, Esporte4),
    aviao(Nome5, Gentilico5, Cor5, Anomalia5, Bebida5, Esporte5)
  ],

  % Farfarelli a esquerda de quem gosta de futebol
  aEsquerda(aviao(farfarelli, _, _, _, _, _), aviao(_, _, _, _, _, futebol), ListaSolucao),

  % Quem gosta de água também gosta de pescar
  member(aviao(_, _, _, _, agua, pesca), ListaSolucao),

  % Walter entre praticantes de equitacao e tenis, nesta ordem
  aDireita(aviao(walter, _, _, _, _, _), aviao(_, _, _, _, _, equitacao), ListaSolucao),
  aEsquerda(aviao(walter, _, _, _, _, _), aviao(_, _, _, _, _, tenis), ListaSolucao),

  % Quem joga tenis está no meio da formação
  Esporte3 = tenis,

  % Avião com anomalia no rádio, o piloto que gosta de café e o mineiro estão nessa ordem na formação
  aEsquerda(aviao(_, _, _, radio, _, _), aviao(_, _, _, _, cafe, _), ListaSolucao),
  aEsquerda(aviao(_, _, _, _, cafe, _), aviao(_, mineiro, _, _, _, _), ListaSolucao),

  % Piloto que gosta de chá está entre os que gostam de cerveja e café, nesta ordem
  aDireita(aviao(_, _, _, _, cha, _), aviao(_, _, _, _, cerveja, _), ListaSolucao),
  aEsquerda(aviao(_, _, _, _, cha, _), aviao(_, _, _, _, cafe, _), ListaSolucao),

  % Piloto que gosta de cerveja está na posição mais a esquerda
  primeiro(aviao(_, _, _, _, cerveja, _), ListaSolucao),

  % Piloto que bebe água está ao lado do avião com anomalia na bússola
  aoLado(aviao(_, _, _, _, agua, _), aviao(_, _, _, bussola, _, _), ListaSolucao),

  % Avião com anomalia no altímetro está a direita do piloto Rui
  aDireita(aviao(_, _, _, altimetro, _, _), aviao(rui, _, _, _, _, _), ListaSolucao),

  % Quem gosta de futebol está exatamente à direita do avião com anomalia no altímetro
  imediatamenteDireita(aviao(_, _, _, _, _, futebol), aviao(_, _, _, altimetro, _, _), ListaSolucao),

  % Rui está à esquerda do avião com anomalia na bússola
  aEsquerda(aviao(rui, _, _, _, _, _), aviao(_, _, _, bussola, _, _), ListaSolucao),

  % Avião com anomalia no rádio, hidráulico e na temperatura estão nessa ordem
  aEsquerda(aviao(_, _, _, radio, _, _), aviao(_, _, _, hidraulico, _, _), ListaSolucao),
  aEsquerda(aviao(_, _, _, hidraulico, _, _), aviao(_, _, _, temperatura, _, _), ListaSolucao),

  % Avião com fumaça verde está à direita do Farfarelli 
  aDireita(aviao(_, _, verde, _, _, _), aviao(farfarelli, _, _, _, _, _), ListaSolucao),

  % Avião do Rui solta fumaça branca
  member(aviao(rui, _, branca, _, _, _), ListaSolucao),

  % O piloto que gosta de chá pilota o avião de fumaça azul
  member(aviao(_, _, azul, _, cha, _), ListaSolucao),

  % O paulista está exatamente à esquerda do que solta fumaça amarela
  imediatamenteEsquerda(aviao(_, paulista, _, _, _, _), aviao(_, _, amarela, _, _, _), ListaSolucao),

  % Rui está ao lado do mineiro
  aoLado(aviao(rui, _, _, _, _, _), aviao(_, mineiro, _, _, _, _), ListaSolucao),

  % Gaúcho está exatamente à direita de quem gosta de cerveja
  imediatamenteDireita(aviao(_, gaucho, _, _, _, _), aviao(_, _, _, _, cerveja, _), ListaSolucao),

  % Fluminense está ao lado do avião de fumaça azul
  aoLado(aviao(_, fluminense, _, _, _, _), aviao(_, _, azul, _, _, _), ListaSolucao),

  % Nascimento está ao lado de quem gosta de chá
  aoLado(aviao(nascimento, _, _, _, _, _), aviao(_, _, _, _, cha, _), ListaSolucao),

  % Testa todas as possibilidades
  nome(Nome1),
  nome(Nome2),
  nome(Nome3),
  nome(Nome4),
  nome(Nome5),
  todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),

  gentilico(Gentilico1),
  gentilico(Gentilico2),
  gentilico(Gentilico3),
  gentilico(Gentilico4),
  gentilico(Gentilico5),
  todosDiferentes([Gentilico1, Gentilico2, Gentilico3, Gentilico4, Gentilico5]),

  cor(Cor1),
  cor(Cor2),
  cor(Cor3),
  cor(Cor4),
  cor(Cor5),
  todosDiferentes([Cor1, Cor2, Cor3, Cor4, Cor5]),

  anomalia(Anomalia1),
  anomalia(Anomalia2),
  anomalia(Anomalia3),
  anomalia(Anomalia4),
  anomalia(Anomalia5),
  todosDiferentes([Anomalia1, Anomalia2, Anomalia3, Anomalia4, Anomalia5]),

  bebida(Bebida1),
  bebida(Bebida2),
  bebida(Bebida3),
  bebida(Bebida4),
  bebida(Bebida5),
  todosDiferentes([Bebida1, Bebida2, Bebida3, Bebida4, Bebida5]),

  esporte(Esporte1),
  esporte(Esporte2),
  esporte(Esporte3),
  esporte(Esporte4),
  esporte(Esporte5),
  todosDiferentes([Esporte1, Esporte2, Esporte3, Esporte4, Esporte5]).