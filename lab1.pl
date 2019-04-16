:- module(lab1, [largo/2, todos_iguales/1, concatenacion/3, contenida/2, ww/2, sin_elem/3, matriz/3, valor_celda/4, fila/3, col/3]).

%% ----------------
%% 		listas
%% ----------------

%% largo(+L,?N) <- N es el largo de la lista L.
%% Ej.: largo([a,b,c],3).
largo(L, N) :- largo_ac(L, 0, N).

%% largo(+L,+Ac,?N) <- N es el largo de la lista L, calculado mediante el uso del acumulador Ac.
%% Ej.: largo_ac(L,0,N).
largo_ac([], Ac, Ac).
largo_ac([_|R], Ac, N) :- Ac1 is Ac+1, largo_ac(R, Ac1, N).


%% todos_iguales(?L) <- Todos los elementos de la lista L son iguales entre sí.
%% Ej.: todos_iguales([a,a,a]).
todos_iguales([]).
todos_iguales([_]).
todos_iguales([C,C|R]) :- todos_iguales([C|R]).

%% concatenacion(?L1,?L2,?L) <- La lista L es la concatenación de L1 con L2.
%% Ej.: concatenacion([a,b,c],[d,e],[a,b,c,d,e]).
concatenacion([], [], []).
concatenacion([C1|L1], [], [C1|L1]).
concatenacion([], [C2|L2], [C2|L2]).
concatenacion([C1|L1], [C2|L2], [C1|L]) :- concatenacion(L1, [C2|L2], L).

%% contenida(?L1,+L2) <- todos los elementos de la lista L1 pertenecen a L2.
%% Ej.: contenida([a,b,a,a],[a,b,c]).
contenida([], _).
contenida([C|L1], L2) :- member(C, L2), sin_elem(L1, C, L1sinC), sin_elem(L2, C, L2sinC), contenida(L1sinC, L2sinC).

%% ww(?L,+V) <- La lista L es la concatenación consigo misma de una lista W, cuyos elementos pertenecen al conjunto representado por la lista V, largo(L) >= 2.
%% Ej.: ww([a,c,c,a,c,c],[a,b,c]).
ww(L, V) :- L \= [], concatenacion(L1, L1, L), contenida(L1, V).

%% wwR(?L,+V) <- La lista L es la concatenacion de una lista W y su reverso, con elementos pertenecientes al conjunto representado por la lista V, largo(L) >= 2.
%% Ej.: wwR([a,b,b,a],[a,b]), wwR([a,c,a],[a,b,c]).

%% sin_elem(+L,?E,?LSinE) <- LSinE es la lista L sin ninguna ocurrencia del elemento E.
%% Ejs.: sin_elem([a,b,a,c],a,[b,c]), sin_elem([a,a],a,[]), sin_elem([b,c],a,[b,c]).
sin_elem([], _, []).
sin_elem([E|L1], E, L2) :- sin_elem(L1, E, L2).
sin_elem([C|L1], E, [C|L2]) :- C \= E, sin_elem(L1, E, L2).

%% ------------------
%% 		matrices
%% ------------------

matriz(0,_,[]).
matriz(M,N,[C|R]) :- M>0, M1 is M-1, matriz(M1,N,R), largo(C,N).

%% valor_celda(+I,+J,+A,?E) <- E es el contenido de la celda (I,J) de la matriz A.
%% Ej.: valor_celda(2,1,[[8,-10,1],[5,4,2], [7,9,3]],5)
valor_celda(1,J,[C|_],E) :- valor_posicion(J,C,E).
valor_celda(I,J,[_|R],E) :- I>1, I1 is I-1, valor_celda(I1,J,R,E).

%% valor_posicion(+J,+F,?E) ← E es el contenido de la posicion j de la fila F.
%% Ej.: valor_posicion(2,[2,3,5],3)
valor_posicion(1,[C|_],C).
valor_posicion(J,[_|R],E) :- J>1, J1 is J-1, valor_posicion(J1,R,E).

%% fila(+M,?N,?F) <- F es la fila N-ésima de la matriz
%% Ej.: fila([[8,-10,1],[5,4,2],[7,9,3]],3,[7,9,3]).
fila([C|_],1,C).
fila([_|R],N,F) :- N>0, N1 is N-1, fila(R,N1,F).

%% col(+M,?N,?C) <- C es la columna N-ésima de la matriz
%% Ej.: col([[8,-10,1],[5,4,2],[7,9,3]],2,[-10,4,9])
col([],_,[]).
col([C|R],N,[X|Y]) :- N>0, valor_posicion(N,C,X), col(R,N,Y).

%% diagonalD(+M,coord(?I,?J),?Dir) <- Dir es una diagonal de la matriz M, con índices de fila y de columna consecutivos crecientes. El 1er elemento de Dir tiene coordenadas I,J. Los elementos de la fila 1 y los de la columna 1 son los posibles 1eros elementos de Dir
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(1,2),[-10,2])
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(2,1),[5,9])
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(1,1),[8,4,3])
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(3,1),[7])

%% diagonalI(+M,coord((?I,?J),?Inv) <- Inv es una una diagonal inversa de la matriz M, con índices de fila consecutivos decrecientes y de columna consecutivos crecientes. El 1er elemento de Inv tiene coordenadas I,J. Los elementos de la columna 1 y los de la última fila son los posibles 1eros elementos de Inv
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(3,2),[9,2])
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(2,1),[5,-10])
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(1,1),[8])
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(3,1),[7,4,1])