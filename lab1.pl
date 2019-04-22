:- module(lab1, [largo/2, todos_iguales/1, concatenacion/3, contenida/2, ww/2, wwR/2, sin_elem/3, sublista/2, enesimo/3, sublista/4, matriz/3, valor_celda/4, fila/3, col/3, diagonalD/3, sopa/3]).

%% ----------------
%% 		listas
%% ----------------

%% largo(?L,?N) <- N es el largo de la lista L.
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

%% contenida(?L1,?L2) <- todos los elementos de la lista L1 pertenecen a L2.
%% Ej.: contenida([a,b,a,a],[a,b,c]).
contenida([], _).
contenida([C|L1], L2) :- member(C, L2), sin_elem(L1, C, L1sinC), sin_elem(L2, C, L2sinC), contenida(L1sinC, L2sinC).

%% ww(?L,?V) <- La lista L es la concatenación consigo misma de una lista W, cuyos elementos pertenecen al conjunto representado por la lista V, largo(L) >= 2.
%% Ej.: ww([a,c,c,a,c,c],[a,b,c]).
ww(L, V) :- L \== [], concatenacion(L1, L1, L), contenida(L1, V).

%% wwR(?L,?V) <- La lista L es la concatenacion de una lista W y su reverso, con elementos pertenecientes al conjunto representado por la lista V, largo(L) >= 2.
%% Ej.: wwR([a,b,b,a],[a,b]), wwR([a,c,c,a],[a,b,c]).
wwR(L, V) :- L \== [], concatenacionR(L1, L1, L), contenida(L1, V).

%% Predicado auxiliar:
%% concatenacionR(+L1,+Ac,?L) <- L es la lista que resulta de concatenar el reverso de L1 con Ac, utilizando Ac como acumulador para una implementación eficiente.
%% Ej.: concatenacionR([a, b], [a, b], [b, a, a, b]).
concatenacionR([], Ac, Ac).
concatenacionR([X|L1], Ac, L) :- concatenacionR(L1, [X|Ac], L).

%% sin_elem(+L,?E,?LSinE) <- LSinE es la lista L sin ninguna ocurrencia del elemento E.
%% Ejs.: sin_elem([a,b,a,c],a,[b,c]), sin_elem([a,a],a,[]), sin_elem([b,c],a,[b,c]).
sin_elem([], _, []).
sin_elem([E|L1], E, L2) :- sin_elem(L1, E, L2).
sin_elem([C|L1], E, [C|L2]) :- C \= E, sin_elem(L1, E, L2).

%% sublista(?L,?Sub) <- Sub contiene un subconjunto de elementos contiguos de L en el mismo orden que aparecen en L.
%% Ej.: sublista([5,2,3,1,7],[2,3,1]).
sublista(L, S) :- sufijo(L, Sufijo), prefijo(Sufijo, S). % <- Las sublistas son prefijos de sufijos de la lista original.

%% Predicado auxiliar:
%% prefijo(?L,?P) <- La lista P es un prefijo de la lista L.
%% Ej.: prefijo([a,b,c],[a,b,c]), prefijo([a,b,c,d,e],[a,b,c]).
prefijo(L, P) :- concatenacion(P, _, L). % <- La lista P es un prefijo de la lista L.

%% Predicado auxiliar:
%% sufijo(?L,?S) <- La lista S es un sufijo de la lista L.
%% Ej.: sufijo([a,b,c],[a,b,c]), sufijo([a,b,c,d,e],[d,e]).
sufijo(L, S) :- concatenacion(_, S, L). % <- La lista S es un sufijo de la lista L.

%% enesimo(?L,?N,?E) <- El elemento E está en la N-sima posición en la lista L.
%% Ej.: enesimo([5,2,3,1,7],4,1). enesimo([5,2,[3,1],7],3,[3,1]).
enesimo(L, N, E) :- enesimo_ac(L, N, 1, E).

%% Predicado auxiliar:
%% enesimo_ac(?L,?N,?Ac,?E) <- E es el N-esimo elemento de la lista L. Se utiliza Ac como acumulador.
%% Ej.: enesimo_ac([5,2,3,1,7],4,1,1). enesimo_ac([5,2,[3,1],7],3,1,[3,1]).
enesimo_ac([E|_], Ac, Ac, E).
enesimo_ac([C|L], N, Ac, E) :- C \== E, Ac1 is Ac+1, enesimo_ac(L, N, Ac1, E).

%% sublista(?L,?Sub,?I,?J) <-   Sub contiene un subconjunto de elementos contiguos de L en el mismo orden que aparecen en L, empezando en la posición I-ésima
%%                              de L y terminado en la J-ésima.
%% Ej.: sublista([5,2,3,1,7],[2,3,1],2,4).
sublista(_, [], 0, 0).
sublista(L, [S], J, J) :- enesimo(L, J, S).
sublista(L, [S|Sub], I, J) :- enesimo(L, I, S), I1 is I+1, sublista(L, Sub, I1, J).


%% ------------------
%% 		matrices
%% ------------------

%% matriz(?M,?N,+A) ← A es una matriz de M filas y N columnas. La matriz se representa mediante una lista de M filas, donde cada fila es una lista de N celdas.
%% Ej.: matriz(3,3,[[8,-10,1],[5,4,2], [7,9,3]]).
matriz(0,_,[]).
matriz(M,N,[C|R]) :- matriz(M1,N,R), largo(C,N), M is M1+1.

%% valor_celda(+I,+J,+A,?E) <- E es el contenido de la celda (I,J) de la matriz A.
%% Ej.: valor_celda(2,1,[[8,-10,1],[5,4,2], [7,9,3]],5)
valor_celda(1,J,[C|_],E) :- valor_posicion(J,C,E).
valor_celda(I,J,[_|R],E) :- valor_celda(I1,J,R,E), I is I1+1.

%% valor_posicion(?J,+F,?E) ← E es el contenido de la posicion j de la fila F.
%% Ej.: valor_posicion(2,[2,3,5],3)
valor_posicion(1,[C|_],C).
valor_posicion(J,[_|R],E) :- valor_posicion(J1,R,E), J is J1+1.

%% fila(+M,?N,?F) <- F es la fila N-ésima de la matriz
%% Ej.: fila([[8,-10,1],[5,4,2],[7,9,3]],3,[7,9,3]).
fila([C|_],1,C).
fila([_|R],N,F) :- fila(R,N1,F), N is N1+1.

%% col(+M,?N,?C) <- C es la columna N-ésima de la matriz
%% Ej.: col([[8,-10,1],[5,4,2],[7,9,3]],2,[-10,4,9])
col([],_,[]).
col([C|R],N,[X|Y]) :- valor_posicion(N,C,X), col(R,N,Y).


%% diagonalD(+M,coord(?I,?J),?Dir) <- Dir es una diagonal de la matriz M, con índices de fila y de columna consecutivos crecientes. El 1er elemento de Dir tiene coordenadas I,J. Los elementos de la fila 1 y los de la columna 1 son los posibles 1eros elementos de Dir
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(1,2),[-10,2])
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(2,1),[5,9])
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(1,1),[8,4,3])
%% Ej.:diagonalD([[8,-10,1],[5,4,2], [7,9,3]],coord(3,1),[7])
diagonalD([C],coord(1,N),[E]) :- valor_posicion(N,C,E).
diagonalD(A,coord(1,J),[C]) :- matriz(_,N,A), J=N, valor_celda(1,J,A,C).
diagonalD([C|R],coord(1,J),[X|Y]) :- matriz(_,_,[C|R]), diagonalD(R,coord(1,J1),Y), valor_posicion(J,C,X),  J is J1-1.
diagonalD([_|R],coord(I,J),Dir) :- diagonalD(R,coord(I1,J),Dir), I is I1+1.



%% diagonalI(+M,coord((?I,?J),?Inv) <- Inv es una una diagonal inversa de la matriz M, con índices de fila consecutivos decrecientes y de columna consecutivos crecientes. El 1er elemento de Inv tiene coordenadas I,J. Los elementos de la columna 1 y los de la última fila son los posibles 1eros elementos de Inv
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(3,2),[9,2])
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(2,1),[5,-10])
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(1,1),[8])
%% Ej.:diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(3,1),[7,4,1])


%% sopa(+M,+Pals,?Coords) <- Coords es una lista de elementos de la forma p(Pal,((I1,J1),(I2,J2)))
%% - donde ((I1,J1),(I2,J2)) es el par de coordenadas que indica los índices inicial y final de la palabra Pal (lista de letras) en la matriz M
%% - debe haber un elemento en Coords para cada palabra de Pals
reverso_ac([],Ac,Ac).
reverso_ac([H|T],Ac,Reverso) :- reverso_ac(T,[H|Ac],Reverso).
reverso(Texto,Reverso) :- reverso_ac(Texto,[],Reverso).

buscaPalabra(Texto,Pal,Ini,Fin) :- sublista(Texto,Pal,Ini,Fin).
buscaPalabra(Texto,Pal,Ini,Fin) :- reverso(Pal,Reverso), sublista(Texto,Reverso,Fin,Ini).

coordPalabraFilas(M,Pals,p(PalN,[(NroFila,IniCol),(NroFila,FinCol)])) :-
        enesimo(Pals,_,PalN),
        fila(M,NroFila,Fila),
        buscaPalabra(Fila,PalN,IniCol,FinCol).

coordPalabraColumnas(M,Pals,p(PalN,[(IniFila,NroCol),(FinFila,NroCol)])) :-
        enesimo(Pals,_,PalN),
        col(M,NroCol,Columna),
        buscaPalabra(Columna,PalN,IniFila,FinFila).

coordPalabraDiagonal(M,Pals,p(PalN,[(NroFila1,NroCol1),(NroFila2,NroCol2)])) :-
        enesimo(Pals,_,PalN),
        diagonalD(M,coord(I,J),Diagonal),
        buscaPalabra(Diagonal,PalN,Ini,Fin),
        NroFila1 is I  + Ini - 1,
        NroCol1  is J  + Ini - 1,
        NroFila2 is I  + Fin - 1,
        NroCol2  is J  + Fin - 1.

coordPalabra(M,Pals,Coords) :- coordPalabraFilas(M,Pals,Coords).
coordPalabra(M,Pals,Coords) :- coordPalabraColumnas(M,Pals,Coords).
coordPalabra(M,Pals,Coords) :- coordPalabraDiagonal(M,Pals,Coords).

sopa(_,[],[]).
sopa(M,Pals,[CoordsH|CoordsT]) :-
        coordPalabra(M,Pals,CoordsH),
        CoordsH = p(Pal,[(_,_),(_,_)]),
        sin_elem(Pals,Pal,PalsSinE),
        sopa(M,PalsSinE,CoordsT).
