:- module(lab1,
[
largo/2,
todos_iguales/2,
concatenacion/3,
merge_ordenado/3,
sin_elem/3,
rotacion/2,
subsecuencia/2,
jugada_valida/3,
matriz/4,
valor_celda/4,
nuevo_valor_celda/5,
adyacente/6,
matriz_f/4,
valor_celda_f/4,
nuevo_valor_celda_f/4,
adyacente_f/6
]).

%   ----------------------------
%	 1) Predicados sobre listas
%   ----------------------------
 
 
%	largo(+L,?N) ← N es el largo de la lista L.
%	Ej.: largo([a,b,c],3).
largo([],0).
largo([_|R], X) :- largo(R, Y), X is Y+1.


%	todos_iguales(?L,?E) ← Todos los elementos de la lista L son iguales a E.
%	Ej.: todos_iguales([a,a,a],a).
todos_iguales([X],X).
todos_iguales([C|R],C) :- todos_iguales(R,C).

%	concatenacion(?L1,?L2,?L) ← La lista L es la concatenación de L1 con L2.
%	Ej.: concatenacion([a,b,c],[d,e],[a,b,c,d,e]).
concatenacion([], L, L). 
concatenacion([C|R1], L, [C|R2]):- concatenacion(R1, L, R2).


%	merge_ordenado(+L1,+L2,?L) ← L es el resultado de combinar ordenadamente las listas ordenadas L1 y L2.
%	Ej.: merge_ordenado([1,5,9,12],[3,5,10],[1,3,5,5,9,10,12]).
merge_ordenado(L,[],L).
merge_ordenado([],L,L).
merge_ordenado([C1|R1],[C2|R2],L) :- C1<C2, merge_ordenado(R1,[C2|R2],L2),concatenacion([C1|[]],L2,L).
merge_ordenado([C1|R1],[C2|R2],L) :- C1>=C2, merge_ordenado([C1|R1],R2,L2),concatenacion([C2|[]],L2,L).


%	sin_elem(+L,?E,?LSinE) ← LSinE es la lista L sin una ocurrencia del elemento E.
%	Ejs.: sin_elem([a,b,a,c],a,[b,a,c]).
%	sin_elem([a,b,a,c],a,[a,b,c]).
sin_elem([C|R],C,R).
sin_elem([C|R1],X,[C|R2]) :- sin_elem(R1,X,R2).


%	rotacion(+L,?R) ← La lista R es una rotación de la lista L.
%	Ejs.: rotacion([1,2,3,4,5],[4,5,1,2,3]).
%	rotacion([],[]).
rotacion(L, R) :- concatenacion(A, B, L), concatenacion(B, A, R).


%	subsecuencia(?L,?Subsec) ← Subsec contiene un subconjunto de elementos de L en
%	el mismo orden que aparecen en L.
%	Ej.: subsecuencia([5,2,3,1,7],[2,1]).
subsecuencia(_,[]).
subsecuencia([C|R1],[C|R2]) :- subsecuencia(R1,R2).
subsecuencia([_|R1],[C2|R2]) :- subsecuencia(R1,[C2|R2]).


%   -----------------------
%	  2 ) Juego de cartas
%   -----------------------


%	total(+L,?T) ← T es la suma de los numeros de las cartas en la lista L
total([],0).
total([carta(V,_)|R],T) :- total(R,T1), T is V+T1.


%	jugada_valida(+Mano,+Tapete,?Jugada) ← Jugada es una jugada válida para un jugador con Mano
%	cartas en la mano y Tapete cartas en el tapete
%	Una carta se representa como el término: carta(Numero,Palo) donde Numero es un entero (por
%	simplicidad va del 1 al 10, no consideramos los cambios de valor de sota, caballo y rey) y Palo es
%	un átomo que puede valer espada, oro, copa, o basto.
%	Mano y Tapete son listas de cartas.
%	Jugada es un término jugada(CartaMano, CartasTapete), donde CartaMano es una carta de la
%	mano del jugador y CartasTapete es una lista de cartas presentes en el tapete.
jugada_valida(M,T,jugada(M1, T1)) :- subsecuencia(T,T1), member(M1,M), total([M1|T1],15).


%   --------------------------------
%	  3) Predicados sobre matrices
%   --------------------------------


%	matriz(+M,+N,+E,?A) ← A es una matriz de M filas y N columnas. Cada celda debe 
%	tener el valor e. La matriz se representa mediante una lista de M filas, donde 
%	cada fila es una lista de N celdas.
%	Ej.: matriz(2,2,-10,[[-10,-10],[-10,-10]]).
matriz(0,_,_,[]).
matriz(M,N,E,[C|R]) :- M>0, M1 is M-1, matriz(M1,N,E,R), fila(N,E,C).


%	fila(+N,+E,?F) ← F es una fila de N elementos de valor E
%	Ej.: fila(3,5,[5,5,5]).
fila(0,_,[]).
fila(N,E,[E|R]) :- N>0, N1 is N-1, fila(N1,E,R).


%	valor_celda(+I,+J,+A,?E) ← E es el contenido de la celda (I,J) de la matriz A.
valor_celda(1,J,[C|_],E) :- valor_posicion(J,C,E).
valor_celda(I,J,[_|R],E) :- I>1, I1 is I-1, valor_celda(I1,J,R,E).


%	valor_posicion(+J,+F,?E) ← E es el contenido de la posicion j de la fila F.
valor_posicion(1,[C|_],C).
valor_posicion(J,[_|R],E) :- J>1, J1 is J-1, valor_posicion(J1,R,E).


%	nuevo_valor_celda(+I,+J,+A1,+E,-A2) ← A2 es una matriz que contiene el valor E en la celda (I,J)
%	y en el resto de las celdas contiene los mismos valores que A1.
nuevo_valor_celda(1,J,[C1|R],E,[C2|R]) :- nuevo_valor_fila(J,C1,E,C2).
nuevo_valor_celda(I,J,[C|R1],E,[C|R2]) :- I>1, I1 is I-1, nuevo_valor_celda(I1,J,R1,E,R2).


%	nuevo_valor_fila(+J,+F1,E,-F2) ← F2 es una lista que contiene el valor E en la posicion J
%	y en el resto de las posiciones contiene los mismos valores que F1.
nuevo_valor_fila(1,[_|R],E,[E|R]).
nuevo_valor_fila(J,[C|R1],E,[C|R2]) :- J>1, J1 is J-1, nuevo_valor_fila(J1,R1,E,R2).


%	adyacente(+I,+J,+A,?I2,?J2,?V) ← (I2,J2) son las coordenadas de una celda adyacente a la celda
%	(I,J), ya sea horizontal, vertical o diagonalmente, y el valor de dicha celda V.
adyacente(I1,J1,A,I2,J2,V) :- I2 is I1-1, J2 is J1-1, valor_celda(I2,J2,A,V).
adyacente(I1,J,A,I2,J,V) :- I2 is I1-1, valor_celda(I2,J,A,V).
adyacente(I1,J1,A,I2,J2,V) :- I2 is I1-1, J2 is J1+1, valor_celda(I2,J2,A,V).
adyacente(I,J1,A,I,J2,V) :- J2 is J1-1, valor_celda(I,J2,A,V).
adyacente(I,J1,A,I,J2,V) :- J2 is J1+1, valor_celda(I,J2,A,V).
adyacente(I1,J1,A,I2,J2,V) :- I2 is I1+1, J2 is J1-1, valor_celda(I2,J2,A,V).
adyacente(I1,J,A,I2,J,V) :- I2 is I1+1, valor_celda(I2,J,A,V).
adyacente(I1,J1,A,I2,J2,V) :- I2 is I1+1, J2 is J1+1, valor_celda(I2,J2,A,V).


%   ---------------------------------------------
%	   4 ) Predicados eficientes sobre matrices
%   ---------------------------------------------


%	matriz_f(+M,+N,+E,?A) ← A es una matriz de M filas e N columnas. Cada celda debe tener el valor
%	E. Cada fila se representa mediante un functor 'row' con N argumentos. La matriz se representa
%	mediante un functor 'matrix' con M filas como argumentos.
%	Ej.: matriz_f(2,2,-10,matrix(row(-10,-10),row(-10,-10))).
matriz_f(M,N,E,A) :- functor(A,matrix,M), matriz_f_aux(M,N,E,A).

%	matriz_f_aux(+M,+N,+E,+A) ← Metodo auxiliar para crear la matriz A representada mediante un functor 'matrix' con M elementos
matriz_f_aux(1,N,E,A) :- arg(1,A,R), fila_f(N,E,R).
matriz_f_aux(M,N,E,A) :- M1 is M-1, arg(M,A,R), fila_f(N,E,R), matriz_f_aux(M1,N,E,A).


%	fila_f(+N,+E,?F) ← F es una fila de N elementos de valor E. Cada fila se representa mediante un functor 'row' con N argumentos.
fila_f(N,E,R) :- functor(R,row,N), fila_f_aux(R,N,E).


%	fila_f_aux(+R,+I,+E) ← Se carga el valor E en los primeras I argumentos del functor R.
fila_f_aux(R,1,E) :- arg(1,R,E).
fila_f_aux(R,I,E) :- I1 is I-1, arg(I,R,E), fila_f_aux(R,I1,E).


%	valor_celda_f(+I,+J,+A,?E) ← E es el contenido de la celda (M,N) de la matriz A. A está
%	representada con el formato definido para matriz_f.
valor_celda_f(I,J,A,E) :- arg(I,A,F), arg(J,F,E).


%	nuevo_valor_celda_f(+M,+N,+A,+E) ← Cambia el contenido de la celda (M,N) de la matriz A por el
%	valor E. A está representada en el formato definido para matriz_f.
nuevo_valor_celda_f(M,N,A,E) :- arg(M,A,R), setarg(N,R,E).


%	adyacente_f(+I,+J,+A,?I2,?J2,?V) ← (I2,J2) son las coordenadas de una celda adyacente a la
%	celda (I,J), ya sea horizontal, vertical o diagonalmente, y el valor de dicha celda V
adyacente_f(I1,J1,A,I2,J2,V) :- I2 is I1-1, J2 is J1-1, valor_celda_f(I2,J2,A,V).
adyacente_f(I1,J,A,I2,J,V) :- I2 is I1-1, valor_celda_f(I2,J,A,V).
adyacente_f(I1,J1,A,I2,J2,V) :- I2 is I1-1, J2 is J1+1, valor_celda_f(I2,J2,A,V).
adyacente_f(I,J1,A,I,J2,V) :- J2 is J1-1, valor_celda_f(I,J2,A,V).
adyacente_f(I,J1,A,I,J2,V) :- J2 is J1+1, valor_celda_f(I,J2,A,V).
adyacente_f(I1,J1,A,I2,J2,V) :- I2 is I1+1, J2 is J1-1, valor_celda_f(I2,J2,A,V).
adyacente_f(I1,J,A,I2,J,V) :- I2 is I1+1, valor_celda_f(I2,J,A,V).
adyacente_f(I1,J1,A,I2,J2,V) :- I2 is I1+1, J2 is J1+1, valor_celda_f(I2,J2,A,V).


%   ----------------------------
%	  5) Pruebas de eficiencia
%   ----------------------------

%	Predicados parte 3
test_tiempo(I,J) :- time(test(I,J)).
test(I,J) :- matriz(I,J,0,A1), rellenar_matriz(I,J,A1,A2).

%	rellenar_matriz(+M,+N,+A1,-A2) ← A2 es una matriz de mismas dimensiones que A1 (MxN) pero con el valor i+j en la celda (i,j)
rellenar_matriz(0,_,A,A).
rellenar_matriz(M,N,A1,A3) :- M>0, V is N+M, rellenar_matriz_fila(M,N,A1,A2), M1 is M-1, rellenar_matriz(M1,N,A2,A3).

rellenar_matriz_fila(_,0,A,A).
rellenar_matriz_fila(M,N,A1,A3) :- M>0, V is N+M, nuevo_valor_celda(M,N,A1,V,A2), N1 is N-1, rellenar_matriz_fila(M,N1,A2,A3).

%	Predicados parte 4
test_tiempo_f(I,J) :- time(test_f(I,J)).
test_f(I,J) :- matriz_f(I,J,0,A), rellenar_matriz_f(I,J,A).

%	rellenar_matriz_f(+M,+N,+A1) ← A1 con el valor i+j en cada celda (i,j)
rellenar_matriz_f(0,_,A).
rellenar_matriz_f(M,N,A) :- M>0, V is N+M, rellenar_matriz_fila_f(M,N,A), M1 is M-1, rellenar_matriz_f(M1,N,A).

rellenar_matriz_fila_f(_,0,A).
rellenar_matriz_fila_f(M,N,A) :- M>0, V is N+M, nuevo_valor_celda_f(M,N,A,V), N1 is N-1, rellenar_matriz_fila_f(M,N1,A).


