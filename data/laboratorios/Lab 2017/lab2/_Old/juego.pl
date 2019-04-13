:- module(juego,[cantidad_casilleros/2,estado_inicial/1,posicion_pelota/2,mover/3,prefijo_movimiento/2,gol/2,turno/2]).

%cantidad_casilleros(?Ancho,?Alto) <- Ancho es la cantidad de casilleros que hay de ancho en el tablero y Alto es la cantidad que hay de alto (sin contar los arcos). Ambos números son pares. Notar que la cantidad de vértices es uno más que la cantidad de casilleros. Este predicado está dado, no hay que modificarlo.
cantidad_casilleros(8,10).

/*
	estado_inicial(?E) <- E es el estado inicial del juego, de tamaño AltoxAncho unificado por cantidad_casilleros(Ancho,Alto).
	La pelota comienza en la posición p(0,0), que es la posición central del tablero. El turno inicial es del jugador 1, que es el que patea hacia arriba.
	E es un functor 'estado' que tiene 3 argumentos.
	1 -> Es un functor 'tablero'
	2 -> Es la posicion actual de la pelota en el tablero
	3 -> Es el jugador que tiene que mover en el siguiente turno
*/
estado_inicial(E):- cantidad_casilleros(Ancho,Alto), AltoAux is Alto+3, AnchoAux is Ancho+1, tablero_incial(AltoAux,AnchoAux,M), functor(E,estado,3),
					arg(1,E,M), Y0 is div(Alto,2) + 2, X0 is div(Ancho,2) + 1, arg(2,E,(Y0,X0)), arg(3,E,1).

/*
	posicion_pelota(+E,?P) <- P es la posición de la pelota para el estado E.
*/
posicion_pelota(E,P) :- arg(2,E,H), arg(1,H,F), arg(2,H,C), coordenadasMatrizToWeb(F,C,P).

/*
	mover(+E,?LP,?E2) <- E2 el estado resultante de hacer un movimiento con la pelota, a través de las posiciones de la lista LP en el estado E y de cambiar el turno.
*/
mover(E,LP,E2) :- mover_rec(E,LP,E2), cambiar_turno(E2).

/*
	gol(+E,?NJugador) <- La pelota está en situación de gol a favor del jugador NJugador para el estado E.
*/
gol(E,J):- posicion_pelota(E,P), cantidad_casilleros(_,Alto),X is -(div(Alto,2)+1), P = p(-1,X), J=2.
gol(E,J):- posicion_pelota(E,P), cantidad_casilleros(_,Alto),X is -(div(Alto,2)+1), P = p(0,X), J=2.
gol(E,J):- posicion_pelota(E,P), cantidad_casilleros(_,Alto),X is -(div(Alto,2)+1), P = p(1,X), J=2.
gol(E,J):- posicion_pelota(E,P), cantidad_casilleros(_,Alto),X is (div(Alto,2)+1), P = p(-1,X), J=1.
gol(E,J):- posicion_pelota(E,P), cantidad_casilleros(_,Alto),X is (div(Alto,2)+1), P = p(0,X), J=1.
gol(E,J):- posicion_pelota(E,P), cantidad_casilleros(_,Alto),X is (div(Alto,2)+1), P = p(1,X), J=1.

/*
	turno(+E,?NJugador) <- NJugador es el jugador que tiene que mover en el siguiente turno para el estado E.
*/
turno(E,J) :- arg(3,E,J).

/*
	prefijo_movimiento(+E,+LP) <- LP es una lista no vacía de posiciones que constituyen el prefijo de un movimiento para el estado E, sin llegar a formar un movimiento.
*/
prefijo_movimiento(E,LP) :- copia_estado(Copia,E), prefijo_movimiento_rec(Copia,LP).



/*
	------------------
	METODOS AUXILIARES
	------------------
*/

/*
	tablero_incial(+F,+C,?M). M es una matriz de F filas y C columnas. 
	La matriz se representa mediante un functor 'tablero' con F filas como argumentos.
	Cada fila se representa mediante un functor 'row' con C vertices como argumentos. 
	Cada vertice se representa con un functor 'vertice' con 2 argumentos.
	1-> Estado
			 1: vertice visitado
			 2: vertice no visitado
			-1:	fuera de cancha
	2 -> Lista de posiciones adyacentes cuya arista que los comunica fue utilizada
*/
tablero_incial(F,C,M) :- functor(M,tablero,F), crear_filas(M,F,C).

/*
	crear_filas(+M,+F,+C). <- Se crean los functores 'row' de la Matriz M, que tiene F cantidad de filas, C cantidad de columnas.
*/
crear_filas(_,0,_).
crear_filas(M,F,C) :- F > 0, arg(F,M,Row), functor(Row,row,C), cargar_dato(Row,C,F), F1 is F - 1, crear_filas(M,F1,C).

/*
	cargar_dato(+Row,+C,+F). <- Se carga en el functor row 'Row', los C argumentos que tiene el estado que corresponda((1) visitado \ (0) no visitado\ (-1)fuera de cancha).
*/
cargar_dato(_,0,_).
cargar_dato(Row,C,F) :- es_afuera(F,C),functor(V,vertice,2),arg(1,V,-1),arg(2,V,[]),C1 is C - 1 ,arg(C,Row,V),cargar_dato(Row,C1,F).
cargar_dato(Row,C,F) :- es_cancha(F,C),F is 7, C is 5,functor(V,vertice,2),arg(1,V,1),arg(2,V,[]), C1 is C - 1 ,arg(C,Row,V),cargar_dato(Row,C1,F).
cargar_dato(Row,C,F) :- es_cancha(F,C),F is 7, C =\= 5,functor(V,vertice,2),arg(1,V,0),arg(2,V,[]), C1 is C - 1 ,arg(C,Row,V),cargar_dato(Row,C1,F).
cargar_dato(Row,C,F) :- es_cancha(F,C),F =\= 7,functor(V,vertice,2),arg(1,V,0),arg(2,V,[]), C1 is C - 1 ,arg(C,Row,V),cargar_dato(Row,C1,F).
cargar_dato(Row,C,F) :-es_borde(F,C), functor(V,vertice,2),arg(1,V,1),arg(2,V,[]), C1 is C - 1 ,arg(C,Row,V) ,cargar_dato(Row,C1,F).

/*
	es_borde(?F,?C) <- Indica si las  posicion (F,C) es borde.
*/
es_borde(F,C) :- cantidad_casilleros(_,Alto), C is 1, F > 2, F < Alto + 2. %linea izquierda
es_borde(F,C) :- cantidad_casilleros(Ancho,Alto), C is Ancho + 1, F > 2, F < Alto + 2. %linea derecha
es_borde(F,C) :- cantidad_casilleros(Ancho,_), F is 2, C > 0, C < Ancho + 2, P is (div(Ancho,2) + 1), C=\= P. %linea arriba sin esquinas
es_borde(F,C) :- cantidad_casilleros(Ancho,Alto), F is Alto + 2, C > 0, C < Ancho + 2, P is (div(Ancho,2) + 1), C=\= P. %linea abajo sin esquinas
es_borde(F,C) :- cantidad_casilleros(Ancho,_), F is 1, C >= (div(Ancho,2)) , C =< (div(Ancho,2) + 2). %linea gol arriba
es_borde(F,C) :- cantidad_casilleros(Ancho,Alto), F is Alto + 3, C >= (div(Ancho,2)) , C =< (div(Ancho,2) + 2). %linea gol abajo

/*
	es_borde(?F,?C) <- Indica si las  posicion (F,C) es cancha.
*/
es_cancha(F,C) :- cantidad_casilleros(Ancho,Alto), C > 1, C < Ancho + 1, F > 2, F < Alto + 2. %medio de la cancha
es_cancha(F,C) :- cantidad_casilleros(Ancho,_), F is 2, C is (div(Ancho,2) + 1). %medio linea arco arriba
es_cancha(F,C) :- cantidad_casilleros(Ancho,Alto), F is Alto + 2, C is (div(Ancho,2) + 1). %medio linea arco abajo

/*
	es_borde(?F,?C) <- Indica si las  posicion (F,C) es fuera de cancha.
*/
es_afuera(F,C) :- not(es_cancha(F,C)), not(es_borde(F,C)). %afuera de la cancha

/*
	es_esquina(?F,?C) <- Indica si las  posicion (F,C) es fuera una esquina de la cancha.
*/
es_esquina(F,C) :- F is 2, C is 1.
es_esquina(F,C) :- cantidad_casilleros(Ancho,_), F is 2, C is Ancho + 1.
es_esquina(F,C) :- cantidad_casilleros(_,Alto), F is Alto + 2, C is 1.
es_esquina(F,C) :- cantidad_casilleros(Ancho,Alto), F is Alto + 2, C is Ancho + 1.

/*
	coordenadasWebToMatriz(+P,?F,?C) <- Traduce coordenadas web de la forma p(X,Y) a coordenadas matriz de la forma F(fila) y C (Columna).
*/
coordenadasWebToMatriz(P,F,C) :- obtenerX(P,X), obtenerY(P,Y), cantidad_casilleros(Ancho,Alto), F is (-Y+div(Alto,2)+2), C is (X+div(Ancho,2)+1).

/*
	coordenadasMatrizToWeb(+F,+C,?P) <- Traduce coordenadas matriz de la forma F(fila) y C (Columna) a coordenadas web de la forma p(X,Y).
*/
coordenadasMatrizToWeb(F,C,P) :- cantidad_casilleros(Ancho,Alto), X is (C-(div(Ancho,2)+1)), Y is (-F+div(Alto,2)+2), P = p(X,Y).

/*
	obtenerX(+P,?X) <- Devuelve el valor de X de una posicion en coordenadas web de la forma p(X,Y).
*/
obtenerX(P,X):- arg(1,P,X).

/*
	obtenerY(+P,?Y) <- Devuelve el valor de Y de una posicion en coordenadas web de la forma p(X,Y).
*/
obtenerY(P,Y):- arg(2,P,Y).


mover_rec(E1,[H|[]],E2) :- movimiento_valido_mover(E1,H), realizar_movimiento_dos(E1,H,E2), not(hay_movimiento_siguiente(E2)).
mover_rec(E1,[H|T],E3) :- movimiento_valido_mover(E1,H), realizar_movimiento_dos(E1,H,E2),hay_movimiento_siguiente(E2), mover_rec(E2,T,E3).
	

cambiar_turno(E) :- turno(E,J), J is 1, setarg(3,E,2).
cambiar_turno(E) :- turno(E,J), J is 2, setarg(3,E,1).

obtener_tablero(E,T) :- arg(1,E,T).

nuevo_valor_celda(F, C, M, X) :-arg(F, M, Fila), arg(C, Fila, V), setarg(1,V,X).
agregar_adyacente_vertice(F, C, M, P) :-arg(F, M, Fila), arg(C, Fila, V), arg(2,V,L), L1 = [P|L], setarg(2,V,L1).

realizar_movimiento(E,PN) :- coordenadasWebToMatriz(PN,F,C), obtener_tablero(E,T), 
nuevo_valor_celda(F,C,T,1), posicion_actual(E,PA), agregar_adyacente_vertice(F, C, T, PA), arg(1,PA,F1),arg(2,PA,C1),agregar_adyacente_vertice(F1, C1, T, (F,C)),setarg(2,E,(F,C)).


%Es igual al otro, solo que en vez de realizar el movimiento sibre el mismo E lo copia y realiza el movimiento sobre la copia
realizar_movimiento_dos(E1,PN, E2) :- coordenadasWebToMatriz(PN,F,C), copia_estado(E2,E1), obtener_tablero(E2,T), 
nuevo_valor_celda(F,C,T,1), posicion_actual(E2,PA), agregar_adyacente_vertice(F, C, T, PA), arg(1,PA,F1),arg(2,PA,C1),agregar_adyacente_vertice(F1, C1, T, (F,C)),setarg(2,E2,(F,C)).




copia_estado(C,E):-functor(C,estado,3), arg(1,E,M), arg(2,E,P), arg(3,E,J),arg(1,C,M), arg(2,C,P), arg(3,C,J).

%prefijo_movimiento(+E,+LP) <- LP es una lista no vacía de posiciones que constituyen el prefijo de un movimiento para el estado E, sin llegar a formar un movimiento.


prefijo_movimiento_rec(E,[H|[]]) :- movimiento_valido(E,H),realizar_movimiento(E,H),hay_movimiento_siguiente(E).
prefijo_movimiento_rec(E,[H|T]) :- movimiento_valido(E,H), realizar_movimiento(E,H),prefijo_movimiento_rec(E,T).


% TODO - probar a fondo
% ESTA MAL HECHOOOOOOOOOOOOOOOOOOOO
% Pruebo agregar condicion N > 1
hay_movimiento_siguiente(E) :- not(gol(E,_)), posicion_actual(E,P),obtener_vertice(E,P,V),arg(1,P,F), arg(2,P,C), es_cancha(F,C), arg(1,V,1), arg(2,V,L),largo_lista(L,N), N > 1,N < 8.
hay_movimiento_siguiente(E) :- not(gol(E,_)),posicion_actual(E,P),obtener_vertice(E,P,V),arg(1,P,F), arg(2,P,C), not(es_esquina(F,C)), es_borde(F,C), arg(2,V,L),largo_lista(L,N), N < 3.

%valor_celda_f(+I,+J,+M,?E) Devuelve en E el contenido de la celda I,J
valor_celda_f(I, J, M, E) :- arg(I, M, Fila), arg(J, Fila, E). %Agarro la fila y luego el elemento dentro de la fila.

obtener_vertice(E,P,V):- obtener_tablero(E,M), arg(1,P,F),arg(2,P,C) ,arg(F, M, Fila),arg(C,Fila,V).


%me viene un H = p(X,Y) de coordenadas web, la copia del E global y veo a ver si es valido	en nuestra matriz
%Obtengo posicion actual en las coordenadas web
%Pregunto si H es adyacente a la posicion actual de la matriz
%Obtengo las coordenadas de H (que si llega hasta aca es igual a P) a coordenadas de matriz 
%movimiento_valido(E,H) :- posicion_actual(E,P),adyacente(P,X1,Y1), obtenerX(H,X1), obtenerY(H,Y1),coordenadasWebToMatriz(H,F,C),posicion_actual(E,PActual), not(arista_visitada(PActual,F,C)).

movimiento_valido(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), es_cancha(F1,C1), obtenerX(H,X1), obtenerY(H,Y1),adyacente(P,X1,Y1),coordenadasWebToMatriz(H,F,C), obtener_vertice(E,P,V),not(arista_visitada(V,F,C)).
movimiento_valido(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), es_borde(F1,C1), obtenerX(H,X1), obtenerY(H,Y1),adyacente(P,X1,Y1),coordenadasWebToMatriz(H,F,C), not(es_borde(F,C)), obtener_vertice(E,P,V),not(arista_visitada(V,F,C)).
movimiento_valido(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), coordenadasWebToMatriz(H,F,C), obtener_vertice(E,P,V), not(arista_visitada(V,F,C)), es_arista_borde_esquina(F1,C1,F,C).

es_arista_borde_esquina(F1,C1,F2,C2) :- F1 is 2, C1 is 2, F2 is 3, C2 is 1.
es_arista_borde_esquina(F2,C2,F1,C1) :- F1 is 2, C1 is 2, F2 is 3, C2 is 1.
es_arista_borde_esquina(F1,C1,F2,C2) :- cantidad_casilleros(Ancho,_), F1 is 2, C1 is Ancho, F2 is 3, C2 is Ancho + 1.
es_arista_borde_esquina(F2,C2,F1,C1) :- cantidad_casilleros(Ancho,_), F1 is 2, C1 is Ancho, F2 is 3, C2 is Ancho + 1.
es_arista_borde_esquina(F1,C1,F2,C2) :- cantidad_casilleros(_,Alto), F1 is Alto + 1 , C1 is 1, F2 is Alto + 2, C2 is 2.
es_arista_borde_esquina(F2,C2,F1,C1) :- cantidad_casilleros(_,Alto), F1 is Alto + 1 , C1 is 1, F2 is Alto + 2, C2 is 2.
es_arista_borde_esquina(F1,C1,F2,C2) :- cantidad_casilleros(Ancho,Alto), F1 is Alto + 1 , C1 is Ancho + 1, F2 is Alto + 2, C2 is Ancho.
es_arista_borde_esquina(F2,C2,F1,C1) :- cantidad_casilleros(Ancho,Alto), F1 is Alto + 1 , C1 is Ancho + 1, F2 is Alto + 2, C2 is Ancho.

movimiento_valido_mover(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), es_cancha(F1,C1), adyacente(P,X,Y), H = p(X,Y), coordenadasWebToMatriz(H,F,C), obtener_vertice(E,P,V), not(arista_visitada(V,F,C)).
movimiento_valido_mover(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), es_borde(F1,C1), adyacente(P,X,Y), H = p(X,Y), coordenadasWebToMatriz(H,F,C), not(es_borde(F,C)), obtener_vertice(E,P,V),not(arista_visitada(V,F,C)).
movimiento_valido_mover(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), adyacente(P,X,Y), H = p(X,Y), coordenadasWebToMatriz(H,F,C), obtener_vertice(E,P,V), not(arista_visitada(V,F,C)), es_arista_borde_esquina(F1,C1,F,C).
movimiento_valido_mover(E,H) :- posicion_actual(E,P), arg(1,P,F1), arg(2,P,C1), es_vertice_linea_gol(F1,C1), adyacente(P,X,Y), H = p(X,Y), coordenadasWebToMatriz(H,F,C), es_gol_mitad(F,C), obtener_vertice(E,P,V),not(arista_visitada(V,F,C)).

es_vertice_linea_gol(F,C) :- cantidad_casilleros(Ancho,_), F is 2, C is (div(Ancho,2)).
es_vertice_linea_gol(F,C) :- cantidad_casilleros(Ancho,_), F is 2, C is (div(Ancho,2)+2).
es_vertice_linea_gol(F,C) :- cantidad_casilleros(Ancho,Alto), F is Alto + 2, C is (div(Ancho,2)).
es_vertice_linea_gol(F,C) :- cantidad_casilleros(Ancho,Alto), F is Alto + 2, C is (div(Ancho,2)+2).

es_gol_mitad(F,C) :- cantidad_casilleros(Ancho,_), F is 1, C is (div(Ancho,2)+1).
es_gol_mitad(F,C) :- cantidad_casilleros(Ancho,Alto), F  is Alto + 3, C is (div(Ancho,2)+1).
    
%devuelve true si F,C estan en la lista de aristas de la posicion PActual.
arista_visitada(VActual,F,C):- arg(2,VActual,L), member((F,C),L).


%Posicion actual en coordenadas matriz
posicion_actual(E,P) :- arg(2,E,P).

%TODO LO RELACIONADO A ADYACENTE ESTA CON LAS COORDENADAS WEB
%por eso paso el P a coordenadas web.
%recibe un P = (F,C) y me devuelve un X1,Y1 que son coordenadas del adyacente de P en el sistema de coordenadas web
%adyacente((7,5),1,0) FORMA DE LLAMAR A ADYACENTE QUE ME DA TRUE
adyacente(P,X1,Y1):- cantidad_casilleros(Ancho,Alto),obtenerX(P,F), obtenerY(P,C),coordenadasMatrizToWeb(F,C,Pos), obtenerX(Pos,X), obtenerY(Pos,Y),adyacente_aux(X,Y,Alto,Ancho,X1,Y1).

adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X + 1,Y1 is Y + 1,pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X ,Y1 is Y + 1 ,pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X -1,Y1 is Y +1, pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X +1,Y1 is Y , pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X -1,Y1 is Y , pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X +1,Y1 is Y -1, pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X ,Y1 is Y -1, pertenece(X1,Y1,Alto,Ancho).
adyacente_aux(X,Y,Alto,Ancho,X1,Y1):- X1 is X -1,Y1 is Y -1, pertenece(X1,Y1,Alto,Ancho).

pertenece(0,Y,Alto,_):- J is (div(Alto,2)+1), Y >= -J , Y =< J.
pertenece(-1,Y,Alto,_):- J is (div(Alto,2)+1), Y >= -J, Y =< J.
pertenece(1,Y,Alto,_):- J is (div(Alto,2)+1), Y >= -J, Y =< J.
pertenece(X,Y,Alto,Ancho):- X > 1,J is (div(Alto,2)), Y >= -J , Y =< J, K is (div(Ancho,2)), X >= -K , X =< K.
pertenece(X,Y,Alto,Ancho):- X < -1,J is (div(Alto,2)), Y >= -J , Y =< J, K is (div(Ancho,2)), X >= -K , X =< K.

%TODO LO RELACIONADO A ADYACENTE ESTA CON LAS COORDENADAS WEB

/*
	largo_lista(+L,?N), siendo N el largo de la lista L.
*/
largo_lista([],0).
largo_lista([_|T],X) :- largo_lista(T,X1), X is X1+1.