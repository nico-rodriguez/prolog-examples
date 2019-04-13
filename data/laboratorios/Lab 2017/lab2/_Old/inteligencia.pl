:- module(inteligencia,[nombre/1,hacer_jugada/3]).

:- use_module(juego).

%nombre(?Nombre) <- Nombre es el nombre de la inteligencia, que deberá ser "Grupo XX", siendo XX el número de grupo.
nombre("Grupo 18").


%niveles_minimax(?MaxNiveles) <- MaxNiveles es la cantidad máxima de niveles de Minimax de la inteligencia.
niveles_minimax(2). %ver hasta que numero funciona


%hacer_jugada(+E,?LP,?E2) <- E2 es el estado resultante de mover según la lista de posiciones LP de un movimiento que la inteligencia elige jugar para el estado E. El turno de jugar en el estado E es el de la inteligencia, mientras que en E2 es del otro jugador.
hacer_jugada(E,LP,E2):-
	niveles_minimax(Niv),
	turno(E,J),
	findall(M,mover(E,M,_),Movs),
	evaluar_y_elegir(Movs,E,Niv,-1000,1000,MejorAux,MejorMov,J),
	arg(1,MejorMov,LP),
    mover(E,LP,E2). % TODO



/* FUNCIONES AUXILIARES */
	
%evaluar_tablero(+E,+J,?V) <- V es el valor para el tablero en estado E, para el jugagador J.
evaluar_tablero(E,J,V) :- gol(E,J), V is 100 . %gol tuyo
evaluar_tablero(E,J,V) :- gol(E,J2), J2 =\= J, V is -100 . %gol del otro
evaluar_tablero(E,_,V) :- sin_movimientos(E), V is -100 . %si no hay movimientos
evaluar_tablero(E,J,V) :- not(gol(E,_)), posicion_actual(E,P), posicion_arco_gol(J,Fg,Cg), distancia_gol(P,Fg,Cg,V). %se valora con la distancia


%posicion_arco_gol(+J,?Fg,?Cg) <- Fg y Cg es la fila y la columna del centro de la linea de gol del arco que el jugador J debe hacer gol.
posicion_arco_gol(J,Fg,Cg) :- J is 1, cantidad_casilleros(Ancho,_), Fg is 1, Cg is (div(Ancho,2)+1).
posicion_arco_gol(J,Fg,Cg) :- J is 2, cantidad_casilleros(Ancho,Alto), Fg is Alto + 3, Cg is (div(Ancho,2)+1).


%distancia_gol(+P,+Fg,+Cg,?D) <- D es la distancia en cantidad de aristas entre P la posicion actual y la posicion del arco para hacer gol + 1 .
distancia_gol(P,Fg,Cg,D) :- arg(1,P,F1), arg(2,P,C1), D is round(sqrt((Fg - F1)**2 + (Cg - C1)**2)).


%sin_movimientos(+E) <- indica si para el estado E no hay movimientos posibles.
sin_movimientos(E) :- 
    not(gol(E,_)), 
    posicion_actual(E,P), obtener_vertice(E,P,V), arg(1,P,F), arg(2,P,C), 
    es_cancha(F,C), arg(1,V,1), arg(2,V,L), largo_lista(L,N), N > 1, N is 8.
sin_movimientos(E) :- 
    not(gol(E,_)),
    posicion_actual(E,P),obtener_vertice(E,P,V),arg(1,P,F), arg(2,P,C), 
    not(es_esquina(F,C)), es_borde(F,C), arg(2,V,L),largo_lista(L,N), N is 3.
sin_movimientos(E) :- 
    posicion_actual(E,P),obtener_vertice(E,P,_),arg(1,P,F), arg(2,P,C), 
    es_esquina(F,C).



/* SACADO DE LIBRO */
%evaluar_y_elegir(Movs,E,Niv,Alpha,Beta,MejorAux,MejorMov,J) <- 
	%Elige MejorMov de la lista de Movs a partir del estado E usando minimax con alpha-beta. 
	%MejorAux mantiene el mejor movimiento actual, Niv indica la profundidad, J indica el jugador.


evaluar_y_elegir([Mov|Movs],E,Niv,Alpha,Beta,MejorAux,MejorMov,J) :- 
	mover(E,Mov,E1), 
	alpha_beta(Niv,E1,Alpha,Beta,MovX,Val,J),
	Val1 is -Val,
	cutoff(Mov,Val1,Niv,Alpha,Beta,Movs,E,MejorAux,MejorMov,J).
evaluar_y_elegir([],E,Niv,Alpha,Beta,Mov,(Mov,Alpha),J).


alpha_beta(0,E,Alpha,Beta,Mov,Val,J) :- 
	evaluar_tablero(E,J,Val).
alpha_beta(Niv,E,Alpha,Beta,Mov,Val,J) :- 
	findall(M,mover(E,M,_),Movs),
	Alpha1 is -Beta,
	Beta1 is -Alpha,
	Niv1 is Niv-1,
	evaluar_y_elegir(Movs,E,Niv1,Alpha1,Beta1,nil,(Mov,Val),J).


cutoff(Mov,Val,Niv,Alpha,Beta,Movs,E,MejorAux,(Mov,Val),J) :- 
	Val >= Beta.
cutoff(Mov,Val,Niv,Alpha,Beta,Movs,E,MejorAux,MejorMov,J) :- 
	Alpha < Val, 
	Val < Beta,
	evaluar_y_elegir(Movs,E,Niv,Val,Beta,Mov,MejorMov,J).
cutoff(Mov,Val,Niv,Alpha,Beta,Movs,E,MejorAux,MejorMov,J) :- 
	Val =< Alpha,
	evaluar_y_elegir(Movs,E,Niv,Alpha,Beta,MejorAux,MejorMov,J).
	
	
/*
USADO EN LAS PRUEBAS ONLINE

%hacer_jugada(+E,?LP,?E2) <- E2 es el estado resultante de mover según la lista de posiciones LP de un movimiento que la inteligencia elige jugar para el estado E. El turno de jugar en el estado E es el de la inteligencia, mientras que en E2 es del otro jugador.
hacer_jugada(E,LP,E2):-
	niveles_minimax(Niv),
	turno(E,J),
	%findall(M,mover(E,M,_),Movs),
	evaluar_y_elegir(Movs,E,Niv,-20,20,_,MejorMov,J),
	arg(1,MejorMov,LP),
    mover(E,LP,E2). % TODO


	
%evaluar_tablero(+E,+J,?V) <- V es el valor para el tablero en estado E, para el jugagador J.
evaluar_tablero(E,J,V) :- gol(E,J), V is 100 . %gol tuyo
evaluar_tablero(E,J,V) :- gol(E,J2), J2 =\= J, V is -100 . %gol del otro
evaluar_tablero(E,_,V) :- sin_movimientos(E), V is -100 . %si no hay movimientos
evaluar_tablero(E,J,V) :- not(gol(E,_)), posicion_actual(E,P), posicion_arco_gol(J,Fg,Cg), distancia_gol(P,Fg,Cg,V). %se valora con la distancia


%posicion_arco_gol(+J,?Fg,?Cg) <- Fg y Cg es la fila y la columna del centro de la linea de gol del arco que el jugador J debe hacer gol.
posicion_arco_gol(J,Fg,Cg) :- J is 1, cantidad_casilleros(Ancho,_), Fg is 1, Cg is (div(Ancho,2)+1).
posicion_arco_gol(J,Fg,Cg) :- J is 2, cantidad_casilleros(Ancho,Alto), Fg is Alto + 3, Cg is (div(Ancho,2)+1).


%distancia_gol(+P,+Fg,+Cg,?D) <- D es la distancia en cantidad de aristas entre P la posicion actual y la posicion del arco para hacer gol + 1 .
distancia_gol(P,Fg,Cg,D) :- arg(1,P,F1), arg(2,P,C1), D is round(sqrt((Fg - F1)**2 + (Cg - C1)**2)).


%sin_movimientos(+E) <- indica si para el estado E no hay movimientos posibles.
sin_movimientos(E) :- 
    not(gol(E,_)), 
    posicion_actual(E,P), obtener_vertice(E,P,V), arg(1,P,F), arg(2,P,C), 
    es_cancha(F,C), arg(1,V,1), arg(2,V,L), largo_lista(L,N), N > 1, N is 8.
sin_movimientos(E) :- 
    not(gol(E,_)),
    posicion_actual(E,P),obtener_vertice(E,P,V),arg(1,P,F), arg(2,P,C), 
    not(es_esquina(F,C)), es_borde(F,C), arg(2,V,L),largo_lista(L,N), N is 3.
sin_movimientos(E) :- 
    posicion_actual(E,P),obtener_vertice(E,P,_),arg(1,P,F), arg(2,P,C), 
    es_esquina(F,C).



evaluar_y_elegir([Mov|Movs],E,Niv,Alpha,Beta,MejorAux,MejorMov,J) :- 
	mover(E,Mov,E1), 
	alpha_beta(Niv,E1,Alpha,Beta,_,Val,J),
	Val1 is -Val,
	cutoff(Mov,Val1,Niv,Alpha,Beta,Movs,E,MejorAux,MejorMov,J).
evaluar_y_elegir([],_,_,Alpha,_,Mov,(Mov,Alpha),_) :- 
    writeln('cometravas').

alpha_beta(0,E,_,_,_,Val,J) :- 
    writeln('alpha_beta - hoja'),
    evaluar_tablero(E,J,Val).
alpha_beta(Niv,E,Alpha,Beta,Mov,Val,J) :- 
	findall(M,mover(E,M,_),Movs),
	Alpha1 is -Beta,
	Beta1 is -Alpha,
	Niv1 is Niv-1,
	evaluar_y_elegir(Movs,E,Niv1,Alpha1,Beta1,nil,(Mov,Val),J).


cutoff(Mov,Val,_,_,Beta,_,_,_,(Mov,Val),_) :- 
    writeln('cutoff - val >= beta'),
    Val >= Beta.
cutoff(Mov,Val,Niv,Alpha,Beta,Movs,E,_,MejorMov,J) :- 
    writeln('cutoff - alpha < val'),
	Alpha < Val, 
	Val < Beta,
	evaluar_y_elegir(Movs,E,Niv,Val,Beta,Mov,MejorMov,J).
cutoff(_,Val,Niv,Alpha,Beta,Movs,E,MejorAux,MejorMov,J) :- 
    writeln('cutoff - val <= alpha'),
    write('val:'),
    write(Val),
    write('alpha:'),
    writeln(Alpha),
	Val =< Alpha,
	evaluar_y_elegir(Movs,E,Niv,Alpha,Beta,MejorAux,MejorMov,J).



*/
