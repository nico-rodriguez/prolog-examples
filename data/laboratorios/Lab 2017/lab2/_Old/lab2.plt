% Para correr a mano los tests:
%
%   ?- [lab2].
%   ?- load_files('lab2.plt').
%   ?- run_tests.
%
% Preferir load_files para cargar los tests porque recarga el archivo si ya estaba cargado. Para ver el coverage:
%
%   ?- show_coverage(run_tests).
%

:- begin_tests(lab2).
%:- use_module(lab2). % No es necesario (y da error con el script), ya que load_files no lo precisa. load_test_files(X) sí lo precisaría.

% ------------------------------------------------------------------------------------------------------------------------------------------------------------
%gol(+E,?NJugador)

test(gol_1,nondet):-
    call_with_time_limit(10,(
        gol([(p(-1,6),1)],1))).

test(gol_2,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(-1,6),1)],2)))).
	
test(gol_3,nondet):-
    call_with_time_limit(10,(
        gol([(p(0,6),1)],1))).

test(gol_4,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(0,6),1)],2)))).

test(gol_5,nondet):-
    call_with_time_limit(10,(
        gol([(p(1,6),1)],1))).

test(gol_6,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(1,6),1)],2)))).
		
test(gol_7,nondet):-
    call_with_time_limit(10,(
        gol([(p(-1,-6),1)],2))).

test(gol_8,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(-1,-6),1)],1)))).
	
test(gol_9,nondet):-
    call_with_time_limit(10,(
        gol([(p(0,-6),1)],2))).

test(gol_10,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(0,-6),1)],1)))).

test(gol_11,nondet):-
    call_with_time_limit(10,(
        gol([(p(1,-6),1)],2))).

test(gol_12,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(1,-6),1)],1)))).

test(gol_13,all(J == [1])):-
    call_with_time_limit(10,(
        gol([(p(-1,6),1)],J))).
		
test(gol_14,all(J == [1])):-
    call_with_time_limit(10,(
        gol([(p(0,6),1)],J))).
		
test(gol_15,all(J == [1])):-
    call_with_time_limit(10,(
        gol([(p(1,6),1)],J))).
		
test(gol_16,all(J == [2])):-
    call_with_time_limit(10,(
        gol([(p(-1,-6),1)],J))).
		
test(gol_17,all(J == [2])):-
    call_with_time_limit(10,(
        gol([(p(0,-6),1)],J))).
		
test(gol_18,all(J == [2])):-
    call_with_time_limit(10,(
        gol([(p(1,-6),1)],J))).
		
test(gol_19,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(0,0),_)],1)))).
		
test(gol_20,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(0,5),_)],1)))).
		
test(gol_21,nondet):-
    call_with_time_limit(10,(
        not(gol([(p(0,-5),_)],1)))).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------
%estado_inicial(?E)		

test(estado_inicial_1,nondet):-
    call_with_time_limit(10,(
        estado_inicial([(p(0,0),2)]))).

test(estado_inicial_2,nondet):-
    call_with_time_limit(10,(
        not(estado_inicial([])))).
		
test(estado_inicial_2,nondet):-
    call_with_time_limit(10,(
        not(estado_inicial([(p(0,0),1)])))).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------
%posicion_pelota(+E,?P) 

test(posicion_pelota_1,nondet):-
    call_with_time_limit(10,(
        posicion_pelota([(p(0,0),2)],p(0,0)))).
		
test(posicion_pelota_2,all(P == [p(0,0)])):-
    call_with_time_limit(10,(
        posicion_pelota([(p(0,0),2)],P))).

test(posicion_pelota_3,nondet):-
    call_with_time_limit(10,(
        posicion_pelota([(p(0,1),1),(p(0,0),2)],p(0,1)))).
		
test(posicion_pelota_4,all(P == [p(0,1)])):-
    call_with_time_limit(10,(
        posicion_pelota([(p(0,1),1),(p(0,0),2)],P))).
		
test(posicion_pelota_5,nondet):-
    call_with_time_limit(10,(
        not(posicion_pelota([],_)))).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------		
%turno(+E,?NJugador)

test(turno_1,nondet):-
    call_with_time_limit(10,(
        turno([(p(0,0),2)],1))).
		
test(turno_2,all(J == [1])):-
    call_with_time_limit(10,(
        turno([(p(0,0),2)],J))).

test(turno_3,nondet):-
    call_with_time_limit(10,(
        turno([(p(0,1),1),(p(0,0),2)],2))).
		
test(turno_4,all(J == [2])):-
    call_with_time_limit(10,(
        turno([(p(0,1),1),(p(0,0),2)],J))).
		
test(turno_5,nondet):-
    call_with_time_limit(10,(
        not(turno([],_)))).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------		
% ------------------------------------------------------------------------------------------------------------------------------------------------------------		
% AUXILIARES
% ------------------------------------------------------------------------------------------------------------------------------------------------------------		
% ------------------------------------------------------------------------------------------------------------------------------------------------------------		

test(obtenerX_1,all(X == [1])):-
    call_with_time_limit(10,(
        obtenerX(p(1,5),X))).
		
test(obtenerX_2,nondet):-
    call_with_time_limit(10,(
        obtenerX(p(1,5),1))).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------		
		
test(obtenerY_1,all(Y == [5])):-
    call_with_time_limit(10,(
        obtenerY(p(1,5),Y))).
		
test(obtenerY_2,nondet):-
    call_with_time_limit(10,(
        obtenerY(p(1,5),5))).

% ------------------------------------------------------------------------------------------------------------------------------------------------------------		

test(pertenece_1,nondet):-
    call_with_time_limit(10,(
        pertenece(-4,5,10,8))).
		
test(pertenece_2,nondet):-
    call_with_time_limit(10,(
        pertenece(4,5,10,8))).
		
test(pertenece_3,nondet):-
    call_with_time_limit(10,(
        pertenece(-4,-5,10,8))).
		
test(pertenece_4,nondet):-
    call_with_time_limit(10,(
        pertenece(-4,5,10,8))).
		
test(pertenece_5,nondet):-
    call_with_time_limit(10,(
        pertenece(-1,5,10,8))).
		
test(pertenece_6,nondet):-
    call_with_time_limit(10,(
        pertenece(0,5,10,8))).
		
test(pertenece_7,nondet):-
    call_with_time_limit(10,(
        pertenece(1,5,10,8))).
		
test(pertenece_8,nondet):-
    call_with_time_limit(10,(
        pertenece(-1,6,10,8))).
		
test(pertenece_9,nondet):-
    call_with_time_limit(10,(
        pertenece(0,6,10,8))).
		
test(pertenece_10,nondet):-
    call_with_time_limit(10,(
        pertenece(1,6,10,8))).

test(pertenece_11,nondet):-
    call_with_time_limit(10,(
        pertenece(-1,-5,10,8))).
		
test(pertenece_12,nondet):-
    call_with_time_limit(10,(
        pertenece(0,-5,10,8))).
		
test(pertenece_13,nondet):-
    call_with_time_limit(10,(
        pertenece(1,-5,10,8))).
		
test(pertenece_14,nondet):-
    call_with_time_limit(10,(
        pertenece(-1,-6,10,8))).
		
test(pertenece_15,nondet):-
    call_with_time_limit(10,(
        pertenece(0,-6,10,8))).
		
test(pertenece_16,nondet):-
    call_with_time_limit(10,(
        pertenece(1,-6,10,8))).
		
test(pertenece_17,nondet):-
    call_with_time_limit(10,(
        not(pertenece(4,6,10,8)))).

test(pertenece_18,nondet):-
    call_with_time_limit(10,(
        not(pertenece(2,6,10,8)))).

test(pertenece_19,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-4,6,10,8)))).

test(pertenece_20,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-2,6,10,8)))).

test(pertenece_21,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-1,7,10,8)))).

test(pertenece_22,nondet):-
    call_with_time_limit(10,(
        not(pertenece(0,7,10,8)))).

test(pertenece_23,nondet):-
    call_with_time_limit(10,(
        not(pertenece(1,7,10,8)))).
		
test(pertenece_24,nondet):-
    call_with_time_limit(10,(
        not(pertenece(4,-6,10,8)))).

test(pertenece_25,nondet):-
    call_with_time_limit(10,(
        not(pertenece(2,-6,10,8)))).

test(pertenece_26,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-4,-6,10,8)))).

test(pertenece_27,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-2,-6,10,8)))).

test(pertenece_28,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-1,-7,10,8)))).

test(pertenece_29,nondet):-
    call_with_time_limit(10,(
        not(pertenece(0,-7,10,8)))).

test(pertenece_30,nondet):-
    call_with_time_limit(10,(
        not(pertenece(1,-7,10,8)))).

test(pertenece_31,nondet):-
    call_with_time_limit(10,(
        not(pertenece(5,0,10,8)))).

test(pertenece_32,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-5,0,10,8)))).

test(pertenece_33,nondet):-
    call_with_time_limit(10,(
        not(pertenece(5,3,10,8)))).

test(pertenece_34,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-5,-3,10,8)))).

test(pertenece_35,nondet):-
    call_with_time_limit(10,(
        not(pertenece(5,3,10,8)))).

test(pertenece_36,nondet):-
    call_with_time_limit(10,(
        not(pertenece(-5,-3,10,8)))).


:- end_tests(lab2).