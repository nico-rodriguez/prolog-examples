% Se verifican varios casos para cada predicado. A su vez, para cada uno, se verifica cada instanciación posible. En general se pone a prueba que lo que debería ser solución realmente lo sea, y donde corresponde se verifica también que sea única la solución. Se perdona que se devuelva más de una vez true (pero no más de una vez la misma solución si se pasaron variables sin instanciar) y que se dejen puntos de backtracking aunque la solución sea única.

%Algunas veces se perdonan soluciones repetidas

% Para correr a mano los tests:
%
%   ?- [lab1].
%   ?- load_files('lab1.plt').
%   ?- run_tests.
%
% Preferir load_files para cargar los tests porque recarga el archivo si ya estaba cargado. Para ver el coverage:
%
%   ?- show_coverage(run_tests).
%

:- begin_tests(lab1).
%:- use_module(lab1). % No es necesario (y da error con el script), ya que load_files no lo precisa. load_test_files(X) sí lo precisaría.

% --------------------------
% largo(+L,?N)

test(largo_1,nondet):-
    call_with_time_limit(10,(
        largo([],0))).

test(largo_2,all(N == [0])):-
    call_with_time_limit(10,(
        largo([],N))).

test(largo_3,nondet):-
    call_with_time_limit(10,(
        largo([a],1))).

test(largo_4,all(N == [1])):-
    call_with_time_limit(10,(
        largo([a],N))).

test(largo_5,nondet):-
    call_with_time_limit(10,(
        largo([a,b],2))).

test(largo_6,all(N == [2])):-
    call_with_time_limit(10,(
        largo([a,b],N))).

test(largo_7,nondet):-
    call_with_time_limit(10,(
        largo([c,a,8],3))).

test(largo_8,all(N == [3])):-
    call_with_time_limit(10,(
        largo([c,a,8],N))).

test(largo_9,nondet):-
    call_with_time_limit(10,(
        largo([35,22,11,123,234,82,90,123,232,22],10))).

test(largo_10,all(N == [10])):-
    call_with_time_limit(10,(
        largo([35,22,11,123,234,82,90,123,232,22],N))).

%largo_lista_larga(+LLL) <- LLL es el largo de la lista larga a probar, para ver que estén usando acumuladores.
largo_lista_larga(10000000).

%lista_larga(?L,+N) <- L es una lista de largo N con elementos iguales a 'e'.
lista_larga([],0):-
    !.

lista_larga([e|Resto],N):-
    N > 0,
    N1 is N-1,
    lista_larga(Resto,N1).

test(largo_11,nondet):-
    call_with_time_limit(10,(
        largo_lista_larga(LLL),
        lista_larga(L,LLL),
        largo(L,LLL))).

test(largo_12):-
    call_with_time_limit(10,(
        largo_lista_larga(LLL),
        lista_larga(L,LLL),
        findall(N,largo(L,N),[LLL]))).

% --------------------------
% todos_iguales(?L,?E)

test(todos_iguales_1,nondet):-
    call_with_time_limit(10,(
        todos_iguales([],3))).

test(todos_iguales_2,nondet):-
    call_with_time_limit(10,(
        todos_iguales([],_))).

test(todos_iguales_3,[true(L == []),nondet]):-
    call_with_time_limit(10,(
        todos_iguales(L,3))).

test(todos_iguales_4):-
    call_with_time_limit(10,(
        todos_iguales([],E),
        var(E))).

test(todos_iguales_5):-
    call_with_time_limit(10,(
        findall(E,todos_iguales([],E),[E1]),
        var(E1))).

test(todos_iguales_6,[true(L == []),nondet]):-
    call_with_time_limit(10,(
        todos_iguales(L,E),
        var(E))).

test(todos_iguales_7,nondet):-
    call_with_time_limit(10,(
        todos_iguales([3],3))).

test(todos_iguales_8,[true(E == 3), nondet]):-
    call_with_time_limit(10,(
        todos_iguales([3],E))).

test(todos_iguales_9,nondet):-
    call_with_time_limit(10,(
        todos_iguales(L,3),
        L == [3])).

test(todos_iguales_10,nondet):-
    call_with_time_limit(10,(
        todos_iguales(L,E),
        L == [E],
        var(E))).

test(todos_iguales_11,nondet):-
    call_with_time_limit(10,(
        todos_iguales([3,3],3))).

test(todos_iguales_12,[true(E == 3), nondet]):-
    call_with_time_limit(10,(
        todos_iguales([3,3],E))).

test(todos_iguales_13,nondet):-
    call_with_time_limit(10,(
        todos_iguales(L,3),
        L == [3,3])).

test(todos_iguales_14,nondet):-
    call_with_time_limit(10,(
        todos_iguales(L,E),
        L == [E,E],
        var(E))).

test(todos_iguales_15,nondet):-
    call_with_time_limit(10,(
        todos_iguales([e,e,e,e,e,e,e,e,e,e],e))).

test(todos_iguales_16,[true(E == e), nondet]):-
    call_with_time_limit(10,(
        todos_iguales([e,e,e,e,e,e,e,e,e,e],E))).

test(todos_iguales_17,nondet):-
    call_with_time_limit(10,(
        todos_iguales(L,e),
        L == [e,e,e,e,e,e,e,e,e,e])).

test(todos_iguales_18,nondet):-
    call_with_time_limit(10,(
        todos_iguales(L,E),
        L == [E,E,E,E,E,E,E,E,E,E],
        var(E))).

% --------------------------
% concatenacion(?L1,?L2,?L)

test(concatenacion_1,nondet):-
    call_with_time_limit(10,(
        concatenacion([],[],[]))).

test(concatenacion_2,all(L1 == [[]])):-
    call_with_time_limit(10,(
        concatenacion(L1,[],[]))).

test(concatenacion_3,all(L2 == [[]])):-
    call_with_time_limit(10,(
        concatenacion([],L2,[]))).

test(concatenacion_4,all(L == [[]])):-
    call_with_time_limit(10,(
        concatenacion([],[],L))).

test(concatenacion_5,all((L1,L2) == [([],[])])):-
    call_with_time_limit(10,(
        concatenacion(L1,L2,[]))).

test(concatenacion_6,[true((L1,L) == ([],[])),nondet]):-
    call_with_time_limit(10,(
        concatenacion(L1,[],L))).

test(concatenacion_7,nondet):-
    call_with_time_limit(10,(
        concatenacion([],L2,L),
        L2 = L,
        L = [])).

test(concatenacion_8,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,L2,L),
        L1 = [],
        L2 = [],
        L = [])).

test(concatenacion_9,nondet):-
    call_with_time_limit(10,(
        concatenacion([],[a],[a]))).

test(concatenacion_10,all(L1 == [[]])):-
    call_with_time_limit(10,(
        concatenacion(L1,[a],[a]))).

test(concatenacion_11,all(L2 == [[a]])):-
    call_with_time_limit(10,(
        concatenacion([],L2,[a]))).

test(concatenacion_12,all(L == [[a]])):-
    call_with_time_limit(10,(
        concatenacion([],[a],L))).

test(concatenacion_13,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,L2,[a]),
        L1 == [],
        L2 == [a])).

test(concatenacion_14,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,[a],L),
        L1 == [],
        L == [a])).

test(concatenacion_15,nondet):-
    call_with_time_limit(10,(
        concatenacion([],L2,L),
        L2 = L,
        L = [a])).

test(concatenacion_16,nondet):-
    call_with_time_limit(10,(
        concatenacion([a],[],[a]))).

test(concatenacion_17,all(L1 == [[a]])):-
    call_with_time_limit(10,(
        concatenacion(L1,[],[a]))).

test(concatenacion_18,all(L2 == [[]])):-
    call_with_time_limit(10,(
        concatenacion([a],L2,[a]))).

test(concatenacion_19,all(L == [[a]])):-
    call_with_time_limit(10,(
        concatenacion([a],[],L))).

test(concatenacion_20,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,L2,[a]),
        L1 == [a],
        L2 == [])).

test(concatenacion_21,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,[],L),
        L1 = [a],
        L = [a])).

test(concatenacion_22,nondet):-
    call_with_time_limit(10,(
        concatenacion([a],L2,L),
        L2 = [],
        L = [a])).

test(concatenacion_23,nondet):-
    call_with_time_limit(10,(
        concatenacion([a],[b],[a,b]))).

test(concatenacion_24,all(L1 == [[a]])):-
    call_with_time_limit(10,(
        concatenacion(L1,[b],[a,b]))).

test(concatenacion_25,all(L2 == [[b]])):-
    call_with_time_limit(10,(
        concatenacion([a],L2,[a,b]))).

test(concatenacion_26,all(L == [[a,b]])):-
    call_with_time_limit(10,(
        concatenacion([a],[b],L))).

test(concatenacion_27,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,L2,[a,b]),
        L1 == [a],
        L2 == [b])).

test(concatenacion_28,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,[b],L),
        L1 = [a],
        L = [a,b])).

test(concatenacion_29,nondet):-
    call_with_time_limit(10,(
        concatenacion([a],L2,L),
        L2 = [b],
        L = [a,b])).

test(concatenacion_30,nondet):-
    call_with_time_limit(10,(
        concatenacion([d,f,b],[a,e,c],[d,f,b,a,e,c]))).

test(concatenacion_31,all(L1 == [[d,f,b]])):-
    call_with_time_limit(10,(
        concatenacion(L1,[a,e,c],[d,f,b,a,e,c]))).

test(concatenacion_32,all(L2 == [[a,e,c]])):-
    call_with_time_limit(10,(
        concatenacion([d,f,b],L2,[d,f,b,a,e,c]))).

test(concatenacion_33,all(L == [[d,f,b,a,e,c]])):-
    call_with_time_limit(10,(
        concatenacion([d,f,b],[a,e,c],L))).

test(concatenacion_34,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,L2,[d,f,b,a,e,c]),
        L1 == [d,f,b],
        L2 == [a,e,c])).

test(concatenacion_35,nondet):-
    call_with_time_limit(10,(
        concatenacion(L1,[a,e,c],L),
        L1 = [d,f,b],
        L = [d,f,b,a,e,c])).

test(concatenacion_36,nondet):-
    call_with_time_limit(10,(
        concatenacion([d,f,b],L2,L),
        L2 = [a,e,c],
        L = [d,f,b,a,e,c])).

test(concatenacion_37,set((L1,L2) == [([],[d,f,b,a,e,c]),([d],[f,b,a,e,c]),([d,f],[b,a,e,c]),([d,f,b],[a,e,c]),([d,f,b,a],[e,c]),([d,f,b,a,e],[c]),([d,f,b,a,e,c],[])])):-
    concatenacion(L1,L2,[d,f,b,a,e,c]).

% --------------------------
% merge(?L1,?L2,?L)

test(merge_1,all(L == [[1,3,5,5,9,10,12]])):-
    call_with_time_limit(10,(
        merge([1,5,9,12],[3,5,10],L))).

test(merge_2,nondet):-
    call_with_time_limit(10,(
        merge([-10,5,6,13],[0,1,2,3],[-10,0,1,2,3,5,6,13]))).

test(merge_3,all(L == [[1,3,5,10,11,200,500,1000]])):-
    call_with_time_limit(10,(
        merge([3,10,200],[1,5,11,500,1000],L))).

test(merge_4,nondet):-
    call_with_time_limit(10,(
        merge([],[],[]))).

test(merge_5,nondet):-
    call_with_time_limit(10,(
        merge([],[],[]))).

test(merge_6,nondet):-
    call_with_time_limit(10,(
        randset(10,1000,L),
        merge(L,[],L))).

test(merge_7,nondet):-
    call_with_time_limit(10,(
        randset(10,1000,L),
        merge([],L,L))).

test(merge_8,all(L == [[1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10]])):-
    call_with_time_limit(10,(
        merge([1,2,3,4,5,6,7,8,9,10],[1,2,3,4,5,6,7,8,9,10],L))).

test(merge_9,nondet):-
    call_with_time_limit(10,(
        \+ merge([1,5,9,12],[3,5,10],[1,3,5,9,10,12]))).

% --------------------------
% sin_elem(+L,?E,?LSinE)

test(sin_elem_1,nondet):-
    call_with_time_limit(10,(
        sin_elem([a],a,[]))).

test(sin_elem_2,[true(LSinE == []),nondet]):-
    call_with_time_limit(10,(
        sin_elem([a],a,LSinE))).

test(sin_elem_3,[true(E == a),nondet]):-
    call_with_time_limit(10,(
        sin_elem([a],E,[]))).

test(sin_elem_4,[true((E,LSinE) == (a,[])),nondet]):-
    call_with_time_limit(10,(
        sin_elem([a],E,LSinE))).

test(sin_elem_5,nondet):-
    call_with_time_limit(10,(
        sin_elem([5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7],5,[t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7]))).

test(sin_elem_6,[set(LSinE == [[t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7],[5,t,7,e,t,6,y,a,3,l,9,c,t,2,b,6,h,7]]),nondet]):-
    sin_elem([5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7],5,LSinE).

test(sin_elem_7,[true(E == 5),nondet]):-
    call_with_time_limit(10,(
        sin_elem([5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7],E,[t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7]))).

test(sin_elem_8,[set((E,LSinE) == [(5,[t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7]),(t,[5,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7]),(7,[5,t,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7]),(e,[5,t,7,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7]),(t,[5,t,7,e,6,y,a,3,l,9,c,5,t,2,b,6,h,7]),(6,[5,t,7,e,t,y,a,3,l,9,c,5,t,2,b,6,h,7]),(y,[5,t,7,e,t,6,a,3,l,9,c,5,t,2,b,6,h,7]),(a,[5,t,7,e,t,6,y,3,l,9,c,5,t,2,b,6,h,7]),(3,[5,t,7,e,t,6,y,a,l,9,c,5,t,2,b,6,h,7]),(l,[5,t,7,e,t,6,y,a,3,9,c,5,t,2,b,6,h,7]),(9,[5,t,7,e,t,6,y,a,3,l,c,5,t,2,b,6,h,7]),(c,[5,t,7,e,t,6,y,a,3,l,9,5,t,2,b,6,h,7]),(5,[5,t,7,e,t,6,y,a,3,l,9,c,t,2,b,6,h,7]),(t,[5,t,7,e,t,6,y,a,3,l,9,c,5,2,b,6,h,7]),(2,[5,t,7,e,t,6,y,a,3,l,9,c,5,t,b,6,h,7]),(b,[5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,6,h,7]),(6,[5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,h,7]),(h,[5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,7]),(7,[5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h])]),nondet]):-
    sin_elem([5,t,7,e,t,6,y,a,3,l,9,c,5,t,2,b,6,h,7],E,LSinE).

% --------------------------
% rotacion(+L,?R)

test(rotacion_1,nondet):-
    call_with_time_limit(10,(
        rotacion([],[]))).

test(rotacion_2,[true(R == []),nondet]):-
    call_with_time_limit(10,(
        rotacion([],R))).

test(rotacion_3,nondet):-
    call_with_time_limit(10,(
        rotacion([f,e,f,t,8,9,e],[f,t,8,9,e,f,e]))).

% No voy a verificar todas las rotaciones porque quedó ambiguo si una lista es una rotación de sí misma, pero pruebo de una forma tal que ambas soluciones son válidas.
test(rotacion_4,nondet):-
    call_with_time_limit(10,(
        findall(R,rotacion([f,e,f,t,8,9,e],R),Sols),
        member([e,f,t,8,9,e,f],Sols),
        member([f,t,8,9,e,f,e],Sols),
        member([t,8,9,e,f,e,f],Sols),
        member([8,9,e,f,e,f,t],Sols),
        member([9,e,f,e,f,t,8],Sols),
        length(Sols,N),
        between(6,7,N),
        (
            N is 7
            -> (
                member([f,e,f,t,8,9,e],Sols)
            )
        ))).

test(rotacion_5,nondet):-
    call_with_time_limit(10,(
        \+ rotacion([1,2,3,4,5],[1,2,4,3,5]))).

% --------------------------
% subsecuencia(?L,?Subsec)

test(subsecuencia_1,nondet):-
    call_with_time_limit(10,(
        subsecuencia([a,6,b,48,test,c],[6,test]))).

test(subsecuencia_2,nondet):-
    call_with_time_limit(10,(
        \+ subsecuencia([a,6,b,48,test,c],[test,6]))).

test(subsecuencia_3,nondet):-
    call_with_time_limit(10,(
        findall(S,subsecuencia([5,2,3,1,7],S),Sols),
        member([5, 2, 3, 1, 7],Sols),
        member([5, 2, 3, 1],Sols),
        member([5, 2, 3, 7],Sols),
        member([5, 2, 3],Sols),
        member([5, 2, 1, 7],Sols),
        member([5, 2, 1],Sols),
        member([5, 2, 7],Sols),
        member([5, 2],Sols),
        member([5, 3, 1, 7],Sols),
        member([5, 3, 1],Sols),
        member([5, 3, 7],Sols),
        member([5, 3],Sols),
        member([5, 1, 7],Sols),
        member([5, 1],Sols),
        member([5, 7],Sols),
        member([5],Sols),
        member([2, 3, 1, 7],Sols),
        member([2, 3, 1],Sols),
        member([2, 3, 7],Sols),
        member([2, 3],Sols),
        member([2, 1, 7],Sols),
        member([2, 1],Sols),
        member([2, 7],Sols),
        member([2],Sols),
        member([3, 1, 7],Sols),
        member([3, 1],Sols),
        member([3, 7],Sols),
        member([3],Sols),
        member([1, 7],Sols),
        member([1],Sols),
        member([7],Sols),
        member([],Sols),
        length(Sols,32))).

test(subsecuencia_4,nondet):-
    call_with_time_limit(10,(
        findall([X,Y],subsecuencia([10,b,atom,-2,5],[X,Y]),Sols),
        member([10, b],Sols),
        member([10, atom],Sols),
        member([10, -2],Sols),
        member([10, 5],Sols),
        member([b, atom],Sols),
        member([b, -2],Sols),
        member([b, 5],Sols),
        member([atom, -2],Sols),
        member([atom, 5],Sols),
        member([-2, 5],Sols),
        length(Sols,10))).

test(subsecuencia_5,nondet):-
    call_with_time_limit(10,(
        subsecuencia([X,Y],[1,2]),
        X = 1,
        Y = 2)).


% -----------------------------
% juego de cartas
% -----------------------------

% -----------------------------
% jugada_valida(+Mano,+Tapete,?Jugada).

test(jugada_valida_1,nondet):-
    call_with_time_limit(10,(
        Mano = [carta(5,oro),carta(2,basto),carta(7,espada)],
        Tapete = [carta(2,oro),carta(10,copa),carta(6,espada),carta(1,espada),carta(9,oro)],
        findall(J,jugada_valida(Mano,Tapete,J),Js),
        member(jugada(carta(5,oro),[carta(10,copa)]),Js),
        member(jugada(carta(5,oro),[carta(1,espada),carta(9,oro)]),Js),
        member(jugada(carta(2,basto),[carta(2,oro),carta(10,copa),carta(1,espada)]),Js),
        member(jugada(carta(7,espada),[carta(2,oro),carta(6,espada)]),Js),
        length(Js,4))).

test(jugada_valida_2,nondet):-
    call_with_time_limit(10,(
        Mano = [carta(8,oro)],
        Tapete = [carta(7,basto)],
        findall(J,jugada_valida(Mano,Tapete,J),Js),
        member(jugada(carta(8,oro),[carta(7,basto)]),Js),
        length(Js,1))).

test(jugada_valida_3,nondet):-
    call_with_time_limit(10,(
        Mano = [carta(5,oro),carta(2,basto),carta(7,espada)],
        Tapete = [],
        \+ jugada_valida(Mano,Tapete,_))).

test(jugada_valida_4,nondet):-
    call_with_time_limit(10,(
        Mano = [carta(4,oro),carta(2,basto),carta(8,espada)],
        Tapete = [carta(2,oro),carta(4,espada),carta(6,oro),carta(8,copa),carta(10,oro),carta(12,copa)],
        \+ jugada_valida(Mano,Tapete,_))).

test(jugada_valida_5,nondet):-
    call_with_time_limit(10,(
        Mano = [carta(1,oro),carta(2,basto),carta(3,espada)],
        Tapete = [carta(1,copa),carta(7,oro),carta(2,espada),carta(2,copa)],
        jugada_valida(Mano,Tapete,jugada(carta(3,espada),[carta(1,copa),carta(7,oro),carta(2,espada),carta(2,copa)])))).

% -----------------------------
% predicados de matrices
% -----------------------------

% -----------------------------
% matriz(+M,+N,+E,?A)

test(matriz_1,[all(A == [[[5,5],[5,5]]]),nondet]):-
    call_with_time_limit(10,matriz(2,2,5,A)).
	
% matriz(+M,+N,+E,?A).
test(matriz_2,[all(A == [[[11,11],[11,11],[11,11]]]),nondet]):-
	call_with_time_limit(10,matriz(3,2,11,A)).
	
% matriz(+M,+N,+E,?A).
test(matriz_3,[all(A == [[[7,7,7],[7,7,7]]]),nondet]):-
	call_with_time_limit(10,matriz(2,3,7,A)).

% matriz(+M,+N,+E,?A).
test(matriz_4,nondet):-
	call_with_time_limit(10,matriz(1,1,-3,[[-3]])).

% matriz(+M,+N,+E,?A).
test(matriz_5,nondet):-
    call_with_time_limit(10,(
        \+ matriz(2,2,6,[[6,6],[6,3]]))).

% -----------------------------
% valor_celda(+I,+J,+A,?E)

test(valor_celda_1,nondet):-
    call_with_time_limit(10,(
        matriz(2,2,5,A),
        valor_celda(1,1,A,5),
        valor_celda(1,2,A,5),
        valor_celda(2,1,A,5),
        valor_celda(2,2,A,5))).

test(valor_celda_2,nondet):-
    call_with_time_limit(10,(
        matriz(3,2,7,A),
        \+ valor_celda(2,1,A,5))).

% -----------------------------
% nuevo_valor_celda(+I,+J,+A1,+E,?A2)

test(nuevo_valor_celda_1,nondet):-
    call_with_time_limit(10,(
        matriz(2,2,5,A1),
        nuevo_valor_celda(1,1,A1,-3,A2),
        valor_celda(1,1,A2,-3),
        valor_celda(1,2,A2,5),
        valor_celda(2,1,A2,5),
        valor_celda(2,2,A2,5))).

test(nuevo_valor_celda_2,nondet):-
    call_with_time_limit(10,(
        matriz(2,2,5,A1),
        nuevo_valor_celda(1,2,A1,-3,A2),
        valor_celda(1,1,A2,5),
        valor_celda(1,2,A2,-3),
        valor_celda(2,1,A2,5),
        valor_celda(2,2,A2,5))).

test(nuevo_valor_celda_3,nondet):-
    call_with_time_limit(10,(
        matriz(2,2,5,A1),
        nuevo_valor_celda(2,1,A1,-3,A2),
        valor_celda(1,1,A2,5),
        valor_celda(1,2,A2,5),
        valor_celda(2,1,A2,-3),
        valor_celda(2,2,A2,5))).

test(nuevo_valor_celda_4,nondet):-
    call_with_time_limit(10,(
        matriz(2,2,5,A1),
        nuevo_valor_celda(2,2,A1,-3,A2),
        valor_celda(1,1,A2,5),
        valor_celda(1,2,A2,5),
        valor_celda(2,1,A2,5),
        valor_celda(2,2,A2,-3))).

% -----------------------------
% adyacente(+I,+J,+A,?I2,?J2,?V)

matriz_3_3(A9):-
    matriz(3,3,0,A0),
    nuevo_valor_celda(1,1,A0,-3,A1),
    nuevo_valor_celda(1,2,A1,-2,A2),
    nuevo_valor_celda(1,3,A2,-1,A3),
    nuevo_valor_celda(2,1,A3,-1,A4),
    nuevo_valor_celda(2,2,A4,0,A5),
    nuevo_valor_celda(2,3,A5,1,A6),
    nuevo_valor_celda(3,1,A6,1,A7),
    nuevo_valor_celda(3,2,A7,2,A8),
    nuevo_valor_celda(3,3,A8,3,A9).

test(adyacente_1,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(1,1,A,I,J,V),Sols),
        member([1,2,-2],Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        length(Sols,3))).

test(adyacente_2,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(1,2,A,I,J,V),Sols),
        member([1,1,-3],Sols),
        member([1,3,-1],Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        length(Sols,5))).

test(adyacente_3,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(1,3,A,I,J,V),Sols),
        member([1,2,-2],Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        length(Sols,3))).

test(adyacente_4,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(2,1,A,I,J,V),Sols),
        member([1,1,-3],Sols),
        member([1,2,-2],Sols),
        member([2,2,0],Sols),
        member([3,1,1],Sols),
        member([3,2,2],Sols),
        length(Sols,5))).

test(adyacente_5,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(2,2,A,I,J,V),Sols),
        member([1,1,-3],Sols),
        member([1,2,-2],Sols),
        member([1,3,-1],Sols),
        member([2,1,-1],Sols),
        member([2,3,1],Sols),
        member([3,1,1],Sols),
        member([3,2,2],Sols),
        member([3,3,3],Sols),
        length(Sols,8))).

test(adyacente_6,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(2,3,A,I,J,V),Sols),
        member([1,2,-2],Sols),
        member([1,3,-1],Sols),
        member([2,2,0],Sols),
        member([3,2,2],Sols),
        member([3,3,3],Sols),
        length(Sols,5))).

test(adyacente_7,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(3,1,A,I,J,V),Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        member([3,2,2],Sols),
        length(Sols,3))).

test(adyacente_8,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(3,2,A,I,J,V),Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        member([3,1,1],Sols),
        member([3,3,3],Sols),
        length(Sols,5))).

test(adyacente_9,nondet):-
    call_with_time_limit(10,(
        matriz_3_3(A),
        findall([I,J,V],adyacente(3,3,A,I,J,V),Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        member([3,2,2],Sols),
        length(Sols,3))).

% -----------------------------
% matriz_f(+M,+N,+E,?A)

test(matriz_f_1,[all(A == [matrix(row(5,5),row(5,5))]),nondet]):-
	call_with_time_limit(10,matriz_f(2,2,5,A)).
	
test(matriz_f_2,[all(A == [matrix(row(11,11),row(11,11),row(11,11))]),nondet]):-
	call_with_time_limit(10,matriz_f(3,2,11,A)).
	
test(matriz_f_3,[all(A == [matrix(row(7,7,7),row(7,7,7))]),nondet]):-
	call_with_time_limit(10,matriz_f(2,3,7,A)).

test(matriz_f_4,nondet):-
	call_with_time_limit(10,matriz_f(1,1,-3,matrix(row(-3)))).

test(matriz_f_5,nondet):-
	call_with_time_limit(10,\+ matriz_f(2,2,6,matrix(row(6,6),row(6,3)))).

% -----------------------------
% valor_celda_f(+I,+J,+A,?E)

test(valor_celda_f_1,nondet):-
	call_with_time_limit(10,(
        matriz_f(2,2,5,A),
        valor_celda_f(1,1,A,5),
        valor_celda_f(1,2,A,5),
        valor_celda_f(2,1,A,5),
        valor_celda_f(2,2,A,5))).

test(valor_celda_f_2,nondet):-
    call_with_time_limit(10,(
        matriz_f(3,2,7,A),
        \+ valor_celda_f(2,1,A,5))).

% -----------------------------
% nuevo_valor_celda_f(+I,+J,+A,+E)

test(nuevo_valor_celda_f_1,nondet):-
	call_with_time_limit(10,(
        matriz_f(2,2,5,A),
        nuevo_valor_celda_f(1,1,A,-3),
        valor_celda_f(1,1,A,-3),
        valor_celda_f(1,2,A,5),
        valor_celda_f(2,1,A,5),
        valor_celda_f(2,2,A,5))).

test(nuevo_valor_celda_f_2,nondet):-
	call_with_time_limit(10,(
        matriz_f(2,2,5,A),
        nuevo_valor_celda_f(1,2,A,-3),
        valor_celda_f(1,1,A,5),
        valor_celda_f(1,2,A,-3),
        valor_celda_f(2,1,A,5),
        valor_celda_f(2,2,A,5))).

test(nuevo_valor_celda_f_3,nondet):-
	call_with_time_limit(10,(
        matriz_f(2,2,5,A),
        nuevo_valor_celda_f(2,1,A,-3),
        valor_celda_f(1,1,A,5),
        valor_celda_f(1,2,A,5),
        valor_celda_f(2,1,A,-3),
        valor_celda_f(2,2,A,5))).

test(nuevo_valor_celda_f_4,nondet):-
	call_with_time_limit(10,(
        matriz_f(2,2,5,A),
        nuevo_valor_celda_f(2,2,A,-3),
        valor_celda_f(1,1,A,5),
        valor_celda_f(1,2,A,5),
        valor_celda_f(2,1,A,5),
        valor_celda_f(2,2,A,-3))).

% -----------------------------
% adyacente(+I,+J,+A,?I2,?J2,?V)

matriz_f_3_3(A):-
	matriz_f(3,3,0,A),
	nuevo_valor_celda_f(1,1,A,-3),
	nuevo_valor_celda_f(1,2,A,-2),
	nuevo_valor_celda_f(1,3,A,-1),
	nuevo_valor_celda_f(2,1,A,-1),
	nuevo_valor_celda_f(2,2,A,0),
	nuevo_valor_celda_f(2,3,A,1),
	nuevo_valor_celda_f(3,1,A,1),
	nuevo_valor_celda_f(3,2,A,2),
	nuevo_valor_celda_f(3,3,A,3).

test(adyacente_f_1,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(1,1,A,I,J,V),Sols),
        member([1,2,-2],Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        length(Sols,3))).

test(adyacente_f_2,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(1,2,A,I,J,V),Sols),
        member([1,1,-3],Sols),
        member([1,3,-1],Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        length(Sols,5))).

test(adyacente_f_3,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(1,3,A,I,J,V),Sols),
        member([1,2,-2],Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        length(Sols,3))).

test(adyacente_f_4,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(2,1,A,I,J,V),Sols),
        member([1,1,-3],Sols),
        member([1,2,-2],Sols),
        member([2,2,0],Sols),
        member([3,1,1],Sols),
        member([3,2,2],Sols),
        length(Sols,5))).

test(adyacente_f_5,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(2,2,A,I,J,V),Sols),
        member([1,1,-3],Sols),
        member([1,2,-2],Sols),
        member([1,3,-1],Sols),
        member([2,1,-1],Sols),
        member([2,3,1],Sols),
        member([3,1,1],Sols),
        member([3,2,2],Sols),
        member([3,3,3],Sols),
        length(Sols,8))).

test(adyacente_f_6,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(2,3,A,I,J,V),Sols),
        member([1,2,-2],Sols),
        member([1,3,-1],Sols),
        member([2,2,0],Sols),
        member([3,2,2],Sols),
        member([3,3,3],Sols),
        length(Sols,5))).

test(adyacente_f_7,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(3,1,A,I,J,V),Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        member([3,2,2],Sols),
        length(Sols,3))).

test(adyacente_f_8,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(3,2,A,I,J,V),Sols),
        member([2,1,-1],Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        member([3,1,1],Sols),
        member([3,3,3],Sols),
        length(Sols,5))).

test(adyacente_f_9,nondet):-
	call_with_time_limit(10,(
        matriz_f_3_3(A),
        findall([I,J,V],adyacente_f(3,3,A,I,J,V),Sols),
        member([2,2,0],Sols),
        member([2,3,1],Sols),
        member([3,2,2],Sols),
        length(Sols,3))).

:- end_tests(lab1).