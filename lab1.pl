:- module(lab1, [largo/2, todos_iguales/1, concatenacion/3, contenida/2]).

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
concatenacion(L1, [], L1).
concatenacion([], L2, L2).
concatenacion([C|L1], L2, [C|L]) :- concatenacion(L1, L2, L).

%% contenida(?L1,+L2) <- todos los elementos de la lista L1 pertenecen a L2.
%% Ej.: contenida([a,b,a,a],[a,b,c]).
contenida([], _).
contenida([C|L1], L2) :- member(C, L2), quitar_elemento(C, L1, L1sinC), quitar_elemento(C, L2, L2sinC), contenida(L1sinC, L2sinC).

%% quitar_elemento(E, L1, L2) <- predicado auxiliar: es verdadero si L2 es la lista L1 sin el elemento E (incluye el caso en que E no pertence a L1).
%% Ej.: quitar_elemento(a,[a,b,a,a],[b]), quitar_elemento(a,[b,c,d],[b,c,d]).
quitar_elemento(_, [], []).
quitar_elemento(E, [E|L1], [E|L2]) :- quitar_elemento(E, L1, L2).
quitar_elemento(E, [C|L1], [C|L2]) :- C \= E, quitar_elemento(E, L1, L2).


