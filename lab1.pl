:- module(lab1, [largo/2, todos_iguales/1, concatenacion/3, contenida/2, ww/2, sin_elem/3]).

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
