:- begin_tests(lab1).
:- use_module(lab1).

%% testear largo/2.
test(largo_no_vacia) :- largo([1,2,3,4], 4).
test(largo_vacia) :- largo([], 0).
%% test(falla) :- 1 is 0. % <- test verifica que el predicado es verdadero y que no quedan puntos de backtracking por explorar. Por ejemplo, este test falla si se descomenta.

%% testear todos_iguales/1.
test(todos_iguales_3) :- todos_iguales([a,a,a]).
test(todos_iguales_2) :- todos_iguales([a,a]).
test(todos_iguales_1) :- todos_iguales([a]).
test(todos_iguales_0) :- todos_iguales([]).

%% testear concatenacion/3.
test(concatenacion_ambas_vacias) :- concatenacion([], [], []).
test(concatenacion_segunda_vacia) :- concatenacion([1, 2, 3], [], [1, 2, 3]).
test(concatenacion_primera_vacia) :- concatenacion([], [4, 5], [4, 5]).
test(concatenacion_ninguna_vacia) :- concatenacion([1, 2, 3], [4, 5], [1, 2, 3, 4, 5]).

%% testear contenida/2.
test(contenida_vacia) :- contenida([], [a, b, c]).
test(contenida_un_repetido_en_primera) :- contenida([a, b, a], [a, b, c]).
test(contenida_repetidos_en_ambas) :- contenida([a, b, a], [a, b, c, b, a]).
test(contenida_dos_repetidos_en_primera) :- contenida([a, b, a, a, b], [a, b, c]).

:- end_tests(lab1).
