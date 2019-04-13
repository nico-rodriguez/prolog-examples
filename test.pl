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

%% testear ww/2.
test(ww_largo_igual_a_2) :- ww([a, a], [a, b, c]).
test(ww_largo_mayor_a_2) :- ww([a, c, c, a, c, c], [a, b, c]).

%% testear wwR/2.
test(wwR_ejemplo_1) :- wwR([a, b, b, a], [a, b]).
test(wwR_ejemplo_2) :- wwR([a, c, c, a], [a, b, c]).

%% testaer sin_elem/3.
test(sin_elem_caso_base) :- sin_elem([], a, []).
test(sin_elem_ejemplo_1) :- sin_elem([a, b, a, c], a, [b, c]).
test(sin_elem_ejemplo_2) :- sin_elem([a, a], a, []).
test(sin_elem_ejemplo_3) :- sin_elem([b, c], a, [b, c]).

:- end_tests(lab1).
