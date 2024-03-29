:- begin_tests(lab1).
:- use_module(lab1).

%% ----------------
%% 		listas
%% ----------------

%% testear largo/2.
test(largo_no_vacia) :- largo([1,2,3,4], 4).
test(largo_vacia) :- largo([], 0).
test(largo_sin_instanciar_N) :- largo([1,2,3,4], N), N = 4.
test(largo_sin_instanciar_L) :- largo(L, 4), L = [_,_,_,_].
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
test(concatenacion_sin_instanciar_lista_1) :- concatenacion(L, [4, 5], [1, 2, 3, 4, 5]), L = [1, 2, 3].
test(concatenacion_sin_instanciar_lista_2) :- concatenacion([1, 2, 3], L, [1, 2, 3, 4, 5]), L = [4, 5].
test(concatenacion_sin_instanciar_lista_3) :- concatenacion([1, 2, 3], [4, 5], L), L = [1, 2, 3, 4, 5].

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
test(sin_elem_sin_instanciar_E) :- sin_elem([1,2,3], 1, LSinE), LSinE = [2,3].
test(sin_elem_sin_instanciar_LSinE) :- sin_elem([1,2,3], E, [2,3]), E = 1. 

%% testear sublista/2.
test(sublista2_lista_vacia) :- sublista([1, 2, 3, 4], []).
test(sublista2_misma_lista) :- sublista([1, 2, 3, 4], [1, 2, 3, 4]).
test(sublista2_ejemplo) :- sublista([5, 2, 3, 1, 7], [2, 3, 1]).

%% testear enesimo/3.
test(enesimo_ejemplo_1) :- enesimo([5, 2, 3, 1, 7], 4, 1).
test(enesimo_ejemplo_2) :- enesimo([5, 2, [3, 1], 7], 3, [3, 1]).
test(enesimo_sin_instanciar_N) :- enesimo([5, 2, 3, 1, 7], N, 1), N = 4.
test(enesimo_sin_instanciar_E) :- enesimo([5, 2, 3, 1, 7], 4, E), E = 1.

%% testear sublist/4.
test(sublista4_ejemplo) :- sublista([5, 2, 3, 1, 7], [2, 3, 1], 2, 4).
test(sublista4_caso_1) :- sublista([1, 2, 3, 4], [2, 3], I, J), I = 2, J = 3.
test(sublista4_caso_2) :- sublista([1, 2, 3, 4], Sub, 2, 3), Sub = [2, 3].
test(sublista4_caso_1) :- sublista([1, 2, 3, 4], [2, 3], I, 3), I = 2.
test(sublista4_caso_1) :- sublista([1, 2, 3, 4], [2, 3], 2, J), J = 3.
%% test(sublista4_caso_3) :- sublista(L, [2, 3], 2, J), L = [_, 2, 3|_], J = 3.
%% test(sublista4_caso_4) :- sublista(L, [2, 3], I, 3), L = [_, 2, 3|_], I = 2.

%% ------------------
%% 		matrices
%% ------------------

%% matriz/3
test(matriz_ejemplo) :- matriz(3,3,[[8,-10,1],[5,4,2], [7,9,3]]).
test(matriz_caso_1) :- matriz(3,N,[[8,-10,1],[5,4,2], [7,9,3]]), N=3.
test(matriz_caso_2) :- matriz(M,3,[[8,-10,1],[5,4,2], [7,9,3]]), M=3.
test(matriz_caso_3) :- matriz(M,N,[[8,-10,1],[5,4,2], [7,9,3]]), M=3, N=3.
test(matriz_caso_4) :- matriz(M,N,[[8,-10],[5,4],[7,9]]), M=3, N=2.
test(matriz_caso_5) :- matriz(M,N,[[8,-10,1],[5,4,2]]), M=2, N=3.
test(matriz_caso_6) :- matriz(M,N,[[8,-10],[5,4]]), M=2, N=2.

%% valor_celda/4
test(valor_celda_ejemplo) :- valor_celda(2,1,[[8,-10,1],[5,4,2], [7,9,3]],5).
test(valor_celda_caso_1) :- valor_celda(2,1,[[8,-10,1],[5,4,2], [7,9,3]],E), E=5.
test(valor_celda_caso_2) :- valor_celda(1,1,[[8,-10,1],[5,4,2], [7,9,3]],E), E=8.
test(valor_celda_caso_3) :- valor_celda(3,3,[[8,-10,1],[5,4,2], [7,9,3]],E), E=3.
test(valor_celda_caso_4) :- valor_celda(1,3,[[8,-10,1],[5,4,2], [7,9,3]],E), E=1.
test(valor_celda_caso_5) :- valor_celda(I,J,[[8,-10,1],[5,4,2], [7,9,3]],5), I=2, J=1.
test(valor_celda_caso_6) :- valor_celda(I,J,[[8,-10,1],[5,4,2], [7,9,3]],8), I=1, J=1.
test(valor_celda_caso_7) :- valor_celda(I,J,[[8,-10,1],[5,4,2], [7,9,3]],3), I=3, J=3.
test(valor_celda_caso_8) :- valor_celda(I,J,[[8,-10,1],[5,4,2], [7,9,3]],1), I=1, J=3.

%% fila/3
test(fila_ejemplo) :- fila([[8,-10,1],[5,4,2],[7,9,3]],3,[7,9,3]).
test(fila_caso_1) :- fila([[8,-10,1],[5,4,2],[7,9,3]],3,F), F=[7,9,3].
test(fila_caso_2) :- fila([[8,-10,1],[5,4,2],[7,9,3]],N,[7,9,3]), N=3.
test(fila_caso_3) :- fila([[8,-10,1],[5,4,2],[7,9,3]],1,F), F=[8,-10,1].
test(fila_caso_4) :- fila([[8,-10,1],[5,4,2],[7,9,3]],N,[8,-10,1]), N=1.

%% col/3
test(col_ejemplo) :- col([[8,-10,1],[5,4,2],[7,9,3]],2,[-10,4,9]).
test(col_caso_1) :- col([[8,-10,1],[5,4,2],[7,9,3]],N,[-10,4,9]), N=2.
test(col_caso_2) :- col([[8,-10,1],[5,4,2],[7,9,3]],2,C), C=[-10,4,9].
test(col_caso_3) :- col([[8,-10,1],[5,4,2],[7,9,3]],N,[1,2,3]), N=3.
test(col_caso_4) :- col([[8,-10,1],[5,4,2],[7,9,3]],3,C), C=[1,2,3].


%% diagonalD(+M,coord(?I,?J),?Dir)/3
test(diagonalD_ejemplo_1) :- diagonalD([[8,-10,1],[5,4,2],[7,9,3]],coord(1,2),[-10,2]).
test(diagonalD_ejemplo_2) :- diagonalD([[8,-10,1],[5,4,2],[7,9,3]],coord(2,1),[5,9]).
test(diagonalD_ejemplo_3) :- diagonalD([[8,-10,1],[5,4,2],[7,9,3]],coord(1,1),[8,4,3]).
test(diagonalD_ejemplo_4) :- diagonalD([[8,-10,1],[5,4,2],[7,9,3]],coord(3,1),[7]).
test(diagonalD_caso_1) :- diagonalD([[5,4], [7,9]],coord(1,1),[5,9]).
test(diagonalD_caso_2) :- diagonalD([[5,4], [7,9]],coord(I,J),[5,9]), I=1, J=1.
test(diagonalD_caso_3) :- diagonalD([[5,4], [7,9]],coord(1,2),[4]).
test(diagonalD_caso_4) :- diagonalD([[5,4]],coord(1,1),[5]).
test(diagonalD_caso_5) :- diagonalD([[5,4]],coord(1,2),[4]).
test(diagonalD_caso_6) :- diagonalD([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(I,J),[1,6,11,16]),I=1,J=1.
test(diagonalD_caso_7) :- diagonalD([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(1,3),X),X=[3,8].
test(diagonalD_caso_8) :- diagonalD([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(I,J),[5,10,15]),I=2,J=1.
test(diagonalD_caso_9) :- diagonalD([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(4,1),[13]).

%% diagonalI(+M,coord((?I,?J),?Inv)/3
test(diagonalI_ejemplo_1) :- diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(3,2),[9,2]).
test(diagonalI_ejemplo_2) :- diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(2,1),[5,-10]).
test(diagonalI_ejemplo_3) :- diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(1,1),[8]).
test(diagonalI_ejemplo_4) :- diagonalI([[8,-10,1],[5,4,2], [7,9,3]],coord(3,1),[7,4,1]).
test(diagonalI_caso_1) :- diagonalI([[5,4], [7,9]],coord(1,1),[5]).
test(diagonalI_caso_2) :- diagonalI([[5,4], [7,9]],coord(I,J),[7,4]), I=2, J=1.
test(diagonalI_caso_3) :- diagonalI([[5,4], [7,9]],coord(2,2),[9]).
test(diagonalI_caso_4) :- diagonalI([[5,4]],coord(1,1),[5]).
test(diagonalI_caso_5) :- diagonalI([[5,4]],coord(1,2),[4]).
test(diagonalI_caso_6) :- diagonalI([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(I,J),[13,10,7,4]),I=4,J=1.
test(diagonalI_caso_7) :- diagonalI([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(4,3),X),X=[15,12].
test(diagonalI_caso_8) :- diagonalI([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(I,J),[14,11,8]),I=4,J=2.
test(diagonalI_caso_9) :- diagonalI([[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]],coord(4,4),[16]).

%% sopa/3
matrizSopa([[a,c,a,v,d,c,k,c],
            [o,y,w,l,o,b,a,e],
            [v,s,m,n,z,n,k,r],
            [a,o,e,p,i,p,o,d],
            [p,j,v,l,o,t,u,o],
            [o,w,l,e,a,l,p,e],
            [o,a,q,p,j,t,l,w],
            [g,j,q,i,t,a,e,o]]).

palabras([[c,e,r,d,o],
          [p,a,v,o],
          [l,e,a,l],
          [v,a,c,a],
          [g,a,l,l,i,n,a],
          [c,o,n,e,j,o],
          [p,a,t,o],
          [o,v,e,j,a],
          [p,o,l,l,o]]).


coordenadas([p([c,e,r,d,o],[(1,8),(5,8)]),
             p([p,a,v,o],[(5,1),(2,1)]),
             p([l,e,a,l],[(6,3),(6,6)]),
             p([o,v,e,j,a],[(4,2),(8,6)]),
             p([c,o,n,e,j,o],[(1,6),(6,1)]),
             p([g,a,l,l,i,n,a],[(8,1),(2,7)]),
             p([p,a,t,o],[(7,4),(4,7)]),
             p([v,a,c,a],[(1,4),(1,1)]),
             p([p,o,l,l,o],[(4,4),(8,8)])]).

test(sopa_caso_1):- matrizSopa(MatrizSopa),
                    palabras(Palabras),
                    coordenadas(Coordenadas),
                    sopa(MatrizSopa,Palabras,Coordenadas).

:- end_tests(lab1).
