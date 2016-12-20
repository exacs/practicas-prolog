:- module(pract2, [lista_de_moves/2,
        lista_de_swaps/2,
        hacer_move/3,
        hacer_swap/4,
        generar_estados_siguientes/5,
        descartar_caminos/3,
        son_iguales/2]).

alumno_prode('Sabina', 'Hidalgo', 'Irene', 'v130321').
alumno_prode('Saito', 'Murata', 'Carlos', 'v130215').
alumno_prode('Vargas', 'Azpitarte', 'Daniel', 'v130290').

son_iguales(A, B) :-
        functor(A, regs, Longitud),
        son_iguales_rec(A,B,Longitud).

son_iguales_rec(_,_,0).
son_iguales_rec(A,B,I) :-
        I > 0,
        arg(I, A, ElementoA),
        arg(I, B, ElementoB),
        son_elementos_iguales(ElementoA, ElementoB),
        I2 is I - 1,
        son_iguales_rec(A,B, I2).

son_elementos_iguales(X,X).
son_elementos_iguales(*,_).
son_elementos_iguales(_,*).


descartar_caminos(Caminos, Estado_Final, Caminos_Sin_Descartar) :-
        descartar_imposibles(Caminos, Estado_Final, CSD1),
        descartar_ciclos(CSD1, Caminos_Sin_Descartar).


descartar_imposibles([], _, []).
%%% Comentalo tu Irene :D
descartar_imposibles([Camino|Caminos], Estado_Final, [Camino|Caminos_Sin_Descartar]) :-
        Camino = camino(_, _, Estado_Actual),
        contiene(Estado_Actual, Estado_Final),
        descartar_imposibles(Caminos, Estado_Final, Caminos_Sin_Descartar),
        !.

descartar_imposibles([_|Caminos], Estado_Final, Caminos_Sin_Descartar) :-
        descartar_imposibles(Caminos, Estado_Final, Caminos_Sin_Descartar).

descartar_ciclos([], []).
descartar_ciclos([Camino|Caminos], [Camino|Caminos_Sin_Descartar]) :-
        Camino = camino(Estados_Anteriores,_,Estado_Actual),
        descartar_ciclos_rec(Estados_Anteriores, Estado_Actual),
        descartar_ciclos(Caminos, Caminos_Sin_Descartar),
        !.

descartar_ciclos([_|Caminos], Caminos_Sin_Descartar) :-
        descartar_ciclos(Caminos, Caminos_Sin_Descartar).

descartar_ciclos_rec([], _).
descartar_ciclos_rec([Estado_Anterior|Estados_Anteriores], Estado_Actual) :-
        no_son_iguales(Estado_Anterior, Estado_Actual),
        descartar_ciclos_rec(Estados_Anteriores, Estado_Actual).


no_son_iguales(Estado_Anterior, Estado_Actual) :-
        son_iguales(Estado_Anterior, Estado_Actual), !, fail.

no_son_iguales(_,_).


contiene(Estado_Actual, Estado_Final) :-
        functor(Estado_Actual, regs, Longitud),
        contiene_rec(Estado_Actual, Estado_Final, Longitud),
        !.

contiene_rec(_,_,0).
contiene_rec(Estado_Actual, Estado_Final,N) :-
        N>0,
        arg(N,Estado_Final,Var_A_Buscar),
        contiene_var(Estado_Actual, Var_A_Buscar),
        N1 is N -1,
        contiene_rec(Estado_Actual, Estado_Final,N1).

contiene_var(Estado_Actual,Var_A_Buscar):-
        functor(Estado_Actual,regs, Longitud),
        contiene_var_rec(Estado_Actual,Var_A_Buscar,Longitud).


contiene_var_rec(Estado_Actual, Var, N) :-
        N>0,
        arg(N,Estado_Actual,Var_A_Comparar),
        no_son_elementos_iguales(Var_A_Comparar,Var),
        N1 is N - 1,
        contiene_var_rec(Estado_Actual,Var,N1).

contiene_var_rec(Estado_Actual, Var, N) :-
        %        N>0,
        arg(N,Estado_Actual,Var_A_Comparar),
        son_elementos_iguales(Var_A_Comparar,Var).

%contiene_var_rec(_,_,_)

no_son_elementos_iguales(X,Y):-
        son_elementos_iguales(X,Y), !,fail.

no_son_elementos_iguales(_,_).

%generador_de_codigo(Estado_Inicial, Estado_Inicial, []).
%generador_de_codigo(Estado_Inicial, Estado_Final, Lista_Instrucciones) :-
%        Estado_Inicial \= Estado_Final,
%         functor(Estado_Inicial, regs, Numero_Registros),
%         functor(Estado_Final, regs, Numero_Registros),
%         lista_de_swaps(Numero_Registros, Lista_Swaps),
%         lista_de_moves(Numero_Registros, Lista_Moves),
%         append(Lista_Swaps, Lista_Moves, Lista_Pasos),
%         generar_estados_siguientes(Estado_Inicial, Lista_Pasos, Estados_Siguientes).
        %

% Devuelve una lista de todos los camino(paso, estado) siguientes
generar_estados_siguientes(_, _, _, [], []).
generar_estados_siguientes(Estado_A_Cambiar, Estados_Anteriores, Pasos_Anteriores, [Paso|Pasos], [Siguiente|Siguientes]) :-
        aplicar_paso(Estado_A_Cambiar, Paso, Estado_Siguiente),
        append(Estados_Anteriores, [Estado_A_Cambiar], Estados_Anteriores2),
        append(Pasos_Anteriores, [Paso], Pasos_Que_Llevo),
        Siguiente = camino(Estados_Anteriores2, Pasos_Que_Llevo, Estado_Siguiente),
        generar_estados_siguientes(Estado_A_Cambiar, Estados_Anteriores, Pasos_Anteriores, Pasos, Siguientes),
        !.

% lista_de_moves(N, Lista)
%
% Lista es la lista de todas las instrucciones "move" que se pueden realizar
% sobre N registros
lista_de_moves(1, [move(1)]).
lista_de_moves(N, [Move|Lista]) :-
        N > 1,
        Move = move(N),
        N1 is N - 1,
        lista_de_moves(N1, Lista).

% lista_de_swaps(N, Lista)
%
% "Lista" es la lista de todas las instrucciones "swap(i,j)" que se pueden
% realizar sobre "N" registros tales que tales que j>i
lista_de_swaps(2, [swap(2,1)]).
lista_de_swaps(N, Lista) :-
        N > 2,
        N1 is N - 1,
        lista_de_subswaps(N, N1, Lista2),
        lista_de_swaps(N1, Lista3),
        append(Lista2, Lista3, Lista).

lista_de_subswaps(N, 1, [swap(N, 1)]).
lista_de_subswaps(N, M, [Swap|Lista]) :-
        M > 1,
        Swap = swap(N,M),
        M1 is M - 1,
        lista_de_subswaps(N, M1, Lista).


% aplicar_paso(Estado_Inicial, Paso, Estado_Final)
%
% La transición de Estado_Inicial a Estado_Final se realiza por la instrucción
% Paso.
aplicar_paso(Estado_Inicial, move(N), Estado_Final) :-
        hacer_move(Estado_Inicial, N, Estado_Final).

aplicar_paso(Estado_Inicial, swap(N,M), Estado_Final) :-
        hacer_swap(Estado_Inicial, N, M, Estado_Final).


% hacer_move(Estado_Inicial, N, Estado_Final)
%
% Estado_Final es el estado al que se llega tras ejecutar la instrucción
% "move(N)" sobre el Estado_Inicial
hacer_move(Estado_Inicial, N, Estado_Final) :-
        functor(Estado_Inicial, regs, Numero_Registros),
        functor(Estado_Final, regs, Numero_Registros),
        hacer_move_aux(Estado_Inicial, N, Numero_Registros, Estado_Final).

hacer_move_aux(_, _, 0, _).

hacer_move_aux(Estado_Inicial, N, 1, Estado_Final) :-
        % N es el último
        functor(Estado_Inicial, regs, N),
        arg(N, Estado_Inicial, X),
        arg(1, Estado_Final, X).

hacer_move_aux(Estado_Inicial, N, I, Estado_Final) :-
        I > 0,
        % N no es el último
        functor(Estado_Inicial, regs, Longitud),
        N =\= Longitud,
        % I es N + 1
        I is N + 1,
        arg(N, Estado_Inicial, X),
        arg(I, Estado_Final, X),
        % avanzar
        I2 is I - 1,
        hacer_move_aux(Estado_Inicial, N, I2, Estado_Final).


hacer_move_aux(Estado_Inicial, N, I, Estado_Final) :-
        I > 0,
        % Si N no es el último, entrar,
        functor(Estado_Inicial, regs, Longitud),
        N =\= Longitud,
        %
        N1 is N + 1,
        I =\= N1,
        arg(I, Estado_Inicial, X),
        arg(I, Estado_Final, X),
        % avanzar
        I2 is I - 1,
        hacer_move_aux(Estado_Inicial, N, I2, Estado_Final),
        !.

hacer_move_aux(Estado_Inicial, N, I, Estado_Final) :-
        I > 0,
        % Si I no es 1, entrar,
        I \= 1,
        %
        N1 is N + 1,
        I =\= N1,
        arg(I, Estado_Inicial, X),
        arg(I, Estado_Final, X),
        % avanzar
        I2 is I - 1,
        hacer_move_aux(Estado_Inicial, N, I2, Estado_Final),
        !.

hacer_swap(Estado_Inicial, N, M, Estado_Final) :-
        functor(Estado_Inicial, regs, Numero_Registros),
        functor(Estado_Final, regs, Numero_Registros),
        hacer_swap_aux(Estado_Inicial, N, M, Numero_Registros, Estado_Final).

hacer_swap_aux(_, _, _, 0, _).

hacer_swap_aux(Estado_Inicial, I, M, I, Estado_Final) :-
        I > 0,
        arg(I, Estado_Inicial, X),
        arg(M, Estado_Final, X),
        I2 is I - 1,
        hacer_swap_aux(Estado_Inicial, I, M, I2, Estado_Final).

hacer_swap_aux(Estado_Inicial, N, I, I, Estado_Final) :-
        I > 0,
        arg(I, Estado_Inicial, X),
        arg(N, Estado_Final, X),
        I2 is I - 1,
        hacer_swap_aux(Estado_Inicial, N, I, I2, Estado_Final).

hacer_swap_aux(Estado_Inicial, N, M, I, Estado_Final) :-
        I > 0,
        N =\= I,
        M =\= I,
        arg(I, Estado_Inicial, X),
        arg(I, Estado_Final, X),
        I2 is I - 1,
        hacer_swap_aux(Estado_Inicial, N, M, I2, Estado_Final).
