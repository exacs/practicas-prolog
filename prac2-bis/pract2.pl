:- module(_,_).
alumno_prode('Sabina', 'Hidalgo', 'Irene', 'v130321').
alumno_prode('Saito', 'Murata', 'Carlos', 'v130215').
alumno_prode('Vargas', 'Azpitarte', 'Daniel', 'v130290').

generador_de_codigo(EstadoInicial, EstadoFinal, ListaInstrucciones) :-
        functor(EstadoInicial, regs, Longitud),
        functor(EstadoFinal, regs, Longitud),
        generar_posibles(Posibles, Longitud),
        length(ListaInstrucciones, _L),
        comprobar_posibles(ListaInstrucciones, Longitud, EstadoInicial, EstadoFinal, Posibles),
        !.

generar_posibles(PosiblesCodificados, Longitud) :-
        lista_de_moves(Longitud, Moves),
        lista_de_swaps(Longitud, Swaps),
        append(Moves, Swaps, Posibles),
        codificar(Posibles, PosiblesCodificados, 1).

% lista_de_moves(N, Lista)
%
% Unifica si "Lista" es la lista de todas las instrucciones "move(i)" que se pueden
% realizar sobre "N" registros
lista_de_moves(1, [move(1)]).
lista_de_moves(N, [Move|Lista]) :-
        N > 1,
        Move = move(N),
        N1 is N - 1,
        lista_de_moves(N1, Lista).

% lista_de_swaps(N, Lista)
%
% Unifica si "Lista" es la lista de todas las instrucciones "swap(i,j)" que se pueden
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

% codificar(LSC, LC, N)
%
% Unifica si LC es una lista en la que el I-ésimo elemento es "I-" concatenado
% con el I-ésimo elemento de la lista LSC.
codificar([], [], _).
codificar([SinCodificar|ListaSinCodificar], [Codificada|ListaCodificada], N) :-
        Codificada = N-SinCodificar,
        N1 is N + 1,
        codificar(ListaSinCodificar, ListaCodificada, N1).

% comprobar_posibles(Instrucciones, Longitud, EstadoActual, EstadoFinal, Posibles)
%
% Unifica si Instrucciones es una lista de instrucciones que hay que ejecutar
% para llegar desde EstadoActual hasta EstadoFinal.
%
% Todos los elementos de Instrucciones pertenecen a la lista de Posibles.
%
% Longitud es el número de argumentos de los functores EstadoActual y EstadoFinal
comprobar_posibles([], _, EstadoFinal, EstadoFinal, _).
comprobar_posibles([], _, EstadoActual, EstadoFinal, _) :-
        son_iguales(EstadoActual, EstadoFinal).
comprobar_posibles([Instruccion|ListaInstrucciones], Longitud, EstadoActual, EstadoFinal, Posibles) :-
        length(Posibles, Longitud_Posibles),
        generar_numero(Longitud_Posibles, Codigo),
        seleccionar_instruccion(Codigo, Instruccion, Posibles),
        transitar(Instruccion, EstadoActual, EstadoSiguiente, Longitud),
        comprobar_posibles(ListaInstrucciones, Longitud, EstadoSiguiente, EstadoFinal, Posibles).

% son_iguales(A, B)
%
% Unifica si el functor A es igual al functor B, considerando que * es igual
% a cualquier elemento
son_iguales(A, B) :-
        functor(A, _, Longitud),
        son_iguales_rec(A,B,Longitud).

% son_iguales_rec(A, B, I)
%
% Unifica si el argumento I del functor A es igual al argumento I del functor B,
% considerando que * es igual a cualquier elemento
son_iguales_rec(_,_,0).
son_iguales_rec(A,B,I) :-
        I > 0,
        arg(I, A, ElementoA),
        arg(I, B, ElementoB),
        son_elementos_iguales(ElementoA, ElementoB),
        I2 is I - 1,
        son_iguales_rec(A,B, I2).

% son_elementos_iguales(A, B)
%
% Unifica si A es igual a B,
% considerando que * es igual a cualquier elemento
son_elementos_iguales(X,X).
son_elementos_iguales(*,_).
son_elementos_iguales(_,*).


% generar_numero(Max, Generado)
%
% Unifica si Max es mayor o igual que Generado
generar_numero(Max, Max).
generar_numero(Max, Generado) :-
        Max > 1,
        Max1 is Max - 1,
        generar_numero(Max1, Generado).

% seleccionar_instruccion(Codigo, Instruccion, Posibles)
%
% Unifica si Instrucción es la instrucción en la lista de Posibles,
% correspondiente al código Codigo
seleccionar_instruccion(Codigo, Instruccion, Posibles) :-
        member(Codigo-Instruccion, Posibles),
        !.
seleccionar_instruccion(_, _, []).

% transitar(Instruccion, EstadoActual, EstadoSiguiente, Longitud)
%
% Unifica si:
% - EstadoSiguiente es un functor "regs" con Longitud argumentos resultante
%   de aplicar Instruccion sobre EstadoActual
transitar(Instruccion, EstadoActual, EstadoSiguiente, Longitud) :-
        functor(EstadoSiguiente, regs, Longitud),
        cambian(Instruccion, EstadoActual, EstadoSiguiente),
        iguales(Longitud, EstadoActual, EstadoSiguiente).

% cambian(Instruccion, EstadoActual, EstadoSiguiente).
%
% Realiza la Instrucción.
% Unifica si EstadoFinal unifica con el resultado de aplicar Instrucción sobre
% EstadoActual
cambian(swap(N, M), EstadoActual, EstadoSiguiente) :-
        cambian_por_swap(N, M, EstadoActual, EstadoSiguiente).
cambian(move(N), EstadoActual, EstadoSiguiente) :-
        cambian_por_move(N, EstadoActual, EstadoSiguiente).

% cambian_por_swap(N, M, EstadoActual, EstadoSiguiente)
%
% Unifica si al aplicar swap(N,M) en EstadoActual se obtiene EstadoSiguiente
cambian_por_swap(N, M, EstadoActual, EstadoSiguiente) :-
        arg(N, EstadoActual, RegistroN),
        arg(M, EstadoActual, RegistroM),
        arg(M, EstadoSiguiente, RegistroN),
        arg(N, EstadoSiguiente, RegistroM).

% cambian_por_move(N, EstadoActual, EstadoSiguiente)
%
% Unifica si al aplicar move(N) en EstadoActual se obtiene EstadoSiguiente
cambian_por_move(N, EstadoActual, EstadoSiguiente) :-
        functor(EstadoActual, _, Longitud),
        Longitud \= N,
        arg(N, EstadoActual, Registro),
        N1 is N + 1,
        arg(N1, EstadoSiguiente, Registro).

cambian_por_move(N, EstadoActual, EstadoSiguiente) :-
        functor(EstadoActual, _, N),
        arg(N, EstadoActual, Registro),
        arg(1, EstadoSiguiente, Registro).

% iguales(N, EstadoActual, EstadoFinal)
%
% Unifica si en todos los N-ésimos argumentos del
% functor EstadoFinal que sean variables libres, esas variables unifican con el
% valor del N-ésimo argumento de EstadoActual.
iguales(0, _, _).
iguales(N, EstadoActual, EstadoFinal) :-
        N > 0,
        completar_estado(N, EstadoActual, EstadoFinal),
        N1 is N - 1,
        iguales(N1, EstadoActual, EstadoFinal).

% completar_estado(N, EstadoActual, EstadoFinal)
%
% Unifica si EstadoFinal es un functor cuyo N-ésimo argumento
% es una variable libre, esta adquiere el valor del N-ésimo argumento del
% EstadoActual
completar_estado(N, EstadoActual, EstadoFinal) :-
        arg(N, EstadoFinal, Registro),
        var(Registro),
        !,
        arg(N, EstadoActual, Registro).

completar_estado(_, _, _).
