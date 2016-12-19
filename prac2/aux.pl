:- use_module(pract2, [lista_de_moves/2]).
:- use_module(pract2, [lista_de_swaps/2]).
:- use_module(pract2, [hacer_move/3]).
:- use_module(pract2, [hacer_swap/4]).
:- use_module(pract2, [generar_estados_siguientes/5]).

% Prueba lista_de_swaps, lista_de_moves, append y generar_estados_siguientes
generador_de_codigo(Estado_Inicial, Estado_Final, Lista_Instrucciones) :-
        functor(Estado_Inicial, regs, Numero_Registros),
        lista_de_swaps(Numero_Registros, Lista_Swaps),
        lista_de_moves(Numero_Registros, Lista_Moves),
        append(Lista_Swaps, Lista_Moves, Lista_Pasos),
        % Auxiliar empieza aquí
        generar_estados_siguientes(Estado_Inicial, [], [], Lista_Pasos, Caminos_Siguientes),
        % Caminos_Siguientes = C1, C2, C3
        % Cojo el primer "Caminos_Siguientes"  C1
        holaaaa(Caminos_Siguientes, Lista_Pasos, Estado_Final, Lista_Instrucciones).

holaaaa(_,_,_,Lista_Instrucciones) :-
        ground(Lista_Instrucciones).

holaaaa([C1|Siguientesss], Lista_Pasos, Estado_Final, Lista_Instrucciones) :-
        % Compruebo si es el estado final
        comprobar_fin(C1, Lista_Pasos, Estado_Final, Lista_Instrucciones, Siguientesss).

comprobar_fin(camino(_, Pasos, Siguiente), _,  Siguiente, Pasos, _) :-
        !.

comprobar_fin(camino(_, Pasos, Siguiente), _, Estado_Final, Pasos, _) :-
        son_iguales(Siguiente, Estado_Final),
        !.

comprobar_fin(C1, Lista_Pasos, Estado_Final, Lista_Instrucciones, Siguientesss) :-
        % Obtengo todos los siguientes de C1    => Caminos_Siguientes_A_C1 = [C4, C5, C6...]
        camino(EA1, P1, E1) = C1,
        generar_estados_siguientes(E1, EA1, P1, Lista_Pasos, Caminos_Siguientes_A_C1),
        descartar_caminos(Caminos_Siguientes_A_C1, Estado_Final, Caminos_Sin_Descartar),
        % Append(Caminos_Siguientes, Caminos_Siguientes_A_C1, Caminosssss) Caminossss = [C1, C2, C3, C4...]
        append(Siguientesss, Caminos_Sin_Descartar, Caminossss),
        % Recursividad quitando el primero de Caminossss
        holaaaa(Caminossss, Lista_Pasos, Estado_Final, Lista_Instrucciones).

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



% (a, a, b, c)   -> 0 pasos
% => move 1
% (a, a, b, c)   -> 1 paso


ejemplo_caminos(CSD) :-
        descartar_ciclos([camino([regs(1,2,3)], [move(1)], regs(1,1,3)),
                          camino([regs(1,2,3), regs(3,2,1)], [swap(1,3), swap(3,1)], regs(1,2,3))],
                          CSD).

ejemplo_chungo(L) :-
        generador_de_codigo(regs(1,2,3,4,5), regs(1,2,2,1,1), L).