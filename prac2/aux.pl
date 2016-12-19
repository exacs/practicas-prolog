:- use_module(pract2, [lista_de_moves/2]).
:- use_module(pract2, [lista_de_swaps/2]).
:- use_module(pract2, [hacer_move/3]).
:- use_module(pract2, [hacer_swap/4]).
:- use_module(pract2, [generar_estados_siguientes/4]).

% Prueba lista_de_swaps, lista_de_moves, append y generar_estados_siguientes
generador_de_codigo(Estado_Inicial, Estado_Final, Lista_Instrucciones) :-
        functor(Estado_Inicial, regs, Numero_Registros),
        lista_de_swaps(Numero_Registros, Lista_Swaps),
        lista_de_moves(Numero_Registros, Lista_Moves),
        append(Lista_Swaps, Lista_Moves, Lista_Pasos),
        % Auxiliar empieza aquí
        generar_estados_siguientes(Estado_Inicial, [], Lista_Pasos, Caminos_Siguientes),
        % Caminos_Siguientes = C1, C2, C3
        % Cojo el primer "Caminos_Siguientes"  C1
        holaaaa(Caminos_Siguientes, Lista_Pasos, Estado_Final, Lista_Instrucciones).

holaaaa(_,_,_,Lista_Instrucciones) :-
        ground(Lista_Instrucciones).

holaaaa([C1|Siguientesss], Lista_Pasos, Estado_Final, Lista_Instrucciones) :-
        % Compruebo si es el estado final
        comprobar_fin(C1, Lista_Pasos, Estado_Final, Lista_Instrucciones, Siguientesss).

comprobar_fin(camino(Pasos, Siguiente), _,  Siguiente, Pasos, _) :-
        !.

comprobar_fin(camino(Pasos, Siguiente), _, Estado_Final, Pasos, _) :-
        son_iguales(Siguiente, Estado_Final),
        !.

comprobar_fin(C1, Lista_Pasos, Estado_Final, Lista_Instrucciones, Siguientesss) :-
        % Obtengo todos los siguientes de C1    => Caminos_Siguientes_A_C1 = [C4, C5, C6...]
        camino(EA,E1) = C1,
        generar_estados_siguientes(E1, EA, Lista_Pasos, Caminos_Siguientes_A_C1),
        descartar_caminos(Caminos_Siguientes_A_C1, Caminos_Sin_Descartar),
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


descartar_caminos(Caminos_Originales, Caminos_Restantes).

% (a, a, b, c)   -> 0 pasos
% => move 1
% (a, a, b, c)   -> 1 paso

ejemplo_chungo(L) :-
        generador_de_codigo(regs(1,2,3,4,5), regs(1,2,2,1,1), L).