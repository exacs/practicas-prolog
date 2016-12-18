:- use_module(pract2, [lista_de_moves/2]).
:- use_module(pract2, [lista_de_swaps/2]).
:- use_module(pract2, [hacer_move/3]).
:- use_module(pract2, [hacer_swap/4]).
:- use_module(pract2, [generar_estados_siguientes/3]).

% Prueba lista_de_swaps, lista_de_moves, append y generar_estados_siguientes
generador_de_codigo(Estado_Inicial, Estado_Final, Lista_Instrucciones) :-
        functor(Estado_Inicial, regs, Numero_Registros),
        lista_de_swaps(Numero_Registros, Lista_Swaps),
        lista_de_moves(Numero_Registros, Lista_Moves),
        append(Lista_Swaps, Lista_Moves, Lista_Pasos),
        auxiliar(Estado_Inicial, Estado_Final, Lista_Pasos, Lista_Instrucciones).

%
auxiliar(Estado_Inicial, Estado_Final, Lista_Pasos, Lista_Instrucciones) :-
        generar_estados_siguientes(Estado_Inicial, Lista_Pasos, Caminos_Siguientes),
        % Caminos_Siguientes = C1, C2, C3
        % Cojo el primer "Caminos_Siguientes"  C1
        holaaaa(Caminos_Siguientes, Lista_Pasos, Estado_Final, Lista_Instrucciones).

holaaaa(_,_,_,Lista_Instrucciones) :-
        ground(Lista_Instrucciones).

holaaaa([C1|Siguientesss], Lista_Pasos, Estado_Final, Lista_Instrucciones) :-
        % Compruebo si es el estado final
        comprobar_fin(C1, Lista_Pasos, Estado_Final, Lista_Instrucciones, Siguientesss).

comprobar_fin(camino(Pasos, Siguiente), _,  Siguiente, Pasos, _).

comprobar_fin(C1, Lista_Pasos, Estado_Final, Lista_Instrucciones, Siguientesss) :-
        % Obtengo todos los siguientes de C1    => Caminos_Siguientes_A_C1 = [C4, C5, C6...]
        camino(_,E1) = C1,
        generar_estados_siguientes(E1, Lista_Pasos, Caminos_Siguientes_A_C1),
        % Append(Caminos_Siguientes, Caminos_Siguientes_A_C1, Caminosssss) Caminossss = [C1, C2, C3, C4...]
        append(Siguientesss, Caminos_Siguientes_A_C1, Caminossss),
        % Recursividad quitando el primero de Caminossss
        holaaaa(Caminossss, Lista_Pasos, Estado_Final, Lista_Instrucciones).

descartar_estados(Estados_Siguientes, Estados_Siguientes).

% (a, a, b, c)   -> 0 pasos
% => move 1
% (a, a, b, c)   -> 1 paso