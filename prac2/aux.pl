:- use_module(pract2, [lista_de_moves/2]).
:- use_module(pract2, [lista_de_swaps/2]).
:- use_module(pract2, [hacer_move/3]).
:- use_module(pract2, [hacer_swap/4]).

% Prueba lista_de_swaps, lista_de_moves, append y generar_estados_siguientes
auxiliar(Estado_Inicial, Estados_Siguientes) :-
        functor(Estado_Inicial, regs, Numero_Registros),
        lista_de_swaps(Numero_Registros, Lista_Swaps),
        lista_de_moves(Numero_Registros, Lista_Moves),
        append(Lista_Swaps, Lista_Moves, Lista_Pasos),
        generar_estados_siguientes(Estado_Inicial, Lista_Pasos, Estados_Siguientes).

% Devuelve una lista de todos los estados siguientes
generar_estados_siguientes(_, [], []).
generar_estados_siguientes(Estado_Inicial, [Paso|Pasos], [Siguiente|Estados_Siguientes]) :-
        aplicar_paso(Estado_Inicial, Paso, Siguiente),
        generar_estados_siguientes(Estado_Inicial, Pasos, Estados_Siguientes).

% Aplica un paso (hacer_move o hacer_swap)
aplicar_paso(Estado_Inicial, move(N), Estado_Final) :-
        hacer_move(Estado_Inicial, N, Estado_Final).
aplicar_paso(Estado_Inicial, swap(N,M), Estado_Final) :-
        hacer_swap(Estado_Inicial, N, M, Estado_Final).
