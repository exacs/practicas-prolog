:- module(pract2, [lista_de_moves/2,
        lista_de_swaps/2,
        hacer_move/3,
        hacer_swap/4]).

alumno_prode('Sabina', 'Hidalgo', 'Irene', 'v130321').
alumno_prode('Saito', 'Murata', 'Carlos', 'v130215').
alumno_prode('Vargas', 'Azpitarte', 'Daniel', 'v130290').

% generador_de_codigo(regs(1,2,3,4), lista(2,2,2,2,5)

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


lista_de_moves(1, [move(1)]).
lista_de_moves(N, [Move|Lista]) :-
        N > 1,
        Move = move(N),
        N1 is N - 1,
        lista_de_moves(N1, Lista).

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


% aplicar_paso(regs(1,2,3,4), move(1), Estado_Final)
aplicar_paso(Estado_Inicial, move(N), Estado_Final) :-
        hacer_move(Estado_Inicial, N, Estado_Final).

aplicar_paso(Estado_Inicial, swap(N,M), Estado_Final) :-
        hacer_swap(Estado_Inicial, N, M, Estado_Final).


% hacer_move(regs(1,2,3,4), 4, regs(4,2,3,4))
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

% Si N no es el último o I no es 1, entrar


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

% %
% lista_de_swaps(1, L)
% L = []
%
% lista_de_swaps(2, L)
% L = (1,2)
%
% lista_de_swaps(3, L)
% L = (1,2), (1,3), (2,3)
%
% lista_de_swaps(4, L)
% L = (1,2), (1,3), (1,4), (2,3), (2,4), (3,4)
% L = (4,3), (4,2), (4,1), (3,2), (3,1), (2,1)
%
% lista_de_subswaps(4, L)
% L = (4,3), (4,2), (4,1)
