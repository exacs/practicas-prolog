alumno_prode('Sabina', 'Hidalgo', 'Irene', 'v130321').
alumno_prode('Saito', 'Murata', 'Carlos', 'v130215').
alumno_prode('Vargas', 'Azpitarte', 'Daniel', 'v130290').

%
% EJERCICIO 1
% Run-length encoding modificado
%
% comprimir(L0, LC), L0 y LC son listas, tales que LC es L0 comprimida.
% - L0 es una lista de constantes
% - LC es una lista de tuplas rlec(S, C) donde S es un símbolo que aparece en L0
%   y C el número de veces consecutivas en las que aparece; si C es 1, en lugar
%   de la tupla, aparece solamente S.
comprimir([], []).
comprimir([X|R], LC) :-
        contar([X|R], X, L1),
        singularizar(L1, LC).

%
% contar(L0, a, LC), L0 y LC son listas:
% - L0 es una lista de constantes
% - LC es una lista de tuplas rlec(S, C) donde S es un símbolo que aparece en L0
%   y C el número de veces consecutivas en las que aparece.
% - a es el primer elemento de L0
%
contar([], _, []).
contar([], X, [rlec(X, 0)]).
contar([X|Xr], X, [rlec(X, s(C)) | Yr]) :-
        contar(Xr, X, [rlec(X, C) | Yr]).

contar([X|Xr], Y, [rlec(Y, 0), rlec(X, Nx) | Zr]) :-
        X \= Y,
        contar([X|Xr], X, [rlec(X, Nx) | Zr]).

%
% singularizar(L0, LC), son listas:
% - L0 es una lista de tuplas rlec(S, C).
% - LC es una lista del mismo tamaño que L0, en la que el i-ésimo elemento de LC es
%   - S, solo si el i-ésimo elemento de L0 es rlec(S, s(0))
%   - Igual al i-ésimo elemento de L0 en caso contrario.
singularizar([], []).
singularizar([rlec(X, s(0))|Xr], [X|Yr]) :-
        X \= rlec(_, _),
        singularizar(Xr, Yr).
singularizar([rlec(X, s(N))|Xr], [rlec(X, s(N))|Yr]) :-
        N \= 0,
        singularizar(Xr, Yr).

%
% descomprimir(L0, LC), realiza la operación contraria de comprimir.
% es decir: comprimir(L0, LC) equivale a descomprimir(LC, L0)
descomprimir([], []).
descomprimir(L0, [X|R]) :-
        singularizar(L1, L0),
        contar([X|R], X, L1).


%
% EJERCICIO 2
% Exploración de árboles n-arios
%

%
% nat(N), N es un número natural
nat(0).
nat(s(X)) :- nat(X).

%
% menor_igual(X, Y), en donde X es menor o igual que Y
menor_igual(0, X) :- nat(X).
menor_igual(s(X), s(Y)) :- menor_igual(X, Y).

%
% menores(Arbol, Max), cierto si Max es mayor o igual que todos los elementos
% de Arbol
menores(hoja(X), Max) :-
        menor_igual(X, Max).

menores(nodo(X, [Y|Yr]), Max) :-
        menor_igual(X, Max),
        menores([Y|Yr], Max).

menores([], _).

menores([X|Xr], Max) :-
        menores(X, Max),
        menores(Xr, Max).

%
% plus(X, Y, Z), en donde X + Y = Z
plus(0,X,X) :- nat(X).
plus(s(X),Y,s(Z)) :- plus(X,Y,Z).

%
% suma(Arbol, Suma), en donde Suma es la suma de todos los elementos de Arbol
suma(hoja(X), Suma) :-
        plus(0, X, Suma).

suma(nodo(X, [Y|Yr]), Suma) :-
        suma([Y|Yr], Hijos),
        plus(Hijos, X, Suma).

suma([], 0).

suma([X|Xr], Suma) :-
        suma(X, SumaNodo),
        suma(Xr, SumaResto),
        plus(SumaNodo, SumaResto, Suma).
