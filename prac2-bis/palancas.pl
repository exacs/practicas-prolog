:- module(_,_).

abrir(EIni, EFin, MovAuto, LMov) :-
        functor(EIni, palancas, NPal),
        functor(EFin, palancas, NPal),
        length(LMov, _Longitud),
        comprobar_LMov(LMov, NPal, EIni, EFin, MovAuto),
        !.

comprobar_LMov([], _, EFin, EFin, _).
comprobar_LMov([Pal|LPal], NPal, EAct, EFin, MovAuto) :-
        seleccionar_pal(NPal, Pal),
        mover_pal(NPal, EAct, Pal, MovAuto, ESig),
        comprobar_LMov(LPal, NPal, ESig, EFin, MovAuto).

seleccionar_pal(NPal, NPal).
seleccionar_pal(NPal, Pal) :-
        NPal > 1,
        N1 is NPal - 1,
        seleccionar_pal(N1, Pal).

mover_pal(NPal, EAct, Pal, MovAuto, ESig) :-
        functor(ESig, palancas, NPal),
        mov_auto(Pal, MovAuto, PalMovidas),
        cambian([Pal|PalMovidas], EAct, ESig),
        iguales(NPal, EAct, ESig).
%%%%%%%%%%
mov_auto(Pal, MovAuto, PalMovidas) :-
        member(Pal-PalMovidas, MovAuto),
        !.
mov_auto(_, _, []).

flip(on, off).
flip(off, on).

cambian([], _, _).
cambian([Pal|LPal], EAct, ESig) :-
        arg(Pal, EAct, EPal),
        arg(Pal, ESig, NuevoEPal),
        flip(EPal, NuevoEPal),
        cambian(LPal, EAct, ESig).

completar_estado(Pal, EAct, EFin) :-
        arg(Pal, EFin, EPal),
        var(EPal),
        !,
        arg(Pal, EAct, EPal).
completar_estado(_, _, _).

iguales(0, _, _).
iguales(NPal, EAct, EFin) :-
        NPal > 0,
        completar_estado(NPal, EAct, EFin),
        NuevoNPal is NPal - 1,
        iguales(NuevoNPal, EAct, EFin).

ejemplo(S) :-
        abrir(palancas(on,on,off,off,on,off,on,off),palancas(off,on,on,off,off,on,on,off),[1-[2,3],2-[3,4],3-[4,5],4-[5,6],5-[6,7],6-[7,1],7-[1,2]],S).


%    1 1 0 0 1 0 1 0
%    % 6-[7-1]
%    0 1 0 0 1 1 0 0
%    % 5-[6-7]
%    0 1 0 0 0 0 1 0
%    % 4-[6-5]
%    0 1 0 1 1 1 1 0
%    % 3-[4-5]
%    0 1 1 0 0 1 1 0
%    0 1 1 0 0 1 1 0
