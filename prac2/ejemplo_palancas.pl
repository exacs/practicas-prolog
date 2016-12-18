flip(on,off).
flip(off,on).

palancas_que_cambian([],_,_).
palancas_que_cambian([Palanca|Palancas],Estado_Actual,Nuevo_Estado) :-
        arg(Palanca,Estado_Actual,Estado_Palanca),
        arg(Palanca,Nuevo_Estado,Nuevo_Estado_Palanca),
        flip(Estado_Palanca,Nuevo_Estado_Palanca),
        palancas_que_cambian(Palancas,Estado_Actual,Nuevo_Estado).

completar_estado_palanca_ID_Palanca(ID_Palanca,Estado_Actual,Estado_Final) :-
        arg(ID_Palanca,Estado_Final,Estado_Palanca),
        var(Estado_Palanca),
        !,
        arg(ID_Palanca,Estado_Actual,Estado_Palanca).
completar_estado_palanca_ID_Palanca(_,_,_).

palancas_que_no_cambian(0,_,_).
palancas_que_no_cambian(Numero_Palancas,Estado_Actual,Estado_Final) :-
        Numero_Palancas > 0,
        completar_estado_palanca_ID_Palanca(Numero_Palancas,Estado_Actual,Estado_Final),
        Nuevo_Numero_Palancas is Numero_Palancas + 1,
        palancas_que_no_cambian(Nuevo_Numero_Palancas,Estado_Actual,Estado_Final).

abrir(Estado_Inicial,Estado_Final,Movimientos_Automaticos,Lista_Movimientos) :-
        functor(Estado_Inicial,palancas,Numero_Palancas),
        functor(Estado_Final,palancas,Numero_Palancas),
        length(Lista_Movimientos,_Longitud_Lista_Movimientos),
        comprobar_lista_movimientos(Lista_Movimientos,Numero_Palancas,Estado_Inicial,Estado_Final,Movimientos_Automaticos).

comprobar_lista_movimientos([],_,Estado_Final,Estado_Final,_).
comprobar_lista_movimientos([Palanca|Palancas],Numero_Palancas,Estado_Actual,Estado_Final,Movimientos_Automaticos) :-
        seleccionar_palanca(Numero_Palancas,Palanca),
        mover_palanca(Numero_Palancas,Estado_Actual,Palanca,Movimientos_Automaticos,Estado_Siguiente),
        comprobar_lista_movimientos(Palancas,Numero_Palancas,Estado_Siguiente,Estado_Final,Movimientos_Automaticos).

seleccionar_palanca(Numero_Palancas,Numero_Palancas).
seleccionar_palanca(Numero_Palancas,ID_Palanca_A_Mover) :-
        Numero_Palancas > 1,
        N1 is Numero_Palancas-1,
        seleccionar_palanca(N1,ID_Palanca_A_Mover).

mover_palanca(Numero_Palancas,Estado_Actual,ID_Palanca_A_Mover,Movimientos_Automaticos,Nuevo_Estado) :-
        functor(Nuevo_Estado,palancas,Numero_Palancas),
        movimientos_automaticos(ID_Palanca_A_Mover,Movimientos_Automaticos,Palancas_Movidas_Automaticamente),
        palancas_que_cambian([ID_Palanca_A_Mover|Palancas_Movidas_Automaticamente],Estado_Actual,Nuevo_Estado),
        palancas_que_no_cambian(Numero_Palancas,Estado_Actual,Nuevo_Estado).

movimientos_automaticos(ID_Palanca_A_Mover,Movimientos_Automaticos,Palancas_Movidas_Automaticamente) :-
        member(ID_Palanca_A_Mover - Palancas_Movidas_Automaticamente,Movimientos_Automaticos),
        !.

movimientos_automaticos(_,_,[]).

ejemplo(S) :- abrir(
        palancas(on, on, off, off, on, off, on, off),
        palancas(off, on, on, off, off, on, on, off),
        [1-[2,3], 2-[3,4], 3-[4,5], 4-[5,6], 5-[6,7], 6-[7,1], 7-[1,2]],
         S).
