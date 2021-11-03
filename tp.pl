:-use_rendering(table).

/*Ejemplos de Ejecucion*/

tableroEjemplo(T):-
    T =  [[-,-,-,b,-,-,-],
          [-,-,-,a,-,-,-],
          [-,-,-,a,a,-,-],
          [a,-,b,a,b,b,-],
          [b,a,a,b,a,b,-],
          [a,a,b,b,a,b,-]].


/*============EJERCICIO 1===============*/

tableroInicial(T):-
    T = [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-]].

/*===========EJERCICIO 2================*/

/*haya una ficha abajo - remplazamos*/
ingresarFicha([F1, F2|Fs], Columna, Ficha, [T1, T2|Ts]):- 
    posicion(F1, Columna, Ret1), Ret1='-',
    posicion(F2, Columna, Ret2), Ret2\='-',!,
    T2 = F2, Ts = Fs,
    remplazar(F1, Columna, Ficha, T1).
/*no haya ficha - bajamos - evaluamos again*/
ingresarFicha([F1, F2|[Fs1 | Fss]], Columna, Ficha, [T1, T2|[Ts1|Tss]]):-
    posicion(F1, Columna, Ret1), Ret1='-',
    posicion(F2, Columna, Ret2), Ret2='-',
    T1 = F1,
    ingresarFicha([F2, Fs1|Fss], Columna, Ficha, [T2, Ts1|Tss]).
/*estamos es la ultima fila - Remplazamos*/
ingresarFicha([F1, F2|[]], Columna, Ficha, [T1, T2|[]]):-
    posicion(F1, Columna, Ret1), Ret1='-',
    posicion(F2, Columna, Ret2), Ret2='-',
    T1 = F1,
    remplazar(F2, Columna, Ficha, T2).

/*==================EJERCICIO 3======================*/

columnaDisp(Tablero, Columna):-
    columna(Columna),
    ingresarFicha(Tablero, Columna, _, _).


/*==================EJERCICIO 4======================*/

contenido([T|Ts],[Y, X],Ficha):-
    longitud(Ts, L), 6 is L+Y,!, posicion(T, X, Ficha).

contenido([_T|Ts],[Y, X],Ficha):-
    longitud(Ts, L), not(6 is L+Y),
    contenido(Ts,[Y, X],Ficha).

/*==================EJERCICIO 5======================*/

conecta4(Tablero,Ficha,Retorno):- columna(X), fila(Y), ficha(Ficha),
    direccion(Horizontal, Vertical),
	contenido(Tablero, [Y, X], Ficha),
    contenido(Tablero, [Y+(Vertical), X+(Horizontal)], Ficha),
    contenido(Tablero, [Y+(2*Vertical),X+(2*Horizontal)], Ficha),
    contenido(Tablero, [Y+(3*Vertical), X+(3*Horizontal)], Ficha),!,
    R11 is Y+(Vertical),R12 is X+(Horizontal),
    R21 is Y+(2*Vertical),R22 is X+(2*Horizontal),
    R31 is Y+(3*Vertical),R32 is X+(3*Horizontal),
    Retorno=[[Y, X], 
             [R11, R12],
             [R21, R22],
             [R31, R32]].
    

/*Funciones Auxiliares*/
posicion([Elemento|Resto], Pos, Elemento):- longitud(Resto, Longitud), 7 is Longitud+Pos.
posicion([_Elemento|Resto], Pos, Nuevo):- longitud(Resto, Longitud), not(7 is Longitud+Pos), posicion(Resto, Pos, Nuevo).

longitud([], 0).
longitud([_|Xs], L):-longitud(Xs, L2), L is L2+1.

remplazar([_Elemento|Resto], Posicion, Ficha, [R|Rs]):-
    longitud(Resto, Lon), Posicion is 7-Lon, !,
    R=Ficha, Rs=Resto.
remplazar([Elemento|Resto], Posicion, Ficha, [R|Rs]):-
    longitud(Resto, Lon), Posicion+Lon \= 7,
    R=Elemento,
    remplazar(Resto, Posicion, Ficha, Rs).



/*====================Valores estaticos=======================*/
fila(1).
fila(2).
fila(3).
fila(4).
fila(5).
fila(6):-!.
columna(1).
columna(2).
columna(3).
columna(4).
columna(5).
columna(6).
columna(7):-!.
ficha(a).
ficha(b):-!.
direccion(1, 0).
direccion(0, 1).
direccion(-1, 0).
direccion(0, -1).
direccion(1, 1).
direccion(1, -1).
direccion(-1, 1).
direccion(-1, -1):-!.



