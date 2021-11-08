:-use_rendering(table).

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
ingresarFicha([F1, F2|Fs], Columna, Ficha, [T1, T2|Ts]):- columna(Columna), ficha(Ficha),
    posicion(F1, Columna, Ret1), Ret1='-',
    posicion(F2, Columna, Ret2), Ret2\='-',!,
    T2 = F2, Ts = Fs,
    remplazar(F1, Columna, Ficha, T1).
/*no haya ficha - bajamos - evaluamos again*/
ingresarFicha([F1, F2|[Fs1 | Fss]], Columna, Ficha, [T1, T2|[Ts1|Tss]]):- columna(Columna), ficha(Ficha),
    posicion(F1, Columna, Ret1), Ret1='-',
    posicion(F2, Columna, Ret2), Ret2='-',
    T1 = F1,
    ingresarFicha([F2, Fs1|Fss], Columna, Ficha, [T2, Ts1|Tss]).
/*estamos es la ultima fila - Remplazamos*/
ingresarFicha([F1, F2|[]], Columna, Ficha, [T1, T2|[]]):- columna(Columna), ficha(Ficha),
    posicion(F1, Columna, Ret1), Ret1='-',
    posicion(F2, Columna, Ret2), Ret2='-',
    T1 = F1,
    remplazar(F2, Columna, Ficha, T2).

/*==================EJERCICIO 3======================*/

columnaDisp(Tablero, Columna):- ficha(X),!,
    columna(Columna),
    ingresarFicha(Tablero, Columna, X, _).


/*==================EJERCICIO 4======================*/

contenido([T|Ts],[Y, X],Ficha):- columna(X), fila(Y),
    longitud(Ts, L), 6 is L+Y,!, posicion(T, X, Ficha).

contenido([_T|Ts],[Y, X],Ficha):- columna(X), fila(Y),
    longitud(Ts, L), not(6 is L+Y),
    contenido(Ts,[Y, X],Ficha).

/*==================EJERCICIO 5======================*/



conecta4(Tablero,Ficha,Retorno):- 
    nb_setval(memoria, []),!,
    
    columna(X), fila(Y), ficha(Ficha),
    direccion(Horizontal, Vertical),
    
    Y1 is Y+(Vertical), X1 is X+(Horizontal), 
    Y2 is Y+(2*Vertical), X2 is X+(2*Horizontal), 
    Y3 is Y+(3*Vertical), X3 is X+(3*Horizontal), 
    
	contenido(Tablero, [Y, X], Ficha),
    contenido(Tablero, [Y1, X1], Ficha),
    contenido(Tablero, [Y2, X2], Ficha),
    contenido(Tablero, [Y3, X3], Ficha),
	
    Retorno=[[Y, X], 
             [Y1, X1],
             [Y2, X2],
             [Y3, X3]],
    
	RetornoInverso = [[Y3, X3], 
            		 [Y2, X2],
          		     [Y1, X1],
         		     [Y, X]],
    
    nb_getval(memoria, M),
    not(member(Retorno, M)),
    append([RetornoInverso], M, NewMemoria),
    nb_setval(memoria, NewMemoria).
    

/*==================EJERCICIO 6======================*/

noEmpate(Tablero):- 
    columna(Columna), ficha(F1),
    ingresarFicha(Tablero, Columna, F1, _AA);
    ficha(F2),
    conecta4(Tablero,F2,_A).

empate(Tablero):- not(noEmpate(Tablero)).

/*==================EJERCICIO 7======================*/

jugadaGanadora(Tablero,Ficha,Columna):-
    nb_setval(parFichaColumna, []),!,
    
    columna(Columna), ficha(Ficha),
    ingresarFicha(Tablero, Columna, Ficha, T2),
    conecta4(T2,Ficha, _),
    
    nb_getval(parFichaColumna, M),
    not(member([Ficha, Columna], M)),
    append([[Ficha, Columna]], M, NewMemoria),
    nb_setval(parFichaColumna, NewMemoria).
    

/*==================EJERCICIO 8======================*/

jugadaSegura(Tablero,Ficha,Columna):-
    nb_setval(parFichaColumna, []),!,
    
    ficha(Ficha), ficha(Rival), columna(Columna),
    Rival\=Ficha,
    ingresarFicha(Tablero, Columna, Ficha, T2),
    not(jugadaGanadora(T2, Rival, _C2)),
    
    nb_getval(parFichaColumna, M),
    not(member([Ficha, Columna], M)),
    append([[Ficha, Columna]], M, NewMemoria),
    nb_setval(parFichaColumna, NewMemoria).

/*==================EJERCICIO 9======================*/

jugadaDefinitiva(Tablero,Ficha,Columna):- 
    ficha(Ficha), ficha(Rival), columna(Columna),
    Rival\=Ficha,
    ingresarFicha(Tablero,Columna,Ficha,T2),
    not(jugadaSegura(T2, Rival, _)).

/*====================================================*/

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

/*Ejemplos de Ejecucion*/

tableroEjemplo3a(T):-
    T =  [[a,-,-,b,-,-,-],
          [a,-,-,b,-,-,-],
          [a,-,-,b,-,-,a],
          [a,-,-,a,-,-,a],
          [a,-,-,b,a,b,b],
          [a,a,b,b,a,b,b]].

tableroEjemplo4(T):-
    T =  [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,b,-,-],
          [-,-,-,-,b,-,-],
          [-,-,a,-,b,-,-],
          [b,a,a,a,b,-,-]].

tableroEjemplo5(T):-
    T =  [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,b,-,-],
          [-,-,-,-,b,-,-],
          [-,-,a,-,b,-,-],
          [b,a,a,a,b,-,-]].

tableroEmpate(T):-
	T =  [[a,a,a,b,a,a,a],
          [a,b,a,b,a,b,a],
          [a,a,a,b,a,a,a],
          [b,b,b,a,b,b,b],
          [a,a,a,b,a,a,a],
          [a,b,a,b,a,b,a]].

tableroLlenoNoEmpate(T):-
	T =  [[a,a,a,a,a,b,a],
          [b,a,b,a,b,a,b],
          [a,b,a,b,a,b,a],
          [b,a,b,a,b,a,b],
          [a,b,a,b,a,b,a],
          [b,a,b,a,b,a,b]].

tableroEjemploJugadaGanadora(T):-
    T =  [[-,-,-,b,-,-,-],
          [-,-,-,a,-,-,-],
          [-,-,-,a,-,-,-],
          [a,-,b,a,b,b,-],
          [b,a,a,b,a,b,-],
          [a,a,b,b,a,b,-]].




tableroEjemplo8a(T):-
    T =  [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,a,-,-,-],
          [-,-,-,a,-,-,-],
          [b,b,b,a,a,-,-]].

tableroEjemplo8b(T):-
    T =  [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,a,-,-,-,-],
          [b,b,b,a,-,-,-],
          [b,b,b,a,a,-,-]].

tableroEjemplo8c(T):-
    T =  [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,a,-,-,-,-,-],
          [a,b,a,-,-,-,-],
          [a,b,b,a,-,b,-]].

tableroEjemplo9(T):-
    T =  [[-,-,-,-,-,-,-],
          [-,-,-,-,-,-,-],
          [-,-,-,-,-,-,a],
          [-,-,-,a,-,-,a],
          [-,-,-,b,a,b,b],
          [a,a,b,b,a,b,b]].



tableroEjemplo(T):-
    T =  [[-,-,-,b,-,-,-],
          [-,-,-,a,-,-,-],
          [-,-,-,a,a,-,-],
          [a,-,b,a,b,b,-],
          [b,a,a,b,a,b,-],
          [a,a,b,b,a,b,-]].



