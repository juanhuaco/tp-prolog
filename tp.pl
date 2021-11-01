% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% Your program goes here



/*EJERCICIO 1*/

tableroInicial(T):-
    T = [[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-],[-,-,-,-,-,-,-]].

/*EJERCICIO 2*/
/*estamos en la posicion 6*/
ingresarFicha([T|[]], Posicion, Ficha, [T2|[]]) :- posicion(T, Posicion, R1), R1 = '-', 
    T2 = T, 
    posicion(T2, Posicion, Ficha).


/*hacer sublistas en un rango*/
/*subLista([Elemento|Resto], [Inicio, Final]):-*/
/*concatenar sublistas*/
/*concatenar(L1, [E], Ret)*/

/*
ingresarFicha(T, C, F, T2).

ingresarFicha([T|Ts], C, F, T2) :- Ts=[], posicion(T, C, Pos), Pos = '-', T2  

1-estamos es la ultima fila - Remplazamos
2-haya una ficha abajo - remplazamos
3-no haya ficha - bajamos - evaluamos again
4-este lleno en la fila 1 no hay espacio
*/
/** <examples> Your example queries go here, e.g.
?- member(X, [cat, mouse]).
*/

/*EJERCICIO 4*/

contenido([T|Ts],[Y, X],Ficha):-
    longitud(Ts, L), 6 is L+Y, posicion(T, X, Ficha).

contenido([_T|Ts],[Y, X],Ficha):-
    longitud(Ts, L), not(6 is L+Y),
    contenido(Ts,[Y, X],Ficha).

/*Funciones Auxiliares*/
posicion([Elemento|Resto], Pos, Elemento):- longitud(Resto, Longitud), 7 is Longitud+Pos.
posicion([_Elemento|Resto], Pos, Nuevo):- longitud(Resto, Longitud), not(7 is Longitud+Pos), posicion(Resto, Pos, Nuevo).

longitud([], 0).
longitud([_|Xs], L):-longitud(Xs, L2), L is L2+1.


