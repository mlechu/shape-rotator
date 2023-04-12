%% point(x,y,z)
%% Field accessors
%% p_(x, point(X,_,_), X).
%% p_(y, point(_,Y,_), Y).
%% p_(z, point(_,_,Z), Z).

%% Bind a point to a name (makes code shorter)
p_def(point(X, Y, Z), point(X, Y, Z)).

p_mat(mat(AX,AY,AZ,
          BX,BY,BZ,
          CX,CY,CZ), P0, P) :-
    P0 = point(X0,Y0,Z0),
    X is X0 * AX + Y0 * AY + Z0 * AZ,
    Y is X0 * BX + Y0 * BY + Z0 * BZ,
    Z is X0 * CX + Y0 * CY + Z0 * CZ,
    P = point(X, Y, Z).

%% ops defined on points
+(point(AX, AY, AZ), point(BX, BY, BZ), P) :-
    PX is AX + BX,
    PY is AY + BY,
    PZ is AZ + BZ,
    P = point(PX, PY, PZ).

*(point(P0X,P0Y,P0Z), Scalar, P) :-
    PX is P0X * Scalar,
    PY is P0Y * Scalar,
    PZ is P0Z * Scalar,
    P = point(PX, PY, PZ).

%% p_pm(Coords, P0, PS) :-
%% PS is a list of 2^Coords points such that each Coord allows
%% each point in PS to be reflected across the Coord axis
%% p_pm([Coord|Coords], P0s, Ps) :-
%% p_pm([Coord|Coords], P0s, Ps) :-

golden_ratio((1 + sqrt(5)) / 2).

%% reflect(x, point(X1,Y, Z),  point(X2,Y, Z))  :- X2 is X1 * -1.
%% reflect(y, point(X, Y1,Z),  point(X, Y2,Z))  :- Y2 is Y1 * -1.
%% reflect(z, point(X, Y, Z1), point(X, Y, Z2)) :- Z2 is Z1 * -1.


%% str_to_l(S, L) :- L is the list of characters (as atoms) in string S
str_to_l(S, L) :-
    atom_codes(A, S),
    atom_chars(A, L).

%% l_append(A, B, AB) :- AB is the concatenation of the two lists
l_append([H|A], B, [H|AB]) :-
    l_append(A,B,AB).
l_append([], B, B).

%% l_last(L, El) :- El is the last element of L
l_last([El], El).
l_last([_|T], El) :- last(T, El).

%% reverse(A, B) :- A and B are equivalent but reversed
l_reverse([], []).
l_reverse([H1|T1], L2) :-
    l_reverse(T1, RT1),
    l_append(RT1, [H1], L2).

%% l_map(Pred, L0, L) :- L is L0 transformed by Pred
l_map(_, [], []).
l_map(Pred, [H0|T0], [H|T]) :-
    call(Pred, H0, H),
    l_map(Pred, T0, T).

%% Pred(HA, HB, H) forall H
%% Map over two lists (or get two return values)
l_map(_, [], [], []).
l_map(Pred, [HA|TA], [HB|TB], [H|T]) :-
    call(Pred, HA, HB, H),
    l_map(Pred, TA, TB, T).

%% l_foldr(Pred, Init, [H|T], Out)
l_foldr(_, A, [], A).
l_foldr(Pred, A0, [H|T], A) :-
    call(Pred, A0, H, A1),
    l_foldr(Pred, A1, T, A).

l_filter(_, [], []).
l_filter(Pred, [H0|T0], L) :-
    l_filter(Pred, T0, LT),
    ( call(Pred, H0) -> L = [H0|LT] ; L = LT).

l_zip([], [], []).
l_zip([HA|TA], [HB|TB], [zip(HA, HB)|T]) :-
    l_zip(TA, TB, T).
