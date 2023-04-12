% The abstract polytope is a lattice describing what faces contain what edges
% and what edges contain what vertices. In R^3, there will always be 5 levels
% (the top being the polyhedron, the bottom being some sort of null node)

:- consult(util).

%% Partial ordering of nodes
ap_has_v(edge(V, _), V).
ap_has_v(edge(_, V), V).
ap_has_v(face(Es), V) :-
    member(edge(X,Y), Es),
    ap_has_v(edge(X,Y), V).
ap_has_v(ap(Fs), V) :-
    member(face(Es), Fs),
    ap_has_v(face(Es), V).

ap_has_e(face(Es), edge(A, B)) :-
    member(edge(A, B), Es).
%% should not be necessary with ap_make_e
%% ap_has_e(face(Es), edge(A, B)) :-
%%     member(edge(B, A), Es).
ap_has_e(ap(Fs), E) :-
    member(F, Fs),
    ap_has_e(F, E).

ap_has_f(ap(Fs), F) :-
    member(F, Fs).

%% ap_make_e(E, A, B) :- E is edge(X, Y) where X < Y
%% Ordering helps with searching for an edge in a face
ap_make_e(edge(A, B), A, B) :- A @=< B, !.
ap_make_e(edge(B, A), A, B).

%% ap_normalize(A0, A) :-
%%     A0 = ap(Fs),

%% + defined on points
+(point(A1, A2, A3), point(B1, B2, B3), P) :-
    P1 is A1 + B1,
    P2 is A2 + B2,
    P3 is A3 + B3,
    P = point(P1, P2, P3).

%% ap_face_avg :- P is the central point of face F
ap_face_avg(F, P) :-
    setof(V, ap_has_v(F, V), FVs),
    length(FVs, L),
    l_foldr(+, point(0,0,0), FVs, point(X1, Y1, Z1)),
    P = point(X1 / L, Y1 / L, Z1 / L).

%% order a face's edges clockwise by angle from the centre
%% assumes 2d shape is convex
%% unmuck_face(F0, F)
%% maybe just do this at render time? graph check time??



%% g_to_ap()
%% ap_contains(T, face([edge(point(1, 1, 1), point(1, -1, -1)), edge(point(1, 1, 1), point(-1, -1, 1)), edge(point(1, -1, -1), point(-1, -1, 1))])).
