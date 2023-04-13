% The abstract polytope is a lattice describing what faces contain what edges
% and what edges contain what vertices. In R^3, there will always be 5 levels
% (the top being the polyhedron, the bottom being some sort of null node)

:- ensure_loaded(util).

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

%% ap_face_avg :- P is the central point of face F
ap_face_avg(F, P) :-
    setof(V, ap_has_v(F, V), FVs),
    length(FVs, L),
    l_foldr(+, point(0,0,0), FVs, point(X1, Y1, Z1)),
    P = point(X1 / L, Y1 / L, Z1 / L).

%% P is the midpoint of E
ap_edge_avg(E, P) :-
    E = edge(PA, PB),
    +(PA, PB, PS),
    *(PS, 1/2, P).

%% AP is the result of calling Pred with (V, Xargs) foreach V in AP0
ap_map_v(Pred0, V0, Xargs, V) :-
    V0 = point(_, _, _),
    args_append(Pred0, [V0|Xargs], Pred),
    call(Pred, V), !.
ap_map_v(Pred, edge(V0A, V0B), Xargs, E) :-
    ap_map_v(Pred, V0A, Xargs, VA),
    ap_map_v(Pred, V0B, Xargs, VB),
    ap_make_e(E, VA, VB), !.
ap_map_v(Pred, face(E0s), Xargs, face(Es)) :-
    ap_map_v(Pred, E0s, Xargs, Es).
ap_map_v(Pred, ap(F0s), Xargs, ap(Fs)):-
    ap_map_v(Pred, F0s, Xargs, Fs), !.
ap_map_v(_, [], _, []) :- !.
ap_map_v(Pred, [X0|X0s], Xargs, [X|Xs]) :-
    ap_map_v(Pred, X0, Xargs, X),
    ap_map_v(Pred, X0s, Xargs, Xs), !.

%% without xargs
ap_map_v(Pred, AP0, AP) :-
    ap_map_v(Pred, AP0, [], AP).


%% point_scale(point(X0,Y0,Z0), N, P) :-
%%     X is X0 * N,
%%     Y is Y0 * N,
%%     Z is Z0 * N,
%%     P = point(X,Y,Z).


%% order a face's edges clockwise by angle from the centre
%% assumes 2d shape is convex
%% unmuck_face(F0, F)
%% maybe just do this at render time? graph check time??

%% g_to_ap()
