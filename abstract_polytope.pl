:- consult(util).
% https://en.wikipedia.org/wiki/Abstract_polytope
% The abstract polytope is a lattice describing what faces contain what edges
% and what edges contain what vertices. In R^3, there will always be 5 levels
% (the top being the polyhedron, the bottom being some sort of null node)
%
% Trying this out because it may make operations easier. might delete.

%% Partial ordering of nodes
has_v(edge(V, _), V).
has_v(edge(_, V), V).
has_v(face(Es), V) :-
    member(E, Es),
    has_v(E, V).
has_v(ap(Fs), V) :-
    member(face(Es), Fs),
    has_v(face(Es), V).

has_e(face(Es), edge(A, B)) :-
    member(edge(A, B), Es).
%% should not be necessary with e_make
%% has_e(face(Es), edge(A, B)) :-
%%     member(edge(B, A), Es).
has_e(ap(Fs), E) :-
    member(F, Fs),
    has_e(F, E).

has_f(ap(Fs), F) :-
    member(F, Fs).

%% e_make(E, A, B) :- E is edge(X, Y) where X < Y
%% Ordering helps with searching for an edge in a face
e_make(edge(A, B), A, B) :- A @=< B, !.
e_make(edge(B, A), A, B).

ap_seed('T', T) :-
    V0 = point( 1, 1, 1),
    V1 = point( 1,-1,-1),
    V2 = point(-1,-1, 1),
    V3 = point(-1, 1,-1),
    e_make(E0, V0, V1        ),
    e_make(E1, V0,     V2    ),
    e_make(E2, V0,         V3),
    e_make(E3,     V1, V2    ),
    e_make(E4,     V1,     V3),
    e_make(E5,         V2, V3),
    F0 = face([E0, E1, E3]),
    F1 = face([E1, E2, E5]),
    F2 = face([E0, E2, E4]),
    F3 = face([E3, E4, E5]),
    T = ap([F0, F1, F2, F3]).

%% ap_normalize(A0, A) :-
%%     A0 = ap(Fs),

%% + defined on points
+(point(A1, A2, A3), point(B1, B2, B3), P) :-
    P1 is A1 + B1,
    P2 is A2 + B2,
    P3 is A3 + B3,
    P = point(P1, P2, P3).

%% arity-1 preds for testing
example_ap(T) :- ap_seed('T', T).
example_f(face([edge(point(1, 1, 1), point(1, -1, -1)),
                edge(point(1, 1, 1), point(-1, -1, 1)),
                edge(point(1, -1, -1), point(-1, -1, 1))])).
example_edge0(edge(point(1, 1, 1), point(1, -1, -1))).
example_edge0R(edge(point(1, -1, -1), point(1, 1, 1))).

%% ap_face_avg :- P is the central point of face F
ap_face_avg(F, P) :-
    setof(V, has_v(F, V), FVs),
    length(FVs, L),
    l_foldr(+, point(0,0,0), FVs, point(X1, Y1, Z1)),
    P = point(X1 / L, Y1 / L, Z1 / L).

%% order a face's edges clockwise by angle from the centre
%% assumes 2d shape is convex
%% unmuck_face(F0, F)
%% maybe just do this at render time? graph check time??

%% E is the dual edge of E0
ap_dual_edge(F0s, Vs, E0, E) :-
    l_zip(F0s, Vs, F0_V_mapping),
    member(zip(FA, VA), F0_V_mapping),
    has_e(FA, E0),
    !,
    member(zip(FB, VB), F0_V_mapping),
    has_e(FB, E0),
    !,
    e_make(E, VA, VB).

%% gross. pls fix
ap_dual_has_v(V0, zip(E0, _)) :-
    has_v(E0, V0).

%% F is the dual face of V0
ap_dual_face(E0s, Es, V0, F) :-
    l_zip(E0s, Es, E0_E_mapping),
    l_filter(call(ap_dual_has_v(V0)), E0_E_mapping, E1s),
    F = face(E1s).

%% The dual of an ap is its upside-down graph!
ap_dual(AP0, Vs) :-
    AP0 = ap(F0s),

    %% Ps is the list of central points of F0s
    l_map(ap_face_avg, F0s, Vs),

    %% for each edge E0 in E0s:
    %%      search F0s; find the faces F1, F2 E0 appears in
    %%      produce edge(avg(F1), avg(F2))
    setof(E_tmp, has_e(AP0, E_tmp), E0s),
    l_map(call(ap_dual_edge(F0s, Vs)), E0s, Es),

    %% for each vertex V0 in V0s:
    %%      search E0s; find the edges E0s P0 appears in
    %%
    %%      add to Fs face(Es[Z0], Es[Z1]...)
    %%      (This is a face with all the dual edges
    %%      of the original edges radiating from P0)
    setof(V_tmp, has_v(AP0, V_tmp), V0s),
    l_map(call(ap_dual_face(E0s, Es)), V0s, Vs).



%% g_to_ap()
%% ap_contains(T, face([edge(point(1, 1, 1), point(1, -1, -1)), edge(point(1, 1, 1), point(-1, -1, 1)), edge(point(1, -1, -1), point(-1, -1, 1))])).
