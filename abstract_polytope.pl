:- consult(util).
% https://en.wikipedia.org/wiki/Abstract_polytope
% The abstract polytope is a lattice describing what faces contain what edges
% and what edges contain what vertices. In R^3, there will always be 5 levels
% (the top being the polyhedron, the bottom being some sort of null node)
%
% Trying this out because it may make operations easier. might delete.

% Partial ordering of nodes
ap_contains(_, null).
% TODO cannot detect different orderings of edges within the face
% we may want to always order points, edges, and faces by coordinates
% OR identify edges/faces by their midpoints (floating point issues?)
ap_contains(abstract_polytope(FS), F) :-
    member(F, FS).
ap_contains(abstract_polytope(FS), X) :-
    member(F, FS),
    ap_contains(F, X).
ap_contains(A, B) :-
    e_has_v(A, B);
    f_has_e(A, B);
    f_has_v(A, B).

% Inclusion with known dimension
e_has_v(edge(V, _), V).
e_has_v(edge(_, V), V).
f_has_e(face(ES), edge(A, B)) :-
    member(edge(A, B), ES).
f_has_e(face(ES), edge(A, B)) :-
    member(edge(B, A), ES).
f_has_v(face(ES), point(X, Y, Z)) :-
    e_has_v(E, point(X, Y, Z)),
    member(E, ES).

ap_seed('T', T) :-
    V0 = point( 1, 1, 1),
    V1 = point( 1,-1,-1),
    V2 = point(-1,-1, 1),
    V3 = point(-1, 1,-1),
    E0 = edge(V0, V1        ),
    E1 = edge(V0,     V2    ),
    E2 = edge(V0,         V3),
    E3 = edge(    V1, V2    ),
    E4 = edge(    V1,     V3),
    E5 = edge(        V2, V3),
    F0 = face([E0, E1, E3]),
    F1 = face([E1, E2, E5]),
    F2 = face([E0, E2, E4]),
    F3 = face([E3, E4, E5]),
    T = abstract_polytope([F0, F1, F2, F3]).

p_sum([], point(0,0,0)).
p_sum([P0|T], point(X,Y,Z)) :-
    P0 = point(X0,Y0,Z0),
    p_sum(T, point(PSX, PSY, PSZ)),
    X is X0 + PSX,
    Y is Y0 + PSY,
    Z is Z0 + PSZ.

example_ap(T) :- ap_seed('T', T).
example_f(face([edge(point(1, 1, 1), point(1, -1, -1)),
                edge(point(1, 1, 1), point(-1, -1, 1)),
                edge(point(1, -1, -1), point(-1, -1, 1))])).
example_edge0(edge(point(1, 1, 1), point(1, -1, -1))).
example_edge0R(edge(point(1, -1, -1), point(1, 1, 1))).

% Find the central point of a face
ap_face_avg(F, P) :-
    findall(V, f_has_v(F, V), FVS),
    % note that each vertex is included twice, once per edge
    length(FVS, L),
    p_sum(FVS, point(X1, Y1, Z1)),
    P = point(X1 / L, Y1 / L, Z1 / L).

% The dual of an ap is its upside-down graph!
%% ap_dual(S0, S) :-


%% g_to_ap()
%% ap_contains(T, face([edge(point(1, 1, 1), point(1, -1, -1)), edge(point(1, 1, 1), point(-1, -1, 1)), edge(point(1, -1, -1), point(-1, -1, 1))])).
