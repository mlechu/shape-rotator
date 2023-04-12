:- ensure_loaded(abstract_polytope).
:- ensure_loaded(operator).

%% arity-1 preds for testing
example_ap(X) :- ap_seed('C', X).
example_f(face([edge(point(1, 1, 1), point(1, -1, -1)),
                edge(point(1, 1, 1), point(-1, -1, 1)),
                edge(point(1, -1, -1), point(-1, -1, 1))])).
example_edge0(edge(point(1, 1, 1), point(1, -1, -1))).
example_edge0R(edge(point(1, -1, -1), point(1, 1, 1))).

ap_seed('T', Tetrahedron) :-
    V0 = point( 1, 1, 1),
    V1 = point( 1,-1,-1),
    V2 = point(-1,-1, 1),
    V3 = point(-1, 1,-1),
    ap_make_e(E0, V0, V1),
    ap_make_e(E1, V0, V2),
    ap_make_e(E2, V0, V3),
    ap_make_e(E3, V1, V2),
    ap_make_e(E4, V1, V3),
    ap_make_e(E5, V2, V3),
    F0 = face([E0, E1, E3]),
    F1 = face([E1, E2, E5]),
    F2 = face([E0, E2, E4]),
    F3 = face([E3, E4, E5]),
    Tetrahedron = ap([F0, F1, F2, F3]).

ap_seed('C', Cube) :-
    AAA = point( 1, 1, 1),
    AAB = point( 1, 1,-1),
    ABA = point( 1,-1, 1),
    ABB = point( 1,-1,-1),
    BAA = point(-1, 1, 1),
    BAB = point(-1, 1,-1),
    BBA = point(-1,-1, 1),
    BBB = point(-1,-1,-1),
    ap_make_e(AAX, AAA, AAB),
    ap_make_e(ABX, ABA, ABB),
    ap_make_e(BAX, BAA, BAB),
    ap_make_e(BBX, BBA, BBB),
    ap_make_e(AXA, AAA, ABA),
    ap_make_e(AXB, AAB, ABB),
    ap_make_e(BXA, BAA, BBA),
    ap_make_e(BXB, BAB, BBB),
    ap_make_e(XAA, AAA, BAA),
    ap_make_e(XAB, AAB, BAB),
    ap_make_e(XBA, ABA, BBA),
    ap_make_e(XBB, ABB, BBB),
    F0 = face([AAX, AXA, ABX, AXB]),
    F1 = face([BAX, BXA, BBX, BXB]),
    F2 = face([AXA, XAA, BXA, XBA]),
    F3 = face([AXB, XAB, BXB, XBB]),
    F4 = face([XAA, AAX, XAB, BAX]),
    F5 = face([XBA, ABX, XBB, BBX]),
    Cube = ap([F0, F1, F2, F3, F4, F5]).

% lol
ap_seed('O', Octahedron) :-
    ap_seed('C', C),
    op_dual(C, Octahedron).
