:- ensure_loaded(abstract_polytope).
:- ensure_loaded(operator).
:- ensure_loaded(util).

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

ap_seed('D', Dodecahedron) :-
    golden_ratio(RP),      %  1.6
    RM is -1 * RP,         % -1.6
    IP is 1 / RP,          %  0.6
    IM is -1 * IP,         % -0.6
    AAA = point( 1, 1, 1), % cube points
    AAB = point( 1, 1,-1),
    ABA = point( 1,-1, 1),
    ABB = point( 1,-1,-1),
    BAA = point(-1, 1, 1),
    BAB = point(-1, 1,-1),
    BBA = point(-1,-1, 1),
    BBB = point(-1,-1,-1),
    XAA = point( 0,RP,IP), % name = [0 axis][gr +/-][inv +/-]
    XAB = point( 0,RP,IM),
    XBA = point( 0,RM,IP),
    XBB = point( 0,RM,IM),
    YAA = point(IP, 0,RP),
    YAB = point(IM, 0,RP),
    YBA = point(IP, 0,RM),
    YBB = point(IM, 0,RM),
    ZAA = point(RP,IP, 0),
    ZAB = point(RP,IM, 0),
    ZBA = point(RM,IP, 0),
    ZBB = point(RM,IM, 0),

    ap_make_e(AAAX, AAA, XAA),
    ap_make_e(AAAY, AAA, YAA),
    ap_make_e(AAAZ, AAA, ZAA),

    ap_make_e(AABX, AAB, XAB),
    ap_make_e(AABY, AAB, YBA),
    ap_make_e(AABZ, AAB, ZAA),

    ap_make_e(ABAX, ABA, XBA),
    ap_make_e(ABAY, ABA, YAA),
    ap_make_e(ABAZ, ABA, ZAB),

    ap_make_e(ABBX, ABB, XBB),
    ap_make_e(ABBY, ABB, YBA),
    ap_make_e(ABBZ, ABB, ZAB),

    ap_make_e(BAAX, BAA, XAA),
    ap_make_e(BAAY, BAA, YAB),
    ap_make_e(BAAZ, BAA, ZBA),

    ap_make_e(BABX, BAB, XAB),
    ap_make_e(BABY, BAB, YBB),
    ap_make_e(BABZ, BAB, ZBA),

    ap_make_e(BBAX, BBA, XBA),
    ap_make_e(BBAY, BBA, YAB),
    ap_make_e(BBAZ, BBA, ZBB),

    ap_make_e(BBBX, BBB, XBB),
    ap_make_e(BBBY, BBB, YBB),
    ap_make_e(BBBZ, BBB, ZBB),

    ap_make_e(X_A, XAA, XAB),
    ap_make_e(X_B, XBA, XBB),
    ap_make_e(Y_A, YAA, YAB),
    ap_make_e(Y_B, YBA, YBB),
    ap_make_e(Z_A, ZAA, ZAB),
    ap_make_e(Z_B, ZBA, ZBB),

    F0  = face([X_A, AAAX, AAAZ, AABZ, AABX]),
    F1  = face([X_A, BABX, BABZ, BAAZ, BAAX]),
    F2  = face([X_B, BBAX, BBAZ, BBBZ, BBBX]),
    F3  = face([X_B, ABBX, ABBZ, ABAZ, ABAX]),

    F4  = face([Y_A, AAAY, AAAX, BAAX, BAAY]),
    F5  = face([Y_A, BBAY, BBAX, ABAX, ABAY]),
    F6  = face([Y_B, BABY, BABX, AABX, AABY]),
    F7  = face([Y_B, ABBY, ABBX, BBBX, BBBY]),

    F8  = face([Z_A, AAAZ, AAAY, ABAY, ABAZ]),
    F9  = face([Z_A, ABBZ, ABBY, AABY, AABZ]),
    F10 = face([Z_B, BBAZ, BBAY, BAAY, BAAZ]),
    F11 = face([Z_B, BABZ, BABY, BBBY, BBBZ]),

    Dodecahedron = ap([F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11]).

% lol
ap_seed('O', Octahedron) :-
    ap_seed('C', C),
    op_dual(C, Octahedron).

ap_seed('I', Icosahedron) :-
    ap_seed('D', D),
    op_dual(D, Icosahedron).
