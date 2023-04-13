:- ensure_loaded(util).
:- ensure_loaded(abstract_polytope).

write_stl_ap(Filename, AP) :-
    ap_facets(AP, STL),
    write_stl(Filename, STL).

ap_facets(AP, Facets) :-
    AP = ap(F0s),
    l_map(stl_tfan_face, F0s, F1ss),
    l_flatten(F1ss, Facets_pm),
    l_map(orient_facet, Facets_pm, Facets).

%% Facets is a list of facet(p1, p2, p3) covering the Face
stl_tfan_face(Face, Facets) :-
    Face = face([edge(P_base, _)|Es]),
    l_map(call(stl_tfan_edge(P_base)), Es, Facets0),
    l_filter(is_triangle, Facets0, Facets).

stl_tfan_edge(P, edge(A, B), facet(P, A, B)).

is_triangle(facet(PA, PB, PC)) :-
    dif(PA, PB),
    dif(PB, PC),
    dif(PA, PC).

%% F has the same points as F0, but in anticlockwise (outward normal) order
orient_facet(F0, F) :-
    F0 = facet(P1, P2, P3),
    facet_normal(F0, Vn_pm),
    %% Dot product of the vector from origin -> midpoint with the facet normal
    %% should be positive with an outward-facing facet normal.
    %%
    %% The midpoint (vector) used is that of the triangle, not the whole face
    facet_mid(F0, Mid),
    vector_dot(Vn_pm, Mid, DP),
    ( DP >= 0 -> F = F0
    ; F = facet(P3, P2, P1)
    ).

write_stl(Filename, Facets) :-
    \+ exists_directory("out"),
    make_directory("out"),
    write_stl(Filename, Facets).
write_stl(Filename, Facets) :-
    atom_concat("out/", Filename, Path0),
    atom_concat(Path0, ".stl", Path),
    open(Path, write, Stream),
    write(Stream, "solid MYSOLID\n"),
    write_facets(Facets, Stream),
    write(Stream, "endsolid MYSOLID\n"),
    close(Stream).

write_facets([], _).
write_facets([F|T], Stream) :-
    format_facet(F, Result),
    write(Stream, Result),
    write_facets(T, Stream).

format_facet(F, Result) :-
    F = facet(P1,P2,P3),
    facet_normal(F,vector(Xn,Yn,Zn)),
    format(string(I1), "  facet normal  ~w  ~w  ~w\n", [Xn,Yn,Zn]),
    string_concat(I1, "    outer loop\n", I2),
    format_point(P1, P1s),
    string_concat(I2, P1s, I3),
    format_point(P2, P2s),
    string_concat(I3, P2s, I4),
    format_point(P3, P3s),
    string_concat(I4, P3s, I5),
    string_concat(I5, "    endloop\n", I6),
    string_concat(I6, "  endfacet\n", Result).

format_point(point(X,Y,Z), Ps) :-
    format(string(Ps), "      vertex    ~20f  ~20f  ~20f\n", [X,Y,Z]).

vector_from_points(point(X0,Y0,Z0), point(XF,YF,ZF), vector(X,Y,Z)) :-
    X is XF-X0,
    Y is YF-Y0,
    Z is ZF-Z0.

vector_dot(vector(X1,Y1,Z1), vector(X2,Y2,Z2), DP) :-
    DP is (X1 * X2) + (Y1 * Y2) + (Z1 * Z2).

vector_cross(vector(X1,Y1,Z1), vector(X2,Y2,Z2), vector(X,Y,Z)) :-
    X is Y1*Z2 - Z1*Y2,
    Y is Z1*X2 - X1*Z2,
    Z is X1*Y2 - Y1*X2.

vector_magnitude(vector(X,Y,Z), M) :-
    M is sqrt(X**2 + Y**2 + Z**2).

vector_normalize(vector(X,Y,Z), vector(Xn,Yn,Zn)) :-
    vector_magnitude(vector(X,Y,Z), M),
    Xn is X / M,
    Yn is Y / M,
    Zn is Z / M.

v_invert(vector(X0, Y0, Z0), vector(X, Y, Z)) :-
    X is 0 - X0,
    Y is 0 - Y0,
    Z is 0 - Z0.

%% P1, P2, P3 are ordered anticlockwise viewed from the outside
%% '   .   ' normal = 2->3 cross 2->1
facet_normal(F,Vn) :-
    F = facet(P1,P2,P3),
    vector_from_points(P2,P3,V1),
    vector_from_points(P2,P1,V2),
    vector_cross(V1,V2,Vc),
    vector_normalize(Vc,Vn).

facet_mid(F, vector(X, Y, Z)) :-
    F = facet(point(X1, Y1, Z1), point(X2, Y2, Z2), point(X3, Y3, Z3)),
    X is (X1 + X2 + X3) / 3,
    Y is (Y1 + Y2 + Y3) / 3,
    Z is (Z1 + Z2 + Z3) / 3.
