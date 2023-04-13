%% utilities for producing STL files from sequences of points

:- ensure_loaded(util).
:- ensure_loaded(abstract_polytope).

write_stl_ap(Filename, AP) :-
    ap_facets(AP, STL),
    write_stl(Filename, STL).

ap_facets(AP, Facets) :-
    AP = ap(F0s),
    l_map(stl_tfan_face, F0s, F1ss),
    l_flatten(F1ss, Facets).

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

write_stl(Filename, Facets) :-
    open(Filename, write, Stream),
    write(Stream, "solid MYSOLID\n"),
    write_facets(Facets, Stream),
    write(Stream, "endsolid MYSOLID\n"),
    close(Stream).

write_facets([], _).
write_facets([F|T], Stream) :-
    format_facet(F, Result),
    write(Stream, Result),
    write_facets(T, Stream).

format_facet(facet(P1,P2,P3),Result) :-
    facet_normal(P1,P2,P3,vector(Xn,Yn,Zn)),
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
    format(string(Ps), "      vertex    ~w  ~w  ~w\n", [X,Y,Z]).

compute_vector(point(X1,Y1,Z1), point(X2,Y2,Z2), vector(X,Y,Z)) :-
    X is X2-X1,
    Y is Y2-Y1,
    Z is Z2-Z1.

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

compute_normal(V1,V2,Vn) :-
    vector_cross(V1,V2,Vc),
    vector_normalize(Vc,Vn).

facet_normal(P1,P2,P3,Vn) :-
    compute_vector(P1,P2,V1),
    compute_vector(P2,P3,V2),
    compute_normal(V1,V2,Vn).

%% TODO: we want to reverse the direction of the normal if it's "facing" the origin
%% I think this can be done by projecting the normal onto the vector from origin -> midpoint
facet_midpoint(point(X1, Y1, Z1), point(X2, Y2, Z2), point(X3, Y3, Z3), point(X, Y, Z)) :-
    X is (X1 + X2 + X3) / 3,
    Y is (Y1 + Y2 + Y3) / 3,
    Z is (Z1 + Z2 + Z3) / 3.
