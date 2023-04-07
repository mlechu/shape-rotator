%% utilities for producing STL files from sequences of points

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

