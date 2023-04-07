%% utilities for producing STL files from sequences of points

write_stl(Filename) :-
    open(Filename, write, Stream),
    write(Stream, "Hello, world!\n"),
    close(Stream).

format_facet(Result) :-
    format(string(I1), "  facet normal ~w\n", [0.0]),
    string_concat(I1, "    outer loop\n", I2),
    string_concat(I2, "    end loop\n", I3),
    string_concat(I3, "  endfacet\n", Result).

format_vertex(X,Y,Z,Result) :-
    format(string(Result), "      vertex    ~w   ~w   ~w", [X,Y,Z]).

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

