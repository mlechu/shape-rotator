:- initialization(main).

%% point(x,y,z)
%% vertex(location, [edges...])
%% shape([points...])

%% All shapes surround the origin
%% Not sure if we should name each vertex instead of id'ing it by location

%% Field accessors
p_(x, point(X,_,_), X).
p_(y, point(_,Y,_), Y).
p_(z, point(_,_,Z), Z).

p_mm(point(X,Y,Z), mat(A1,A2,A3,B1,B2,B3,C1,C2,C3), point(X2,Y2,Z2)) :-
    X2 is X * A1 + Y * A2 + Z * A3,
    Y2 is X * B1 + Y * B2 + Z * B3,
    Z2 is X * C1 + Y * C2 + Z * C3.

hello :- point(1,2,3).

%% reflect(x, point(X1,Y, Z),  point(X2,Y, Z))  :- X2 is X1 * -1.
%% reflect(y, point(X, Y1,Z),  point(X, Y2,Z))  :- Y2 is Y1 * -1.
%% reflect(z, point(X, Y, Z1), point(X, Y, Z2)) :- Z2 is Z1 * -1.


%% tetrahedron :- shape([])
%% cube :- shape([vertex(point( 1, 1, 1),[]),
%%                vertex(point( 1, 1,-1),[]),
%%                vertex(point( 1,-1, 1),[]),
%%                vertex(point( 1,-1,-1),[]),
%%                vertex(point(-1, 1, 1),[]),
%%                vertex(point(-1, 1,-1),[]),
%%                vertex(point(-1,-1, 1),[]),
%%                vertex(point(-1,-1,-1),[])
%%               ]).
%% octahedron :- shape([])
%% dodecahedron :- shape([])
%% icosahedron :- shape([])

%% l_append(A, B, AB) :- AB is the concatenation of the two lists
l_append([H|A], B, [H|AB]) :-
    list_append(A,B,AB).
l_append([], B, B).

%% l_last(L, El) :- El is the last element of L
l_last([El], El).
l_last([_|T], El) :- last(T, El).

%% reverse(A, B) :- A and B are equivalent but reversed
%% reverse([], []).
%% reverse([H|T], [H2|T2]) :-

%% generate_shape(Str).

write_out(Filepath, Data) :-
    \+ exists_directory("out"),
    make_directory("out"),
    write_out(Filepath, Data).

write_out(Filepath, Data) :-
    atom_concat("out/", Filepath, Path),
    open(Path, write, Fd, [create([default])]),
    write(Fd, Data),
    close(Fd).

main :- write_out("b.txt", "hello2").
