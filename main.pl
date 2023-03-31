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

%% Bind a point to a name (makes code shorter)
p_def(point(X, Y, Z), point(X, Y, Z)).

p_mm(point(X,Y,Z), mat(A1,A2,A3,B1,B2,B3,C1,C2,C3), point(X2,Y2,Z2)) :-
    X2 is X * A1 + Y * A2 + Z * A3,
    Y2 is X * B1 + Y * B2 + Z * B3,
    Z2 is X * C1 + Y * C2 + Z * C3.

gratio(GR) :- GR is (1 + sqrt(5)) / 2.

%% reflect(x, point(X1,Y, Z),  point(X2,Y, Z))  :- X2 is X1 * -1.
%% reflect(y, point(X, Y1,Z),  point(X, Y2,Z))  :- Y2 is Y1 * -1.
%% reflect(z, point(X, Y, Z1), point(X, Y, Z2)) :- Z2 is Z1 * -1.


%% str_to_l(S, L) :- L is the list of characters in string S
str_to_l(S, L) :-
    atom_codes(A, S),
    atom_chars(A, L).

%% l_append(A, B, AB) :- AB is the concatenation of the two lists
l_append([H|A], B, [H|AB]) :-
    l_append(A,B,AB).
l_append([], B, B).

%% l_last(L, El) :- El is the last element of L
l_last([El], El).
l_last([_|T], El) :- last(T, El).

%% reverse(A, B) :- A and B are equivalent but reversed
l_reverse([], []).
l_reverse([H1|T1], L2) :-
    l_reverse(T1, RT1),
    l_append(RT1, [H1], L2).

%% Generate a shape from a seed (letter)
%% Supports all 5 platonic solids
%%
%% TODO
%% tetrahedron
%% octahedron
%% dodecahedron
%% icosahedron
%%
%% p_def might be inefficent here
s_seed("C", % Cube
       shape([vertex(AAA,[BAA, ABA, AAB]),
              vertex(AAB,[BAB, ABB, AAA]),
              vertex(ABA,[BBA, AAA, ABB]),
              vertex(ABB,[BBB, AAB, ABA]),
              vertex(BAA,[AAA, BBA, BAB]),
              vertex(BAB,[AAB, BBB, BAA]),
              vertex(BBA,[ABA, BBA, BBB]),
              vertex(BBB,[ABB, BAB, BBA])])) :-
    p_def(AAA, point( 1, 1, 1)),
    p_def(AAB, point( 1, 1,-1)),
    p_def(ABA, point( 1,-1, 1)),
    p_def(ABB, point( 1,-1,-1)),
    p_def(BAA, point(-1, 1, 1)),
    p_def(BAB, point(-1, 1,-1)),
    p_def(BBA, point(-1,-1, 1)),
    p_def(BBB, point(-1,-1,-1)).

%% TODO
%%
%% irreducible: ambo, dual, gyro, join, kis, snub, truncate
%%
%% easy: bevel (ta), expand (aa), meta (kj), ortho (jj)
%%
%% maybe (in addition to john's set): reflect, propellor, needle, zip
apply_op(S1, d, S).


%% TODO allow for exponents on operators?
apply_ops(S, [], S).
apply_ops(S0, [Op|Ops], S) :-
    apply_op(S0, Op, S1),
    apply_ops(S1, Ops, S).

to_svg(S, SVG).

to_stl(S, STL).

%% write_out(Filepath, Data) :- write Data to ./out/<Filepath>
write_out(Filepath, Data) :-
    \+ exists_directory("out"),
    make_directory("out"),
    write_out(Filepath, Data).
write_out(Filepath, Data) :-
    atom_concat("out/", Filepath, Path),
    open(Path, write, Fd, [create([default])]),
    write(Fd, Data),
    close(Fd).

generate_shape(Str, SVG) :-
    str_to_l(Str, Chars),
    l_reverse(Chars, [Seed|Ops]),
    s_seed(Seed, S0),
    apply_ops(S0, Ops, S),
    to_stl(S, STL),
    write_out("shape.svg", SVG).


main :- write_out("b.txt", "hello2").
