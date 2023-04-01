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

%% p_pm(Coords, P0, PS) :-
%% PS is a list of 2^Coords points such that each Coord allows
%% each point in PS to be reflected across the Coord axis
%% p_pm([Coord|Coords], P0s, Ps) :-
%% p_pm([Coord|Coords], P0s, Ps) :-


golden_ratio(GR) :- GR is (1 + sqrt(5)) / 2.

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
%% TODO high chance dodecahedron and icosahedron edges are wrong
%%
s_seed("T", % Tetrahedron
       shape([vertex(V1,[V2, V3, V4]),
              vertex(V2,[V1, V3, V4]),
              vertex(V3,[V1, V2, V4]),
              vertex(V4,[V1, V2, V3])])) :-
    p_def(V1, point( 1, 1, 1)),
    p_def(V2, point( 1,-1,-1)),
    p_def(V3, point(-1, 1,-1)),
    p_def(V4, point(-1,-1, 1)).
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
s_seed("O", % Octahedron
       shape([vertex(VX,[VY, Vy, VZ, Vz]),
              vertex(Vx,[VY, Vy, VZ, Vz]),
              vertex(VY,[VX, Vx, VZ, Vz]),
              vertex(Vy,[VX, Vx, VZ, Vz]),
              vertex(VZ,[VX, Vx, VY, Vy]),
              vertex(Vz,[VX, Vx, VY, Vy])])) :-
    p_def(VX, point( 1, 0, 0)),
    p_def(Vx, point(-1, 0, 0)),
    p_def(VY, point( 0, 1, 0)),
    p_def(Vy, point( 0,-1, 0)),
    p_def(VZ, point( 0, 0, 1)),
    p_def(Vz, point( 0, 0,-1)).
s_seed("D", % Dodecahedron
       shape([vertex(AAA,[XAA, YAA, ZAA]),
              vertex(AAB,[XAB, YAB, ZAB]),
              vertex(ABA,[XBA, YBA, ZBA]),
              vertex(ABB,[XBB, YBB, ZBB]),
              vertex(BAA,[XAA, YAA, ZAA]),
              vertex(BAB,[XAB, YAB, ZAB]),
              vertex(BBA,[XBA, YBA, ZBA]),
              vertex(BBB,[XBB, YBB, ZBB]),
              vertex(XAA,[XBA, AAA, BAA]),
              vertex(XAB,[XBB, AAB, BAB]),
              vertex(XBA,[XAA, ABA, BBA]),
              vertex(XBB,[XAB, ABB, BBB]),
              vertex(YAA,[YBA, AAA, BAA]),
              vertex(YAB,[YBB, AAB, BAB]),
              vertex(YBA,[YAA, ABA, BBA]),
              vertex(YBB,[YAB, ABB, BBB]),
              vertex(ZAA,[ZBA, AAA, BAA]),
              vertex(ZAB,[ZBB, AAB, BAB]),
              vertex(ZBA,[ZAA, ABA, BBA]),
              vertex(ZBB,[ZAB, ABB, BBB])])) :-
    golden_ratio(RP), %  1.6
    RM is -1 * RP,    %  0.6
    IM is -1 * IP,    % -0.6
    IP is 1 / RP,     % -1.6
    p_def(AAA, point( 1, 1, 1)), % cube points
    p_def(AAB, point( 1, 1,-1)),
    p_def(ABA, point( 1,-1, 1)),
    p_def(ABB, point( 1,-1,-1)),
    p_def(BAA, point(-1, 1, 1)),
    p_def(BAB, point(-1, 1,-1)),
    p_def(BBA, point(-1,-1, 1)),
    p_def(BBB, point(-1,-1,-1)),
    p_def(XAA, point( 0,IP,RP)), % [0 axis][inv gr +/-][gr +/-]
    p_def(XAB, point( 0,IP,RM)),
    p_def(XBA, point( 0,IM,RP)),
    p_def(XBB, point( 0,IM,RM)),
    p_def(YAA, point(RP, 0,IP)),
    p_def(YAB, point(RM, 0,IP)),
    p_def(YBA, point(RP, 0,IM)),
    p_def(YBB, point(RM, 0,IM)),
    p_def(ZAA, point(IP,RP, 0)),
    p_def(ZAB, point(IP,RM, 0)),
    p_def(ZBA, point(IM,RP, 0)),
    p_def(ZBB, point(IM,RM, 0)).
s_seed("I", % Icosahedron
       shape([vertex(XAA, [XBA, YAA, YBA, ZAA, ZAB]),
              vertex(XAB, [XBB, YBA, YBB, ZAA, ZAB]),
              vertex(XBA, [XAA, YAA, YBA, ZBA, ZBB]),
              vertex(XBB, [XAB, YBA, YBB, ZBA, ZBB]),
              vertex(YAA, [YBA, XAA, XAB, ZAA, ZBA]),
              vertex(YAB, [YBB, XAA, XAB, ZBA, ZBB]),
              vertex(YBA, [YAA, XBA, XBB, ZAA, ZBA]),
              vertex(YBB, [YAB, XBA, XBB, ZBA, ZBB]),
              vertex(ZAA, [ZBA, XAA, XBA, YAA, YAB]),
              vertex(ZAB, [ZBB, XBA, XBB, YAA, YAB]),
              vertex(ZBA, [ZAA, XAA, XBA, YBA, YBB]),
              vertex(ZBB, [ZAB, XBA, XBB, YBA, YBB])])) :-
    golden_ratio(RP), %  1.6
    RM is -1 * RP,    %  0.6
    p_def(XAA, point( 0, 1,RP)), % [0 axis][1 +/-][gr +/-]
    p_def(XAB, point( 0, 1,RM)),
    p_def(XBA, point( 0,-1,RP)),
    p_def(XBB, point( 0,-1,RM)),
    p_def(YAA, point(RP, 0, 1)),
    p_def(YAB, point(RM, 0, 1)),
    p_def(YBA, point(RP, 0,-1)),
    p_def(YBB, point(RM, 0,-1)),
    p_def(ZAA, point( 1,RP, 0)),
    p_def(ZAB, point( 1,RM, 0)),
    p_def(ZBA, point(-1,RP, 0)),
    p_def(ZBB, point(-1,RM, 0)).


%% new shape representation
%% i don't think this works
%%
%% face([ps...], [])
%% s_seed("C", % Cube
%%        shape([face(point( 1, 1, 1), point( 1, 1,-1), point( 1,-1, 1), point( 1,-1,-1)),
%%               face(point(-1, 1, 1), point(-1, 1,-1), point(-1,-1, 1), point(-1,-1,-1)),
%%               face(point( 1, 1, 1), point( 1, 1,-1), point(-1, 1, 1), point(-1, 1,-1)),
%%               face(point( 1,-1, 1), point( 1,-1,-1), point(-1,-1, 1), point(-1,-1,-1)),
%%               face(point( 1, 1, 1), point( 1,-1, 1), point(-1, 1, 1), point(-1,-1, 1)),
%%               face(point( 1, 1,-1), point( 1,-1,-1), point(-1, 1,-1), point(-1,-1,-1))])) :-
%%     p_def(AAA, point( 1, 1, 1)),
%%     p_def(AAB, point( 1, 1,-1)),
%%     p_def(ABA, point( 1,-1, 1)),
%%     p_def(ABB, point( 1,-1,-1)),
%%     p_def(BAA, point(-1, 1, 1)),
%%     p_def(BAB, point(-1, 1,-1)),
%%     p_def(BBA, point(-1,-1, 1)),
%%     p_def(BBB, point(-1,-1,-1)).


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
