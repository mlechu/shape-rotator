:- initialization(main).
:- consult(seed).
:- consult(util).

%% All shapes surround the origin

%% TODO
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

gen_shape(Str, SVG) :-
    str_to_l(Str, Chars),
    l_reverse(Chars, [Seed|Ops]),
    s_seed(Seed, S0),
    apply_ops(S0, Ops, S),
    to_stl(S, STL),
    to_svg(S, SVG),
    write_out("shape.stl", STL),
    write_out("shape.svg", SVG).

main :- gen_shape("dC", _).
