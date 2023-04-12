:- initialization(main).

:- ensure_loaded(abstract_polytope).
:- ensure_loaded(operator).
:- ensure_loaded(util).
:- ensure_loaded(ap_seed).
:- ensure_loaded(stl).

%% TODO stub
to_stl(A, A).
to_svg(A, A).

%% TODO take the avg of vertex distance from the origin
%% and ensure minimum/maximum shape size

%% TODO allow for exponents on operators?
%% Apply the first operation, then recurse
apply_ops([], S0, S0).
apply_ops([Op|Ops], S0, S) :-
    apply_op(Op, S0, S1),
    apply_ops(Ops, S1, S).

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

gen_shape(Str, S) :-
    str_to_l(Str, Chars),
    l_reverse(Chars, [Seed|Ops]),
    ap_seed(Seed, S0),
    apply_ops(Ops, S0, S),
    %% to_stl(S, STL),
    %% to_svg(S, SVG),
    %% write_out("shape.stl", STL),
    %% write_out("shape.svg", SVG),
    format(string(X), "~w", S),
    write_out("shape.txt", X).

main :- gen_shape("ddddC", _).
