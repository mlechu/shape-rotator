:- initialization(main).

%% vertex(name, edges)

%% tetrahedron
%% cube
%% octahedron
%% dodecahedron
%% icosahedron

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
