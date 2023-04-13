:- ensure_loaded(util).
:- ensure_loaded(abstract_polytope).

%% TODO
%% irreducible: ambo (djd*), dual, gyro, join (dad*), kis, snub (dgd), truncate (dkd)
%%
%% easy: bevel (ta), expand (aa), meta (kj), ortho (jj)
%%
%% maybe (in addition to john's set): reflect (easy), propellor, needle (kd), zip (dk)
%%
%% algorithms reqd only for dual, (ambo OR join), kis, gyro, maybe propellor
%%
apply_op(a, AP0, AP) :- op_ambo(AP0, AP).
apply_op(d, AP0, AP) :- op_dual(AP0, AP).
apply_op(g, AP0, AP).
apply_op(j, AP0, AP).
apply_op(k, AP0, AP) :- op_kis(AP0, AP).
apply_op(s, AP0, AP).
apply_op(t, AP0, AP).

%% E is the dual edge of E0
op_dual_edge(F0s, Vs, E0, E) :-
    l_zip(F0s, Vs, F0_V_mapping),
    member(zip(FA, VA), F0_V_mapping),
    ap_has_e(FA, E0),
    !,
    member(zip(FB, VB), F0_V_mapping),
    dif(VA, VB),
    ap_has_e(FB, E0),
    !,
    ap_make_e(E, VA, VB).

%% gross. pls fix
op_dual_has_v(V0, zip(E0, _)) :-
    ap_has_v(E0, V0).

%% F is the dual face of V0
op_dual_face(E0s, Es, V0, F) :-
    l_zip(E0s, Es, E0_E_mapping),
    l_filter(call(op_dual_has_v(V0)), E0_E_mapping, E1Ms),
    % retrieve the second of each mapping
    l_map(call(arg(2)), E1Ms, E1s),
    F = face(E1s).

%% algorithms reqd only for dual, (ambo OR join), kis, gyro, maybe propellor

%% The dual of an ap is its upside-down graph!
op_dual(AP0, AP) :-
    AP0 = ap(F0s),

    %% Ps is the list of central points of F0s
    l_map(ap_face_avg, F0s, Vs),

    %% for each edge E0 in E0s:
    %%      search F0s; find the faces F1, F2 E0 appears in
    %%      produce edge(avg(F1), avg(F2))
    setof(E_tmp, ap_has_e(AP0, E_tmp), E0s),
    l_map(call(op_dual_edge(F0s, Vs)), E0s, Es),

    %% for each vertex V0 in V0s:
    %%      search E0s; find the edges E0s P0 appears in
    %%      Es = the duals of E0s
    %%      add (to Fs) face(Es)
    setof(V_tmp, ap_has_v(AP0, V_tmp), V0s),
    l_map(call(op_dual_face(E0s, Es)), V0s, Fs),
    AP = ap(Fs).

%% EA, EB are the two edges that end at V on face F
op_fvee(F, V, EA, EB) :-
    F = face(Es),
    member(EA, Es),
    ap_has_v(EA, V),
    !,
    member(EB, Es),
    ap_has_v(EB, V),
    dif(EA, EB).
op_vfee(V, F, EA, EB) :-
    op_fvee(F, V, EA, EB).

%% F is the new face replacing F0
op_ambo_face_face(F0, F) :-
    % F_V0s is the list of vertices in F0
    setof(V0_tmp, ap_has_v(F0, V0_tmp), F_V0s),
    l_map(call(op_fvee(F0)), F_V0s, E0As, E0Bs),
    l_map(call(ap_edge_avg), E0As, VAs),
    l_map(call(ap_edge_avg), E0Bs, VBs),
    l_map(call(ap_make_e), Es, VAs, VBs),
    F = face(Es).

%% F is the new face replacing V0
%% F0s is the list of faces that could contain V0
op_ambo_vert_face(F0s, V0, F) :-
    % V_F0s is the list of faces containing V0
    l_filter(ap_has_v, F0s, [V0], V_F0s),
    l_map(call(op_vfee(V0)), V_F0s, E0As, E0Bs),
    l_map(call(ap_edge_avg), E0As, VAs),
    l_map(call(ap_edge_avg), E0Bs, VBs),
    l_map(call(ap_make_e), Es, VAs, VBs),
    F = face(Es).

%% Creates a face by "cutting off" each vertex at the midpoint of each edge
op_ambo(AP0, AP) :-
    %%%% Faces part 1
    %% for each face F0:
    %%      Es = []
    %%      for each vert V0 in F0:
    %%          find the two edges E0_a, E0_b containing V0
    %%          add the edge between V(E0_a), V(E0_b) to Es
    %%      add face(Es) to Fs
    AP0 = ap(F0s),
    l_map(call(op_ambo_face_face), F0s, Fs_p1),
    %%%% Faces part 2
    %% for each vert V0:
    %%      Es = []
    %%      for each face F0 containing V0:
    %%          find the two edges E0_a, E0_b in F0 containing V0
    %%          add the edge between V(E0_a), V(E0_b) to Es
    %%      add face(Es) to Fs
    %%
    setof(V0_tmp, ap_has_v(AP0, V0_tmp), V0s),
    l_map(call(op_ambo_vert_face(F0s)), V0s, Fs_p2),
    l_append(Fs_p1, Fs_p2, Fs),
    AP = ap(Fs).

op_kis_triangle(Midpoint, edge(A, B), T) :-
    ap_make_e(EMA, Midpoint, A),
    ap_make_e(EMB, Midpoint, B),
    T = face([edge(A, B), EMA, EMB]).

%% Turn one original n-sided face F0 into n triangular faces
%% meeting at the midpoint of F0
op_kis_face0(F0, F0_Fs) :-
    F0 = face(Es),
    ap_face_avg(F0, Midpoint),
    l_map(call(op_kis_triangle(Midpoint)), Es, F0_Fs).

%% Raise a pyramid on each face.
%% The new vertex connects to all points of the face.
%% We want to (try to) maintain convexity, so normalize all
%% new and existing points to lie on the unit sphere
op_kis(ap(F0s), ap(Fs)) :-
    l_map(op_kis_face0, F0s, F0s_Fs),
    l_flatten(F0s_Fs, Fs).

%% TODO
%% Graphically, draw a vertex at the centre of each face, then draw a new edge
%% from there to each of the face's existing edges (Es), hitting E slightly
%% counter-clockwise of its midpoint.
%%
%% Divide each face of n edges into n faces of 5 edges
op_gyro(AP0, AP0).
