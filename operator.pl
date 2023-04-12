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
apply_op(a, AP0, AP).
apply_op(d, AP0, AP) :- op_dual(AP0, AP).
apply_op(g, AP0, AP).
apply_op(j, AP0, AP).
apply_op(k, AP0, AP).
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

%% TODO
op_ambo(AP0, AP0).
%% TODO
op_kis(AP0, AP0).
%% TODO
op_gyro(AP0, AP0).
