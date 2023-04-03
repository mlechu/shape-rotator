%% point(x, y, z)

%% vertex(point, [point ...])
%%               connections

%% shape([vertex ...])

%% Generate a shape from a seed (letter)
%% Supports all 5 platonic solids
%%
%% TODO high chance dodecahedron and icosahedron edges are wrong
%%
s_seed('T', % Tetrahedron
       shape([vertex(V1,[V2, V3, V4]),
              vertex(V2,[V1, V3, V4]),
              vertex(V3,[V1, V2, V4]),
              vertex(V4,[V1, V2, V3])])) :-
    p_def(V1, point( 1, 1, 1)),
    p_def(V2, point( 1,-1,-1)),
    p_def(V3, point(-1, 1,-1)),
    p_def(V4, point(-1,-1, 1)).

s_seed('C', % Cube
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

s_seed('O', % Octahedron
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

s_seed('D', % Dodecahedron
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

s_seed('I', % Icosahedron
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
