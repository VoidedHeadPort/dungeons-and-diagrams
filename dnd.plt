:- [dnd].


:- begin_tests(examples).

test(add) :-
    A is 1 + 2,
    A =:= 3.

test(member, [nondet]) :-
    member(b, [a,b,c]).

:- end_tests(examples).


:- begin_tests(count).

test(count_8_w_8, true(Row == [w,w,w,w,w,w,w,w])) :-
    Row = [_,_,_,_,_,_,_,_],
    count(w, 8, Row).

test(count_8_w_7, all(Row == [
            [w,w,w,w,w,w,w,s],
            [w,w,w,w,w,w,s,w],
            [w,w,w,w,w,s,w,w],
            [w,w,w,w,s,w,w,w],
            [w,w,w,s,w,w,w,w],
            [w,w,s,w,w,w,w,w],
            [w,s,w,w,w,w,w,w],
            [s,w,w,w,w,w,w,w]
        ])) :-
    Row = [_,_,_,_,_,_,_,_],
    count(w, 7, Row).

test(count_8_w_1, all(Row == [
            [w,s,s,s,s,s,s,s],
            [s,w,s,s,s,s,s,s],
            [s,s,w,s,s,s,s,s],
            [s,s,s,w,s,s,s,s],
            [s,s,s,s,w,s,s,s],
            [s,s,s,s,s,w,s,s],
            [s,s,s,s,s,s,w,s],
            [s,s,s,s,s,s,s,w]
        ])) :-
    Row = [_,_,_,_,_,_,_,_],
    count(w, 1, Row).

test(count_8_w_0, all(Row == [
            [s,s,s,s,s,s,s,s]
        ])) :-
    Row = [_,_,_,_,_,_,_,_],
    count(w, 0, Row).

test(count_8_w_7_m, all(Row == [
            [w,w,w,w,w,m,w,w]
        ])) :-
    Row = [_,_,_,_,_,m,_,_],
    count(w, 7, Row).

test(count_8_w_1_m, all(Row == [
            [w,s,m,s,s,s,s,s],
            [s,w,m,s,s,s,s,s],
            [s,s,m,w,s,s,s,s],
            [s,s,m,s,w,s,s,s],
            [s,s,m,s,s,w,s,s],
            [s,s,m,s,s,s,w,s],
            [s,s,m,s,s,s,s,w]
        ])) :-
    Row = [_,_,m,_,_,_,_,_],
    count(w, 1, Row).

test(count_2_w_1_filled, all(Row == [
            [w,s]
        ])) :-
    Row = [w,s],
    count(w, 1, Row).

test(count_2_w_0_filled, all(Row == [
            [s,s]
        ])) :-
    Row = [s,s],
    count(w, 0, Row).

test(count_1_w_0_filled, all(Row == [
            [s]
        ])) :-
    Row = [s],
    count(w, 0, Row).

:- end_tests(count).


:- begin_tests(board).

% We can't use Board == [...] because of the free variables.
% Use Board = [...] to check it unifies.
test(build_board_6x6_empty) :-
    build_board(6, 6, Board),
    Board = [
        [_,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,_]
    ].

test(build_board_6x6_partial) :-
    Board = [
        [_,_,_,_,_,c],
        _,
        [m|_],
        _,
        [_,_,_,_,_,m],
        [m|_]
    ],
    build_board(6, 6, Board),
    Board = [
        [_,_,_,_,_,c],
        [_,_,_,_,_,_],
        [m,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,m],
        [m,_,_,_,_,_]
    ].

test(build_board_8x8_empty) :-
    build_board(8, 8, Board),
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ].

test(build_board_8x8_partial) :-
    Board = [
        _,
        [_,_,_,_,_,_,_,m],
        [_,_,m|_],
        [_,_,_,_,_,_,_,m],
        _,
        [_,c,_,_,_,_,_,m],
        _,
        [_,_,_,_,_,_,_,m]
    ],
    build_board(8, 8, Board),
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [_,_,m,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [_,c,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m]
    ].

:- end_tests(board).


:- begin_tests(mega_board).

test(build_mega_board_8x8_empty) :-
    Board = [
        [A1,A2,A3,A4,A5,A6,A7,A8],
        [B1,B2,B3,B4,B5,B6,B7,B8],
        [C1,C2,C3,C4,C5,C6,C7,C8],
        [D1,D2,D3,D4,D5,D6,D7,D8],
        [E1,E2,E3,E4,E5,E6,E7,E8],
        [F1,F2,F3,F4,F5,F6,F7,F8],
        [G1,G2,G3,G4,G5,G6,G7,G8],
        [H1,H2,H3,H4,H5,H6,H7,H8]
    ],
    build_mega_board(Board, MegaBoard),
    MegaBoard == [
        [ w, w, w, w, w, w, w, w, w, w],
        [ w,A1,A2,A3,A4,A5,A6,A7,A8, w],
        [ w,B1,B2,B3,B4,B5,B6,B7,B8, w],
        [ w,C1,C2,C3,C4,C5,C6,C7,C8, w],
        [ w,D1,D2,D3,D4,D5,D6,D7,D8, w],
        [ w,E1,E2,E3,E4,E5,E6,E7,E8, w],
        [ w,F1,F2,F3,F4,F5,F6,F7,F8, w],
        [ w,G1,G2,G3,G4,G5,G6,G7,G8, w],
        [ w,H1,H2,H3,H4,H5,H6,H7,H8, w],
        [ w, w, w, w, w, w, w, w, w, w]
    ].

:- end_tests(mega_board).


:- begin_tests(chests).

test(rule_chests_0) :-
    MegaBoard = [
        [w,w,w,w,w],
        [w,_,_,_,w],
        [w,_,_,_,w],
        [w,_,_,_,w],
        [w,w,w,w,w]
    ],
    rule_chests(MegaBoard).

test(rule_chests_1_begin, nondet) :-
    Board = [
        [c,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_chests(MegaBoard),
    Board = [
        [ c, r, r,D1|_],
        [ r, r, r,D2|_],
        [ r, r, r,D3|_],
        [A4,B4,C4, _|_]|
    _],
    count(w, 5, [D1,D2,D3,C4,B4,A4]).

test(rule_chests_1_begin_unique, true(SetLength =:= AllLength)) :-
    Board = [
        [c,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    setof(MegaBoard, rule_chests(MegaBoard), SetMegaBoards),
    findall(MegaBoard, rule_chests(MegaBoard), AllMegaBoards),
    length(SetMegaBoards, SetLength),
    length(AllMegaBoards, AllLength).
    

test(rule_chests_1_mid, nondet) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,c,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_chests(MegaBoard).

test(rule_chests_1_mid_unique, true(SetLength =:= AllLength)) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,c,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    setof(MegaBoard, rule_chests(MegaBoard), SetMegaBoards),
    findall(MegaBoard, rule_chests(MegaBoard), AllMegaBoards),
    length(SetMegaBoards, SetLength),
    length(AllMegaBoards, AllLength).

test(rule_chests_2_begin_end, nondet) :-
    Board = [
        [c,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,c]
    ],
    build_mega_board(Board, MegaBoard),
    rule_chests(MegaBoard),
    Board = [
        [ c, r, r,D1, _, _, _, _],
        [ r, r, r,D2, _, _, _, _],
        [ r, r, r,D3, _, _, _, _],
        [A4,B4,C4, _, _, _, _, _],
        [ _, _, _, _, _,F5,G5,H5],
        [ _, _, _, _,E6, r, r, r],
        [ _, _, _, _,E7, r, r, r],
        [ _, _, _, _,E8, r, r, c]
    ],
    count(w, 5, [D1,D2,D3,A4,B4,C4]),
    count(w, 5, [F5,G5,H5,E6,E7,E8]).

test(rule_chests_2_begin_end_unique, true(SetLength =:= AllLength)) :-
    Board = [
        [c,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,c]
    ],
    build_mega_board(Board, MegaBoard),
    setof(MegaBoard, rule_chests(MegaBoard), SetMegaBoards),
    findall(MegaBoard, rule_chests(MegaBoard), AllMegaBoards),
    length(SetMegaBoards, SetLength),
    length(AllMegaBoards, AllLength).

test(rule_chests_puzzle_1_1, nondet) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,c,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_chests(MegaBoard),
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,w,w,_,_,_,_,_],
        [r,r,r,w,_,_,_,_],
        [r,c,r,w,_,_,_,_],
        [r,r,r,w,_,_,_,_],
        [w,w,w,_,_,_,_,_]
    ].

test(rule_chests_puzzle_1_1_unique, true(SetLength =:= AllLength)) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,c,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    setof(MegaBoard, rule_chests(MegaBoard), SetMegaBoards),
    findall(MegaBoard, rule_chests(MegaBoard), AllMegaBoards),
    length(SetMegaBoards, SetLength),
    length(AllMegaBoards, AllLength).

test(detect_chest) :-
    detect_chest([_,_,_,_,c,_,_,_,_]).

test(detect_chest_empty, fail) :-
    detect_chest([_,_,_,_,_,_,_,_,_]).

test(chest_room, true(Length =:= 1)) :-
    Board = [
        [w,w,w,w,w,w],
        [s,w,w,w,w,w],
        [s,w,r,r,r,w],
        [s,w,r,c,r,w],
        [s,s,r,r,r,w],
        [s,w,w,w,w,w]
    ],
    findall(Board, chest_room(Board), Boards),
    length(Boards, Length).


:- end_tests(chests).


:- begin_tests(lines).

test(rule_lines_2x2, all(Board == [
            [
                [w, s],
                [s, w]
            ],
            [
                [s, w],
                [w, s]
            ]
        ])) :-
    RowCounts = [1,1],
    ColCounts = [1,1],
    build_board(2, 2, Board),
    rule_lines(RowCounts, ColCounts, Board).

test(rule_lines_2x2_m, all(Board == [
            [
                [m, w],
                [w, s]
            ]
        ])) :-
    RowCounts = [1,1],
    ColCounts = [1,1],
    build_board(2, 2, Board),
    Board = [
        [m,_],
        [_,_]
    ],
    rule_lines(RowCounts, ColCounts, Board).

test(rule_lines_simple_case, all(Board == [
            [
                [w,w,w,w,w,w,w,w],
                [w,w,w,w,w,w,w,s],
                [w,w,w,w,w,w,s,s],
                [w,w,w,w,w,s,s,s],
                [w,w,w,w,s,s,s,s],
                [w,w,w,s,s,s,s,s],
                [w,w,s,s,s,s,s,s],
                [w,s,s,s,s,s,s,s]
            ]
        ])) :-
    RowCounts = [8,7,6,5,4,3,2,1],
    ColCounts = [8,7,6,5,4,3,2,1],
    build_board(8, 8, Board),
    rule_lines(RowCounts, ColCounts, Board).

test(rule_lines_worst_case, all(Board == [
            [
                [s,s,s,s,s,s,s,w],
                [s,s,s,s,s,s,w,w],
                [s,s,s,s,s,w,w,w],
                [s,s,s,s,w,w,w,w],
                [s,s,s,w,w,w,w,w],
                [s,s,w,w,w,w,w,w],
                [s,w,w,w,w,w,w,w],
                [w,w,w,w,w,w,w,w]
            ]
        ])) :-
    RowCounts = [1,2,3,4,5,6,7,8],
    ColCounts = [1,2,3,4,5,6,7,8],
    build_board(8, 8, Board),
    rule_lines(RowCounts, ColCounts, Board).

:- end_tests(lines).


:- begin_tests(dead_ends).

test(rule_dead_ends_monster_invalid, fail) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,s,_,_,_,_,_,_],
        [w,m,w,_,_,_,_,_],
        [_,s,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_dead_ends(MegaBoard).

test(rule_dead_ends_monster_box, fail) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,w,_,_,_,_],
        [_,_,w,m,w,_,_,_],
        [_,_,_,w,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_dead_ends(MegaBoard).

test(rule_dead_ends_monster) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,w,_],
        [_,_,_,_,_,w,m,w],
        [_,_,_,_,_,_,s,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_dead_ends(MegaBoard).

test(rule_dead_ends_space, fail) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,w,_],
        [_,_,_,_,_,w,s,w],
        [_,_,_,_,_,_,s,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_dead_ends(MegaBoard).

test(rule_dead_ends_space_monster, fail) :-
    Board = [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,w,_],
        [_,_,_,_,_,w,s,w],
        [_,_,_,_,_,_,m,_],
        [_,_,_,_,_,_,_,_]
    ],
    build_mega_board(Board, MegaBoard),
    rule_dead_ends(MegaBoard).

:- end_tests(dead_ends).


:- begin_tests(hallways).

test(rule_hallways_valid) :-
    Board = [
        [w,w,s,m],
        [m,w,s,w],
        [s,w,s,w],
        [s,s,s,w]
    ],
    %setof(Board, rule_hallways(Board), Boards),
    findall(Board, rule_hallways(Board), Boards),
    length(Boards, 1).

test(rule_hallways_invalid, fail) :-
    Board = [
        [w,w,s,m],
        [w,s,s,w],
        [w,s,s,w],
        [w,w,w,w]
    ],
    rule_hallways(Board).

:- end_tests(hallways).
