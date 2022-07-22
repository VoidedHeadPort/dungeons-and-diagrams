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


:- begin_tests(lines).

test(rule_lines_simple_case, nondet) :-
    RowCounts = [8,7,6,5,4,3,2,1],
    ColCounts = [8,7,6,5,4,3,2,1],
    build_board(8, 8, Board),
    rule_lines(RowCounts, ColCounts, Board),
    Board == [
        [w,w,w,w,w,w,w,w],
        [w,w,w,w,w,w,w,s],
        [w,w,w,w,w,w,s,s],
        [w,w,w,w,w,s,s,s],
        [w,w,w,w,s,s,s,s],
        [w,w,w,s,s,s,s,s],
        [w,w,s,s,s,s,s,s],
        [w,s,s,s,s,s,s,s]
    ].

test(rule_lines_worst_case, nondet) :-
    RowCounts = [1,2,3,4,5,6,7,8],
    ColCounts = [1,2,3,4,5,6,7,8],
    build_board(8, 8, Board),
    rule_lines(RowCounts, ColCounts, Board),
    Board == [
        [s,s,s,s,s,s,s,w],
        [s,s,s,s,s,s,w,w],
        [s,s,s,s,s,w,w,w],
        [s,s,s,s,w,w,w,w],
        [s,s,s,w,w,w,w,w],
        [s,s,w,w,w,w,w,w],
        [s,w,w,w,w,w,w,w],
        [w,w,w,w,w,w,w,w]
    ].

:- end_tests(lines).


:- begin_tests(hallways).

test(rule_hallways_valid, nondet) :-
    Rows = [
        [w,w,s,m],
        [m,w,s,w],
        [s,w,s,w],
        [s,s,s,w]
    ],
    rule_hallways(Rows).

test(rule_hallways_invalid, fail) :-
    Rows = [
        [w,w,s,m],
        [w,s,s,w],
        [w,s,s,w],
        [w,w,w,w]
    ],
    rule_hallways(Rows).

:- end_tests(hallways).


:- begin_tests(dnd).

test(solve_puzzle_f_1, nondet) :-
    solve_puzzle('f.1').

test(solve_puzzle_1_1, nondet) :-
    solve_puzzle('1.1').

:- end_tests(dnd).
