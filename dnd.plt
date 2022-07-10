:- [dnd].


:- begin_tests(examples).

test(add) :-
    A is 1 + 2,
    A =:= 3.

test(member, [nondet]) :-
    member(b, [a,b,c]).

:- end_tests(examples).


:- begin_tests(count).

test(count_10_w_10) :-
    Row = [_,_,_,_,_,_,_,_,_,_],
    count(w,10,Row),
    Row = [w,w,w,w,w,w,w,w,w,w].

test(count_10_w_9, all(Row == [
        [w,w,w,w,w,w,w,w,w,'_'],
        [w,w,w,w,w,w,w,w,'_',w],
        [w,w,w,w,w,w,w,'_',w,w],
        [w,w,w,w,w,w,'_',w,w,w],
        [w,w,w,w,w,'_',w,w,w,w],
        [w,w,w,w,'_',w,w,w,w,w],
        [w,w,w,'_',w,w,w,w,w,w],
        [w,w,'_',w,w,w,w,w,w,w],
        [w,'_',w,w,w,w,w,w,w,w],
        ['_',w,w,w,w,w,w,w,w,w]
    ])) :-
    Row = [_,_,_,_,_,_,_,_,_,_],
    count(w,9,Row).

test(count_10_w_1, all(Row == [
    [w,'_','_','_','_','_','_','_','_','_'],
    ['_',w,'_','_','_','_','_','_','_','_'],
    ['_','_',w,'_','_','_','_','_','_','_'],
    ['_','_','_',w,'_','_','_','_','_','_'],
    ['_','_','_','_',w,'_','_','_','_','_'],
    ['_','_','_','_','_',w,'_','_','_','_'],
    ['_','_','_','_','_','_',w,'_','_','_'],
    ['_','_','_','_','_','_','_',w,'_','_'],
    ['_','_','_','_','_','_','_','_',w,'_'],
    ['_','_','_','_','_','_','_','_','_',w]
])) :-
    Row = [_,_,_,_,_,_,_,_,_,_],
    count(w,1,Row).

test(count_10_w_0) :-
    Row = [_,_,_,_,_,_,_,_,_,_],
    count(w,0,Row),
    Row = ['_','_','_','_','_','_','_','_','_','_'].

test(count_8_w_8) :-
    Row = [_,_,_,_,_,_,_,_],
    count(w,8,Row),
    Row = [w,w,w,w,w,w,w,w].

test(count_8_w_0) :-
    Row = [_,_,_,_,_,_,_,_],
    count(w,0,Row),
    Row = ['_','_','_','_','_','_','_','_'].

:- end_tests(count).


:- begin_tests(board).

% We can't use Board == [...] because of the free variables.
% Use Board = [...] to check it unifies.
test(build_board_6x6_empty) :-
    build_board(6,6,Board),
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
    build_board(6,6,Board),
    Board = [
        [_,_,_,_,_,c],
        [_,_,_,_,_,_],
        [m,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,m],
        [m,_,_,_,_,_]
    ].

test(build_board_8x8_empty) :-
    build_board(8,8,Board),
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
    build_board(8,8,Board),
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
    RowCounts = [1,2,3,4,5,6,7,8],
    ColCounts = [7,6,5,4,3,2,1,0],
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
    build_mega_board(RowCounts,ColCounts,Board,MegaRowCounts,MegaColCounts,MegaBoard),
    MegaRowCounts == [10, 3, 4, 5, 6, 7, 8, 9,10,10],
    MegaColCounts == [10, 9, 8, 7, 6, 5, 4, 3, 2,10],
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

test(build_mega_row_counts) :-
    RowCounts = [0,1,2,3,4,5,6,7,8,9],
    build_mega_row_counts(RowCounts,MegaRowCounts),
    MegaRowCounts = [12, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12].

:- end_tests(mega_board).