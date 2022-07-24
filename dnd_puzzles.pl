:- [dnd].


solve_puzzle(Code) :-
    puzzle(Code, Name, RowCounts, ColCounts, Board),
    dnd(RowCounts, ColCounts, Board),
    print_board(Code, Name, RowCounts, ColCounts, Board).


puzzle('f.1', "adventurer's guide",
    [3,1,1,5,2,1],
    [4,1,4,1,2,1],
    [
        [_,_,_,_,_,c],
        [_,_,_,_,_,_],
        [m,_,_,_,_,_],
        [_,_,_,_,_,_],
        [_,_,_,_,_,m],
        [m,_,_,_,_,_]
    ]).

/*
puzzle('', "",
    [,,,,,,,],
    [,,,,,,,],
    [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_]
    ]).
 */

puzzle('1.1', "brightleaf iron mine",
    [3,2,5,3,4,1,4,4],
    [1,4,2,7,0,4,4,4],
    [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [_,_,m,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [_,c,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m]
    ]).

puzzle('1.2', "the secret tunnels",
    [4,4,4,4,3,4,2,6],
    [6,2,4,1,5,4,4,5],
    [
        [_,_,_,_,_,_,m,_],
        [_,_,_,_,_,_,_,_],
        [_,c,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,m,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [m,_,_,_,m,_,_,_]
    ]).

puzzle('1.3', "the graveyard of the vernal king",
    [5,2,2,1,5,3,2,5],
    [4,2,5,0,6,2,4,2],
    [
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,m,_,_,_,c,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [m,_,_,_,_,_,_,_],
        [_,_,_,m,_,m,_,m]
    ]).

puzzle('1.4', "the shifting walls",
    [6,2,5,3,2,5,2,6],
    [6,2,4,3,4,4,2,6],
    [
        [_,_,m,_,m,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [m,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,m],
        [m,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,m,_,m,_,_]
    ]).

puzzle('1.5', "the fetid sewers",
    [0,7,2,4,2,2,7,0],
    [2,4,4,3,2,3,4,2],
    [
        [m,_,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [_,m,_,_,_,_,_,m],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,m,_],
        [_,_,_,_,_,_,_,_],
        [m,_,_,_,_,_,_,m]
    ]).

puzzle('1.6', "the dais of the sun god",
    [2,3,5,4,1,4,5,5],
    [2,3,3,4,4,3,4,6],
    [
        [m,_,_,_,_,_,_,_],
        [_,_,_,_,_,m,_,m],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [m,_,_,_,_,c,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [m,_,_,m,_,_,_,_]
    ]).

puzzle('1.7', "the corination of the lich",
    [3,1,2,6,6,1,4,3],
    [4,1,3,4,3,3,2,6],
    [
        [_,_,_,_,_,m,_,m],
        [_,_,_,_,_,_,_,_],
        [_,c,_,_,_,_,_,_],
        [_,_,_,_,_,_,m,_],
        [_,m,_,_,_,_,_,_],
        [m,_,_,_,_,_,_,_],
        [_,_,_,_,c,_,_,_],
        [_,_,m,_,_,_,_,_]
    ]).

puzzle('1.8', "the demon marquis of lust",
    [1,3,3,6,0,6,3,4],
    [1,5,1,4,3,5,4,3],
    [
        [_,_,_,_,_,_,m,_],
        [_,_,_,_,_,_,_,_],
        [_,_,c,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [_,_,_,_,_,_,_,_],
        [m,_,_,_,m,_,_,_],
        [_,_,_,_,_,m,_,_]
    ]).