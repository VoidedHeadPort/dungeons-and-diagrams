dnd(RowCounts,ColCounts,Board) :-
    length(RowCounts,NumRows),
    length(ColCounts,NumCols),
    build_board(NumRows,NumCols,Board).
    %build_mega_board(RowCounts,ColCounts,Board,MegaRowCounts,MegaColCounts,MegaBoard),


build_board(NumRows,NumCols,Board) :-
    % Board is NumRows x NumCols
    length(Board,NumRows),
    % each row is of NumCols length
    build_rows(Board,NumCols).


% could we use maplist instead?
build_rows([],_).

build_rows([Row|Rows],NumCols) :-
    length(Row,NumCols),
    build_rows(Rows,NumCols).


print_board(Code,Name,RowCounts,ColCounts,Board) :-
    write('   '),writeln(Code),
    write('   '),writeln(Name),
    write('   '),print_col_count(ColCounts),
    print_row_count(RowCounts,Board).

print_col_count([]) :-
    nl.

print_col_count([ColCount|ColCounts]) :-
    write(ColCount),
    write(' '),
    print_col_count(ColCounts).

print_row_count([],[]) :-
    nl.

print_row_count([RowCount|RowCounts],[Row|Rows]) :-
    write(RowCount),
    write(' |'),
    print_row(Row),
    print_row_count(RowCounts,Rows).

print_row([]) :-
    nl.

print_row([Element|Elements]) :-
    print_element(Element),
    write('|'),
    print_row(Elements).

print_element(Element) :-
    var(Element),
    write('_'),
    !.

print_element(Element) :-
    write(Element).


solve_puzzle(Code) :-
    puzzle(Code,Name,RowCounts,ColCounts,Board),
    dnd(RowCounts,ColCounts,Board),
    print_board(Code,Name,RowCounts,ColCounts,Board).


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


