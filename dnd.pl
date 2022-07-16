dnd(RowCounts,ColCounts,Board) :-
    length(RowCounts,NumRows),
    length(ColCounts,NumCols),
    build_board(NumRows,NumCols,Board),
    build_mega_board(RowCounts,ColCounts,Board,_MegaRowCounts,_MegaColCounts,_MegaBoard).
    % Build columns out of the Board (instead of rows)
    % Sum the number of used cells (or potential wall combos) [chests use 3 cells]
    % Sort by the least options to most options (combine rows & columns and their counts)
    % Fill in rows (& columns) with w & s, decrementing count for each w [count(w,Count,List)]
    % Work through the MegaBoard. Use corners at:
    % [0,0], [0,1], [0,1], etc
    % [1,0], [1,1], etc
    % [2,0], etc
    % - Some of these rules could be applied before filling the rows (& columns)
    %   For example, I often try to fill out the chest room before rows
    % - 5x5 rule for chests
    % - 3x3 rule for monsters (and invalid dead ends)
    % - 2x2 rule for invalid corridors


build_board(NumRows,NumCols,Board) :-
    % Board is NumRows x NumCols
    length(Board,NumRows),
    % each row is of NumCols length
    % most examples use same_length/2 since the ordering of arguments in length/2 doesn't work well with maplist
    Board = [Row|Rows],
    length(Row,NumCols),
    maplist(same_length(Row),Rows).


fill(_,[]).

fill(Element,[Element|Rest]) :-
    fill(Element,Rest).

% count(Element, Count, List)
% Count & fill List with the Element - unbound items will be filled with 's'
count(Element,Count,List) :-
    length(List,Count),
    fill(Element,List),
    !.

count(Element,Count,[Element|Tail]) :-
    % succ(essor?) is similar to TailCount is Count + 1
    % but with an extra check that TailCount >= 0
    succ(TailCount,Count),
    count(Element,TailCount,Tail).

count(Element,Count,[s|Tail]) :-
    count(Element,Count,Tail).


build_mega_board(RowCounts,ColCounts,Board,MegaRowCounts,MegaColCounts,MegaBoard) :-
    build_mega_row_counts(RowCounts,MegaRowCounts),
    build_mega_row_counts(ColCounts,MegaColCounts),
    length(MegaColCounts,NumMegaCols),
    length(WallRow,NumMegaCols),
    count(w,NumMegaCols,WallRow),
    build_mega_rows(Board,MegaRows),
    append([WallRow|MegaRows],[WallRow],MegaBoard).

build_mega_row_counts(RowCounts,MegaRowCounts) :-
    length(RowCounts,NumRows),
    % Build with the original NumRows then add 2 to everything
    append([NumRows|RowCounts],[NumRows],TempRowCounts),
    maplist(plus(2),TempRowCounts,MegaRowCounts).

build_mega_rows(Rows,MegaRows) :-
    maplist(build_mega_row,Rows,MegaRows).

build_mega_row(Row,MegaRow) :-
    append([w|Row],[w],MegaRow).


print_board(Code,Name,RowCounts,ColCounts,Board) :-
    write('   '),writeln(Code),
    write('   '),writeln(Name),
    write('   '),print_col_counts(ColCounts),
    print_row_counts(RowCounts,Board).

print_col_counts([]) :-
    nl.

print_col_counts([ColCount|ColCounts]) :-
    write(ColCount),
    write(' '),
    print_col_counts(ColCounts).

print_row_counts([],[]) :-
    nl.

print_row_counts([RowCount|RowCounts],[Row|Rows]) :-
    write(RowCount),
    write(' |'),
    print_row(Row),
    print_row_counts(RowCounts,Rows).

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
        _,
        [m|_],
        _,
        [_,_,_,_,_,m],
        [m|_]
    ]).

puzzle('1.1', "brightleaf iron mine",
    [3,2,5,3,4,1,4,4],
    [1,4,2,7,0,4,4,4],
    [
        _,
        [_,_,_,_,_,_,_,m],
        [_,_,m|_],
        [_,_,_,_,_,_,_,m],
        _,
        [_,c,_,_,_,_,_,m],
        _,
        [_,_,_,_,_,_,_,m]
    ]).


