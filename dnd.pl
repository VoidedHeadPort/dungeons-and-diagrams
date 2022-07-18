:- use_module(library(clpfd)).

dnd(RowCounts, ColCounts, Rows) :-
    length(RowCounts, NumRows),
    length(ColCounts, NumCols),
    build_board(NumRows, NumCols, Rows),
    build_mega_board(Rows, _MegaRows),
    % Build columns out of the Rows (instead of rows)
    rule_lines(RowCounts, ColCounts, Rows).
    % Work through the MegaRows. Use corners at:
    % [0,0], [0,1], [0,1], etc
    % [1,0], [1,1], etc
    % [2,0], etc
    % - Some of these rules could be applied before filling the rows (& columns)
    %   For example, I often try to fill out the chest room before rows
    % - 5x5 rule for chests
    % - 3x3 rule for monsters (and invalid dead ends)
    % - 2x2 rule for invalid corridors


build_board(NumRows, NumCols, Board) :-
    % Board is NumRows x NumCols
    length(Board, NumRows),
    % each row is of NumCols length
    % most examples use same_length/2 since the ordering of arguments in length/2 doesn't work well with maplist
    Board = [Row|Rows],
    length(Row, NumCols),
    maplist(same_length(Row), Rows).


fill(_, []).

fill(Element, [Element|Rest]) :-
    fill(Element, Rest).

% count(Element, Count, List)
% Count & fill List with the Element - unbound items will be filled with 's'
count(_, 0, []).

count(Element, Count, [Element|Tail]) :-
    % succ(essor?) is similar to TailCount is Count + 1
    % but with an extra check that TailCount >= 0
    succ(TailCount, Count),
    count(Element, TailCount, Tail).

count(Element, Count, [s|Tail]) :-
    count(Element, Count, Tail).

count(Element, Count, [Head|Tail]) :-
    nonvar(Head),
    Head \= Element,
    count(Element, Count, Tail).


build_mega_board(Board, MegaBoard) :-
    Board = [Row|_],
    length(Row, NumCols),
    NumMegaCols is NumCols + 2,
    length(WallRow, NumMegaCols),
    count(w, NumMegaCols, WallRow),
    maplist(build_mega_row, Board, MegaRows),
    append([WallRow|MegaRows], [WallRow], MegaBoard).

build_mega_row(Row, MegaRow) :-
    append([w|Row], [w], MegaRow).


rule_lines(RowCounts, ColCounts, Rows) :-
    transpose(Rows, Cols),
    append(RowCounts, ColCounts, LineCounts),
    append(Rows, Cols, Lines),
    % Sum the number of used cells (or potential wall combos) [chests use 3 cells]
    pack(LineCounts, Lines, Pack),
    % Sort by the least options to most options (combine rows & columns and their counts)
    sort(Pack, SortedPack),
    % Fill in rows (& columns) with w & s, decrementing count for each w [count(w, Count, List)]
    rule_lines_(SortedPack).

pack([], [], []).
pack([LineCount|LineCounts], [Line|Lines], [line(Count, LineCount, Line)|Pack]) :-
    findall(Line, count(w, LineCount, Line), AllLines),
    length(AllLines, Count),
    pack(LineCounts, Lines, Pack).

rule_lines_([]).
rule_lines_([line(_, LineCount, Line)|Rest]) :-
    count(w, LineCount, Line),
    rule_lines_(Rest).


print_board(Code, Name, RowCounts, ColCounts, Board) :-
    write('   '), writeln(Code),
    write('   '), writeln(Name),
    write('   '), print_col_counts(ColCounts),
    print_row_counts(RowCounts, Board).

print_col_counts([]) :-
    nl.

print_col_counts([ColCount|ColCounts]) :-
    write(ColCount),
    write(' '),
    print_col_counts(ColCounts).

print_row_counts([], []) :-
    nl.

print_row_counts([RowCount|RowCounts], [Row|Rows]) :-
    write(RowCount),
    write(' |'),
    print_row(Row),
    print_row_counts(RowCounts, Rows).

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
    puzzle(Code, Name, RowCounts, ColCounts, Board),
    dnd(RowCounts, ColCounts, Board),
    print_board(Code, Name, RowCounts, ColCounts, Board).


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


