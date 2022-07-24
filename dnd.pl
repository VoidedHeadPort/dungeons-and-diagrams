:- use_module(library(clpfd)).

dnd(RowCounts, ColCounts, Rows) :-
    length(RowCounts, NumRows),
    length(ColCounts, NumCols),
    build_board(NumRows, NumCols, Rows),
    build_mega_board(Rows, MegaRows),
    % Work through the MegaRows. Use corners at:
    % [0,0], [0,1], [0,1], etc
    % [1,0], [1,1], etc
    % [2,0], etc
    % - Some of these rules could be applied before filling the rows (& columns)
    %   For example, I often try to fill out the chest room before rows
    % - 5x5 rule for chests
    rule_chests(MegaRows),
    % Build columns out of the Rows (instead of rows)
    rule_lines(RowCounts, ColCounts, Rows),
    % - 3x3 rule for monsters (and invalid dead ends)
    rule_dead_ends(MegaRows),
    % - 2x2 rule for invalid corridors
    rule_hallways(Rows).


build_board(NumRows, NumCols, Board) :-
    % Board is NumRows x NumCols
    length(Board, NumRows),
    % each row is of NumCols length
    % most examples use same_length/2 since the ordering of arguments in length/2 doesn't work well with maplist
    Board = [Row|Rows],
    length(Row, NumCols),
    maplist(same_length(Row), Rows).


fill(_, []) :- !.
fill(Element, [Element|Tail]) :-
    fill(Element, Tail).

% count(Element, Count, List)
% Count & fill List with the Element - unbound items will be filled with 's'
count(Element, Count, List) :-
    length(List, Count), !,
    fill(Element, List).
count(Element, Count, [Element|Tail]) :-
    % succ(essor?) is similar to TailCount is Count + 1
    % but with an extra check that TailCount >= 0
    succ(TailCount, Count),
    count(Element, TailCount, Tail).
count(Element, Count, [Head|Tail]) :-
    var(Head),
    Head = s,
    count(Element, Count, Tail).
count(Element, Count, [Head|Tail]) :-
    nonvar(Head),
    Element \= Head,
    count(Element, Count, Tail).


build_mega_board(Board, MegaBoard) :-
    Board = [Row|_],
    length(Row, NumCols),
    NumMegaCols is NumCols + 2,
    length(WallRow, NumMegaCols),
    fill(w, WallRow),
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
    count_pack(SortedPack).

pack([], [], []).
pack([LineCount|LineCounts], [Line|Lines], [line(Count, LineCount, Line)|Pack]) :-
    findall(Line, count(w, LineCount, Line), AllLines),
    length(AllLines, Count),
    pack(LineCounts, Lines, Pack).

count_pack([]).
count_pack([line(_, LineCount, Line)|Rest]) :-
    count(w, LineCount, Line),
    count_pack(Rest).


list_tail([_|Tail], Tail).

list_tail(Num, List, Tail) :-
    length(Head, Num),
    append(Head, Tail, List).


rule_dead_ends(Rows) :-
    rule_dead_ends(true, Rows).

rule_dead_ends(IsFirstRow, [Row1, Row2, Row3|Rows]) :-
    Row1 = [_,N,_|_],
    Row2 = [W,C,E|_],
    Row3 = [_,S,_|_],
    !,
    (   maplist(nonvar, [N,E,S,W]),
        nonvar(C),
        C = m
    ->  dead_end([Row1, Row2, Row3])
    ;   maplist(nonvar, [N,E,S,W]),
        nonvar(C),
        C = s
    ->  \+ dead_end([Row1, Row2, Row3])
    ;   true
    ),
    rule_dead_ends(false, [Row2, Row3|Rows]),
    (   IsFirstRow == true
    ->  maplist(list_tail, [Row1, Row2, Row3|Rows], Tail),
        rule_dead_ends(IsFirstRow, Tail)
    ;   true
    ).

rule_dead_ends(_, _).

dead_end([Row1, Row2, Row3]) :-
    Row1 = [_,w,_|_],
    Row2 = [w,C,w|_],
    Row3 = [_,w,_|_],
    nonvar(C),
    C \= m,
    !.

dead_end([Row1, Row2, Row3]) :-
    Row1 = [_,s,_|_],
    Row2 = [w,_,w|_],
    Row3 = [_,w,_|_],
    !.

dead_end([Row1, Row2, Row3]) :-
    Row1 = [_,w,_|_],
    Row2 = [w,_,s|_],
    Row3 = [_,w,_|_],
    !.

dead_end([Row1, Row2, Row3]) :-
    Row1 = [_,w,_|_],
    Row2 = [w,_,w|_],
    Row3 = [_,s,_|_],
    !.

dead_end([Row1, Row2, Row3]) :-
    Row1 = [_,w,_|_],
    Row2 = [s,_,w|_],
    Row3 = [_,w,_|_],
    !.


% We don't have a terminating clause since it should fail when we reach []
detect_chest([Head|_]) :-
    nonvar(Head),
    !,
    Head == c.

detect_chest([_|Tail]) :-
    detect_chest(Tail).


% rule_chest_rooms?
rule_chests(Rows) :-
    rule_chests(true, true, Rows).

rule_chests(IsFirstRow, IsFirstCol, [Row|Rows]) :-
    chest_zone(IsFirstRow, IsFirstCol, [Row|Rows], ChestZone),
    !,
    (   detect_chest(ChestZone)
    ->  chest_room([Row|Rows])
    ;   true
    ),
    rule_chests(false, IsFirstCol, Rows),
    (   IsFirstRow == true
    ->  maplist(list_tail, [Row|Rows], Tail),
        rule_chests(IsFirstRow, false, Tail)
    ;   true
    ).

% Cheat with ! for the other rules?
rule_chests(_, _, _).

% chest_zone(IsFirstRow, IsFirstCol, Rows, ChestZone)
chest_zone(true, true, [Row1, Row2, Row3, Row4, Row5|_], [B2,C2,D2,B3,C3,D3,B4,C4,D4]) :-
    Row1 = [ _, _, _, _, _|_],
    Row2 = [ _,B2,C2,D2, _|_],
    Row3 = [ _,B3,C3,D3, _|_],
    Row4 = [ _,B4,C4,D4, _|_],
    Row5 = [ _, _, _, _, _|_].

chest_zone(true, false, [Row1, Row2, Row3, Row4, Row5|_], [D2,D3,D4]) :-
    Row1 = [ _, _, _, _, _|_],
    Row2 = [ _, _, _,D2, _|_],
    Row3 = [ _, _, _,D3, _|_],
    Row4 = [ _, _, _,D4, _|_],
    Row5 = [ _, _, _, _, _|_].

chest_zone(false, true, [Row1, Row2, Row3, Row4, Row5|_], [B4,C4,D4]) :-
    Row1 = [ _, _, _, _, _|_],
    Row2 = [ _, _, _, _, _|_],
    Row3 = [ _, _, _, _, _|_],
    Row4 = [ _,B4,C4,D4, _|_],
    Row5 = [ _, _, _, _, _|_].

chest_zone(false, false, [Row1, Row2, Row3, Row4, Row5|_], [D4]) :-
    Row1 = [ _, _, _, _, _|_],
    Row2 = [ _, _, _, _, _|_],
    Row3 = [ _, _, _, _, _|_],
    Row4 = [ _, _, _,D4, _|_],
    Row5 = [ _, _, _, _, _|_].

chest_room([Row1, Row2, Row3, Row4, Row5|_]) :-
    Row1 = [_, B1,C1,D1,_|_],
    Row2 = [A2,B2,C2,D2,E2|_],
    Row3 = [A3,B3,C3,D3,E3|_],
    Row4 = [A4,B4,C4,D4,E4|_],
    Row5 = [_, B5,C5,D5,_|_],
    detect_chest([B2,C2,D2,B3,C3,D3,B4,C4,D4]),
    count(r, 8, [B2,C2,D2,B3,C3,D3,B4,C4,D4]),
    count(w, 11, [B1,C1,D1,E2,E3,E4,D5,C5,B5,A4,A3,A2]).

chest_room([Row1, Row2, Row3, Row4, Row5|Rows]) :-
    Row1 = [_, _, _, _, _|_],
    Row2 = [_, _, _, _, _|_],
    Row3 = [_, B3,C3,D3,_|_],
    Row4 = [_, B4,C4,D4,_|_],
    Row5 = [_, _, _, _, _|_],
    detect_chest([B3,C3,D3,B4,C4,D4]),
    chest_room([Row2, Row3, Row4, Row5|Rows]).

chest_room([Row1, Row2, Row3, Row4, Row5|Rows]) :-
    Row1 = [_, _, _, _, _|_],
    Row2 = [_, _, C2,D2,_|_],
    Row3 = [_, _, C3,D3,_|_],
    Row4 = [_, _, C4,D4,_|_],
    Row5 = [_, _, _, _, _|_],
    detect_chest([C2,D2,C3,D3,C4,D4]),
    maplist(list_tail, [Row1, Row2, Row3, Row4, Row5|Rows], Rest),
    chest_room(Rest).


rule_hallways([_]).
rule_hallways([[_]|_]).
rule_hallways([Row1, Row2|Rows]) :-
    Row1 = [A1,B1|_],
    Row2 = [A2,B2|_],
    rule_hallways_2x2([[A1,B1],[A2,B2]]),
    rule_hallways([Row2|Rows]),
    % Remove the first element from each Row in Rows.
    maplist(list_tail, [Row1, Row2|Rows], Rest),
    rule_hallways(Rest).

rule_hallways_2x2([Row1, Row2]) :-
    Row1 = [s,s],
    Row2 = [s,s],
    !, fail.
rule_hallways_2x2(_).


print_board(Board) :-
    same_length(Board, RowCounts),
    Board = [Row|_],
    same_length(Row, ColCounts),
    fill('_', RowCounts),
    fill('_', ColCounts),
    print_board('_', '_', RowCounts, ColCounts, Board).

print_board(Code, Name, RowCounts, ColCounts, Board) :-
    nl,
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

print_element(r) :-
    write('_'),
    !.

print_element(s) :-
    write('_'),
    !.

print_element(Element) :-
    write(Element).
