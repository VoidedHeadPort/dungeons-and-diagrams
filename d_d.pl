d_d(RowCounts,ColCounts,Board) :-
    length(RowCounts,NumRows),
    length(ColCounts,NumCols),
    build_board(NumRows,NumCols,Board).


build_board(NumRows,NumCols,Board) :-
    % Board is NumRows x NumCols
    length(Board,NumRows),
    % each row is of NumCols length
    build_rows(Board,NumCols).


% could we use maplist instead?
build_rows([],_).

build_rows([Row|RemainingRow],NumCols) :-
    length(Row,NumCols),
    build_rows(RemainingRow,NumCols).


print_board(Code,Name,RowCounts,ColCounts,Board) :-
    write("   "),writeln(Code),
    write("   "),writeln(Name),
    write("   "),print_col_count(ColCounts),
    print_row_count(RowCounts,Board).

print_col_count([]) :-
    nl.

print_col_count([ColCount|RemainingColCounts]) :-
    write(ColCount),
    write(" "),
    print_col_count(RemainingColCounts).

print_row_count([],[]).

print_row_count([RowCount|RemainingRowCounts],[Row|RemainingRows]) :-
    write(RowCount),
    write(" |"),
    print_row(Row),
    print_row_count(RemainingRowCounts,RemainingRows).

print_row([]) :-
    nl.

print_row([Element|RemainingElements]) :-
    print_element(Element),
    write("|"),
    print_row(RemainingElements).

print_element(Element) :-
    var(Element),
    write("_"),
    !.

print_element(Element) :-
    write(Element).


solve_puzzle(Code) :-
    puzzle(Code,Name,RowCounts,ColCounts,Board),
    d_d(RowCounts,ColCounts,Board),
    print_board(Code,Name,RowCounts,ColCounts,Board).


start :-
    solve_puzzle("f.1").


puzzle("f.1", "adventurer's guide",
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


puzzle("1.1", "brightleaf iron mine",
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


