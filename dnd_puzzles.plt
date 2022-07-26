:- [dnd_puzzles].


:- begin_tests(dnd).

test(solve_puzzle, [forall(puzzle(Code, _, _, _, _)), true(Length =:= 1)]) :-
    %time(setof(Board, solve_puzzle(Code, Board), Boards)),
    time(findall(Board, solve_puzzle(Code, Board), Boards)),
    length(Boards, Length).

:- end_tests(dnd).
