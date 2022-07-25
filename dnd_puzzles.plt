:- [dnd_puzzles].


:- begin_tests(dnd).

test(solve_puzzle, [forall(puzzle(Code, _, _, _, _)), nondet]) :-
    time(solve_puzzle(Code)).

:- end_tests(dnd).
