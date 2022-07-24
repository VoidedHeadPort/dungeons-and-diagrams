:- [dnd_puzzles].


:- begin_tests(dnd).

test(solve_puzzle_f_1, nondet) :-
    solve_puzzle('f.1').

test(solve_puzzle_1_1, nondet) :-
    solve_puzzle('1.1').

:- end_tests(dnd).
