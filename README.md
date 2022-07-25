﻿# dungeons-and-diagrams

install SWI-Prolog. This has been developed using 8.4.3, 64-bit.

Easiest way to test this is to run:
```
dungeons-and-diagrams> swipl

?- ['dnd_puzzles.plt'].
?- run_tests(dnd).
```

You can run these commands in sequence, if you are iterating on code changes:
```
?- ['dnd_puzzles.plt'], run_tests(dnd).
```

There are also a set of unit tests that are used for development. You can run the full suite using:
```
?- ['dnd.plt'], run_tests.
```

Or combine everything:
```
?- ['dnd.plt'], ['dnd_puzzles.plt'], run_tests.
```
