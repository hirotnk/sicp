-module (coin).
-export ([
          count1/1,
          count2/1
         ]).


count1(0) -> 0;
count1(N) -> do50(N).

do50(0) -> 1;
do50(N) when N > 1 -> do50 (N - 50) + do25 (N - 25) + do10 (N - 10) + do5 (N - 5) + 1;
do50(_) -> 0.

do25(0) -> 1;
do25(N) when N > 1 -> do25 (N - 25) + do10 (N - 10) + do5 (N - 5) + 1;
do25(_) -> 0.

do10(0) -> 1;
do10(N) when N > 1 -> do10 (N - 10) + do5 (N - 5) + 1;
do10(_) -> 0.

do5(0) -> 1;
do5(N) when N > 1 -> do5 (N - 5) + 1;
do5(_) -> 0.


count2(0) -> 0;
count2(N) -> count2(N, [50,25,10,5,1]).

count2(_,[]) -> 0;
count2(0,_CL) -> 1;
count2(N,_CL) when N < 0 -> 0;
count2(N,[H]) -> count2(N - H, [H]);
count2(N,[H|T]=CL) -> count2(N - H, CL) + count2(N, T).


