-module (nqueen).
-export ([
          do/1
         ]).

%% Erlang version of SICP 2-42
do(N) -> do(N,N).

do(K,_N) when K =:= 0 -> [[]];
do(K,N) ->
  [QueensTocheck ||
      QueensTocheck <-
        lists:flatmap(
          fun(Queens) ->
              lists:map(
                fun(Row) -> Queens ++ [{Row, K}] end,
                lists:seq(1, N))
          end, do(K - 1, N)),
      is_safe(QueensTocheck, K)
  ].

is_safe(Candidate, K) ->
  Q = lists:nth(K, Candidate),
  is_safe_in(Candidate -- [Q], Q).

is_safe_in([],_Q) -> true;
is_safe_in([{R1,_}|_Queens],{R1,_}) -> false;
is_safe_in([{_,C1}|_Queens],{_,C1}) -> false;
is_safe_in([{R1,C1}|Queens],Q={R2,C2}) ->
  if
    (abs(R1 - R2) == abs(C1 - C2)) -> false;
    true -> is_safe_in(Queens,Q)
  end.

