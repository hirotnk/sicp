-module (sort).
-export ([
          do/1
         ]).

do([]) -> [];
do([H|T]) -> add_to_sorted_list(H, do(T)).

add_to_sorted_list(A, []) ->
  [A];
add_to_sorted_list(A, L=[H|_]) when A < H ->
  [A|L];
add_to_sorted_list(A, [H|T]) ->
  [H|add_to_sorted_list(A,T)].

