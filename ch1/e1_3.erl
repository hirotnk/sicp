-module (e1_3).
-export ([do/3]).

do(X, Y, Z) when X > Z, Y > Z ->
  (X * X) + (Y * Y);
do(X, Y, Z) -> do(Y, Z, X).


