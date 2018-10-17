%% http://en.wikipedia.org/wiki/Horner's_method

-module(polynomial).
-export([do/2]).

do(_X, []) -> 0;
do(X, [H|Coeff]) -> H + (X * (do(X, Coeff))).

%3> polynomial:do(2, [1,3,0,5,0,1]).
%79
