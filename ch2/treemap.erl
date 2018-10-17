-module(treemap).
-export([do/2,do_without_map/2]).

do(F, Tree) ->
  lists:map(
    fun
      (SubTree) when is_list(SubTree) ->
        do(F, SubTree)
     ;(Any) ->
        F(Any)
    end, Tree).

%% Eshell V5.10.4  (abort with ^G)
%% 1> c(treemap).
%% {ok,treemap}
%% 2> treemap:do(fun(A) -> A * 2 end, [10,[20,30],40, [50, 60]]).
%% [20,"(<",80,"dx"]
%% 3> treemap:do(fun(A) -> A * 100 end, [10,[20,30],40, [50, 60]]).
%% [1000,[2000,3000],4000,[5000,6000]]
%% 4> treemap:do(fun(A) -> A * 100 end, [10,[20,[70,80],30],40, [50, 60]]).
%% [1000,[2000,[7000,8000],3000],4000,[5000,6000]]
%% 5>

do_without_map(_F, []) -> [];
do_without_map(F, Tree) when is_list(Tree) ->
  [do_without_map(F, hd(Tree)) |
   do_without_map(F, tl(Tree))];
do_without_map(F, Node) -> F(Node).

%% 6> treemap:do_without_map(fun(A) -> A * 100 end, [10,[20,[70,80],30],40, [50, 60]]).
%% [1000,[2000,[7000,8000],3000],4000,[5000,6000]]


