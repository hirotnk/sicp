-module(estream).

-export([
  add/2,
  display/1,
  enumerate_interval/2,
  filter/2,
  for_each/2,
  from_list/1,
  is_stream_null/1,
  map/2,
  mmap/2,
  range/3,
  ref/2,
  stream_hd/1,
  stream_tl/1,
  to_list/1
 ]).

-export([
  ones/0,
  twos/0,
  fib/0,
  fib2/0,
  integer_from/1,
  integers/0,
  integers2/0,
  is_divisible/2,
  no_sevens/0,
  primes/0,
  sieve/1
 ]).

-define(DELAY(E), fun()-> E end).
-define(FORCE(F), F()).
-define(CONS_STREAM(H, S), [H | ?DELAY(S)]).
-define(NIL_STREAM, []).

is_stream_null(S) -> S == ?NIL_STREAM.

stream_hd(S) -> hd(S).
stream_tl(S) ->
  F = tl(S), % since ?FORCE is a macro
  ?FORCE(F).

ref(S, 0) -> stream_hd(S);
ref(S, N) -> ref(stream_tl(S), N - 1).

range(S, Start, End) when Start =< End ->
  range_in(S, 1, Start, End).
range_in(_S, Pos, _Start, End) when Pos > End ->
  ?NIL_STREAM;
range_in(S, Pos, Start, End) when Pos < Start ->
  range_in(stream_tl(S), Pos + 1, Start, End);
range_in(S, Pos, Start, End) when Start =< Pos, Pos =< End ->
  ?CONS_STREAM(
    stream_hd(S),
    range_in(stream_tl(S), Pos + 1, Start, End)).

from_list([]) -> ?NIL_STREAM;
from_list([H|T]) ->
  ?CONS_STREAM(H, from_list(T)).

to_list(S) when S == ?NIL_STREAM -> [];
to_list(S) ->
  [stream_hd(S) | to_list(stream_tl(S))].
  
map(F, S) ->
  case is_stream_null(S) of
    true -> ?NIL_STREAM;
    false ->
      ?CONS_STREAM(F(stream_hd(S)), map(F, stream_tl(S)))
  end.

for_each(F, S) ->
  case is_stream_null(S) of
    true -> ?NIL_STREAM;
    false ->
      F(stream_hd(S)),
      estream:for_each(F, estream:stream_tl(S))
  end.

display(S) ->
  case S of
    ?NIL_STREAM -> ok;
    _ ->
      io:format("~p ", [stream_hd(S)]),
      estream:display(stream_tl(S))
  end.

enumerate_interval(L, H) when L > H ->
  ?NIL_STREAM;
enumerate_interval(L, H) ->
  ?CONS_STREAM(L, enumerate_interval(L+1, H)).

filter(F, S) ->
  case S == ?NIL_STREAM of
    true -> ?NIL_STREAM;
    false ->
      case F(stream_hd(S)) of
        true -> ?CONS_STREAM(stream_hd(S), filter(F, stream_tl(S)));
        false -> filter(F, stream_tl(S))
      end
  end.


%% e.g)
%% 5> S0 = estream:from_list([1,2,3]).
%% [1|#Fun<estream.0.99758566>]
%% 6> S1 = estream:from_list([1,2,3]).
%% [1|#Fun<estream.0.99758566>]
%% 7> S2 = estream:from_list([1,2,3]).
%% [1|#Fun<estream.0.99758566>]
%% 8> SOS = estream:from_list([S0,S1,S2]).
%% [[1|#Fun<estream.0.99758566>]|#Fun<estream.0.99758566>]
%% 9> estream:to_list(estream:mmap(fun lists:sum/1, SOS)).
%% [3,6,9]
mmap(F, SOS0) ->
  case is_stream_null(stream_hd(SOS0)) of
    true -> ?NIL_STREAM;
    false ->
      % build a stream of tails of all stream elements
      SOS1 = map(fun stream_tl/1, SOS0),
      ?CONS_STREAM(
        % apply anonymous fun to the list of head elements
        F(to_list(map(fun stream_hd/1, SOS0))),
        mmap(F, SOS1))
  end.

add(S0, S1) ->
  %io:format("add:~p ", [stream_hd(S1)]),
  mmap(
    fun lists:sum/1,
    from_list([S0, S1])).

ones() ->
  ?CONS_STREAM(1, ones()).

twos() ->
  map(fun(X) -> X + 1 end, ones()).

integer_from(N) ->
  ?CONS_STREAM(N, integer_from(N + 1)).

integers() ->
  integer_from(1).

% extreamly slow
integers2() ->
  ?CONS_STREAM(1, add(ones(), integers2())).

fib_gen(X, Y) ->
  ?CONS_STREAM(
    X,
    fib_gen(Y, X + Y)).

fib() ->
  fib_gen(0,1).

% extreamly slow
fib2() ->
  ?CONS_STREAM(
    0,
    ?CONS_STREAM(
      1, add(fib2(), stream_tl(fib2())))).

is_divisible(X, Y) ->
  (X rem Y) == 0.

no_sevens() ->
  filter(fun(X) -> not is_divisible(X, 7) end, integers()).

sieve(S) ->
  ?CONS_STREAM(
    stream_hd(S),
    sieve(
      filter(
        fun(X) ->
          not is_divisible(X, stream_hd(S))
        end,
        stream_tl(S)))).

primes() ->
  sieve(integer_from(2)).

