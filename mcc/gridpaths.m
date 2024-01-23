:- module gridpaths.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, require, list, string.

:- func fact(int) = int is semidet.
fact(N) =
  ( N < 0 -> unexpected("gridpaths", "factorial of negative")
  ; N >= 0, N < 2 -> 1
  ; N * fact(N - 1)
  ).


:- pred choose(int::in, int::in, int::out) is semidet.

choose(N, K, X) :-
  K >= 0,
  K =< N,
  N > 0,
  Num = fact(N),
  Den = fact(K) * fact(N - K),
  X = Num / Den.

:- pred paths(int::in, int::in, int::out) is semidet.
paths(N, M, X) :-
  N > 0,
  M > 0,
  choose(N + M - 2, N - 1, X).

:- func paths(int, int) = int is det.
paths(N, M) = X :-
  ( paths(N, M, X)
  ; unexpected("gridpaths", "bad arguments to paths/2")
  ).


main(!IO) :-
    N = 5 % get_int(!IO),
  , M = 9 % get_int(!IO),
  , paths(N, M) = Out
  , io.format("%d\n", [i(Out)], !IO)
  .


