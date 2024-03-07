:- module gridpaths.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, require, list, string, uint64, pprint.

:- type integer == uint64.

:- func fact(integer) = integer is det.
fact(N) =
  ( N < uint64.cast_from_int(2) -> uint64.cast_from_int(1)
  ; N * fact(N - uint64.cast_from_int(1))
  ).


:- pred choose(integer::in, integer::in, integer::out) is semidet.

choose(N, K, X) :-
  K =< N,
  Num = fact(N),
  Den = fact(K) * fact(N - K),
  X = Num / Den.

:- pred paths(integer::in, integer::in, integer::out) is semidet.
paths(N, M, X) :-
  choose(N + M - uint64.cast_from_int(2), N - uint64.cast_from_int(1), X).

:- func paths(integer, integer) = integer is det.
paths(N, M) = X :-
  ( paths(N, M, X)
  ; unexpected("gridpaths", "bad arguments to paths/2")
  ).

:- pred get_int_pair(integer::out, integer::out, io::di, io::uo) is det.
get_int_pair(X, Y, !IO) :-
    io.read(Res, !IO),
  ( Res = ok([N, M]) -> X = uint64.cast_from_int(N), Y = uint64.cast_from_int(M)
  ; unexpected("get_int_pair", "bad input")
  ).

main(!IO) :-
    get_int_pair(N, M, !IO)
  , pprint.write(80, N, !IO)
  , pprint.write(80, M, !IO)
  , paths(N, M) = Out
  , pprint.write(80, Out, !IO)
  .
