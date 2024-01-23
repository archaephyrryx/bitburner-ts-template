:- module parensan.
:- interface.
:- use_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- use_module list, solutions, string, int.

:- type balance === Left(int) | Even | Right(int).

:- func bias_left(balance) = balance is det.
bias_left(Even) = Left(1).
bias_left(Right(1)) = Even.
bias_left(Right(N)) = Right(N - 1).
bias_left(Left(N)) = Left(N + 1).

:- func bias_right(balance) = balance is det.
bias_right(Even) = Right(1).
bias_right(Left(1)) = Even.
bias_right(Left(N)) = Left(N - 1).
bias_right(Right(N)) = Right(N + 1).

:- pred balanced(list(char)::in) is semidet.
balanced(Chars) :- balanced_bias(Chars, Even).

:- pred balanced_bias(list(char), balance).
:- mode balanced_bias(in, in) is semidet.

balanced_bias([], Even).
balanced_bias([X | Xs], Balance) :-
    (   Balance = Right(_) => false
    ; X = '(' => balanced_bias(Xs, bias_left(Balance))
    ; X = ')' => balanced_bias(Xs, bias_right(Balance))
    ; balance_biased(Xs, Balance)
    ).

main(!IO) :-
    io.read_line(Res, !IO),
    ( Res = ok(Line) => (

    )
    ; Res = err(Err) => io.format("Error: %s", [s(io.error_message(Err))], !IO)
    ; Res = eof => io.print("No input provided\n", !IO)
    ).
