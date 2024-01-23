:- module trader.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module string, list, int, maybe.

:- type trade_limit ---> no_limit ; limit(int).

:- pred deduct(trade_limit::in, trade_limit::out) is det.
deduct(no_limit, no_limit).
deduct(limit(N), limit(N-1)).

:- func deduct(trade_limit) = trade_limit.
deduct(L0) = L1 :- deduct(L0, L1).

:- type trade_state ---> start(trade_limit)
    ; trading(history :: history, balance :: int, trades :: trade_limit)
    ; end(int).

:- type history ---> holding(int) ; waiting(maybe(int)).

:- type decision_tree ---> tip ; branch(state :: trade_state, act :: decision_tree, wait :: decision_tree).

:- pred run_state(trade_state::in, list(int)::in, decision_tree::out) is det.

run_state(start(Trades), Ps, Tree) :- run_state(trading(waiting(no), 0, Trades), Ps, Tree).
run_state(State @ end(_), _, branch(State, tip, tip)).
run_state(trading(_, Balance, _), [], branch(end(Balance), tip, tip)).
run_state(State @ trading(History, Balance, Trades), [P1 | Pt], Tree) :-
  ( Trades = limit(0) -> Tree = branch(end(Balance), tip, tip))
  ; History = holding(P0)
    , Tree = ( P1 > P0 ->
         branch(State, run_state(trading(waiting(yes(P1)), Balance + P1, deduct(Trades)), Pt), run_state(State, Pt))
      ; branch(State, tip, run_state(State, Pt))
      )
    ; History = waiting(no)
    , BuyState = trading(holding(P1), Balance - P1, Trades),
      Tree = branch(State, run_state(BuyState, Pt), run_state(State, Pt))
    ;  History = waiting(yes(P0)),
    ( Tree = (if P0 =< P1 then tip else
                branch(State, run_state(trading(holding(P1), Balance - P1, Trades), Pt), run_state(State, Pt))
      )
    ).

:- func run_state(trade_state, list(int)) = decision_tree is det.

run_state(State, Ps) = Tree :- run_state(State, Ps, Tree).


:- pred calculate(decision_tree::in, int::out) is det.
calculate(tip, 0).
calculate(branch(State, Act, Wait), X) :-
  ( State = end(Balance) -> X = Balance
  ; Acted = calculate(Act),
    Waited = calculate(Wait),
    X = max(Acted, Waited)
  ).

:- func calculate(decision_tree) = int is det.
calculate(Tree) = X :- calculate(Tree, X).

main(!IO) :-
    Trades = no_limit,
    Ps = [164,5,70,51,163,12,142,80,76,125,194,148,104,1,113],
    State = start(Trades),
    Result = calculate(run_state(State, Ps)),
    io.format("%d", [i(Result)], !IO).
