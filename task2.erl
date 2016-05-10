-module(task2).
-export([interpret_network/1, test/0]).
-export([interpret_network_/1, test_/0, serv/0, clnt/4, calc/3]).

interpret_network(Bin)       when is_binary(Bin)  -> interpret_network(ast:ast(Bin));
interpret_network(Num)       when is_integer(Num) -> {Num, 0};
interpret_network({_,  []} )                      -> error;
interpret_network({Op, Ods})                      -> Es = [interpret_network(Od) || Od <- Ods],
                                                     case lists:member(error, Es) of
                                                         true -> error;
                                                         _    -> {Vs, Ts } = lists:unzip(Es),
                                                                 [V | Vs2] = Vs,
                                                                 F         = case Op of
                                                                                 '(+' -> fun(X, Y) -> X + Y end;
                                                                                 '(-' -> fun(X, Y) -> Y - X end;
                                                                                 '(*' -> fun(X, Y) -> X * Y end
                                                                             end,
                                                                 {lists:foldl(F, V, Vs2), schedule:delay(Op) + lists:max(Ts)}
                                                     end; 
interpret_network(_)                              -> error.

test() -> [interpret_network(S) || S <- ast:samples1()].


calc(Pid, Op, Ods) -> [V | Vs] = Ods,
                      F        = case Op of
                                     '(+' -> fun(X, Y) -> X + Y end;
                                     '(-' -> fun(X, Y) -> Y - X end;
                                     '(*' -> fun(X, Y) -> X * Y end
                                 end,
                      timer:sleep(schedule:delay(Op) * 1000),
                      Pid ! lists:foldl(F, V, Vs).

serv() -> receive
              {Pid, Op, Ods} -> spawn(?MODULE, calc, [Pid, Op, Ods]),
                                serv();
              _              -> ok
          end.

clnt(_, Num      ) when is_integer(Num) -> Num;
clnt(_, {_,  []} )                      -> error;
clnt(S, {Op, Ods})                      -> [spawn(?MODULE, clnt, [S, Od, N, self()]) || {N, Od} <- lists:zip(lists:seq(1, length(Ods)), Ods)],
                                           Xs = collect(length(Ods), []),
                                           Ys = [proplists:get_value(N, Xs) || N <- lists:seq(1, length(Ods))],
                                           E  = lists:member(error, Ys),
                                           if
                                                E    -> error;
                                                true -> S ! {self(), Op, Ys},
                                                        receive
                                                            R -> R
                                                        end
                                           end.

clnt(S, Ast, N, Pid)                    -> Pid ! {N, clnt(S, Ast)}.

collect(NXs, Xs) -> if
                        NXs == length(Xs) -> Xs;
                        true            -> receive 
                                               X -> collect(NXs, [X] ++ Xs)
                                           end 
                    end.

interpret_network_(Bin) when is_binary(Bin) -> interpret_network_(ast:ast(Bin));
interpret_network_(Ast)                     -> S = spawn(?MODULE, serv, []),
                                               B = os:timestamp(),
                                               R = {clnt(S, Ast), timer:now_diff(os:timestamp(), B) / 1000000},
                                               S ! quit,
                                               R.

test_() -> [interpret_network_(S) || S <- ast:samples1()].
