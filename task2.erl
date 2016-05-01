-module(task2).
-export([interpret_network/1, test/0]).

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

