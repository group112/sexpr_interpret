-module(task1).
-export([interpret/1, test/0]).

interpret(Bin)       when is_binary(Bin)  -> interpret(ast:ast(Bin));
interpret(Num)       when is_integer(Num) -> Num;
interpret({_,  []} )                      -> error;
interpret({Op, Ods})                      -> Es = [interpret(Od) || Od <- Ods],
                                             case lists:member(error, Es) of
                                                 true -> error;
                                                 _    -> [E | Es2] = Es,
                                                         F         = case Op of
                                                                         '(+' -> fun(X, Y) -> X + Y end;
                                                                         '(-' -> fun(X, Y) -> Y - X end;
                                                                         '(*' -> fun(X, Y) -> X * Y end
                                                                     end,
                                                         lists:foldl(F, E, Es2)
                                             end;
interpret(_)                              -> error.

test() -> [interpret(S) || S <- ast:samples1()].

