-module(ast).
-export([ast/1, samples1/0, samples2/0]).

tokens(Bin) when is_binary(Bin) -> lists:reverse(tokens_(binary:bin_to_list(Bin), [])).

tokens_([],             Tns) -> Tns;
tokens_([($\ )  | Lst], Tns) -> tokens_(Lst, Tns);
tokens_([$)     | Lst], Tns) -> tokens_(Lst, [')'] ++ Tns);
tokens_([$(, Ch | Lst], Tns) -> case Ch of
                                    $+ -> tokens_(Lst, ['(+'] ++ Tns);
                                    $- -> tokens_(Lst, ['(-'] ++ Tns);
                                    $* -> tokens_(Lst, ['(*'] ++ Tns);
                                    _  -> error
                                end;
tokens_(Lst, Tns)            -> case string:to_integer(Lst) of
                                    {error,    _} -> error;
                                    {Num,   Lst2} -> tokens_(Lst2, [Num] ++ Tns)
                                end.

ast(Bin) when is_binary(Bin) -> ast(tokens(Bin));
ast(Tns)                     -> case ast_(Tns) of
                                    {Ast, []} -> Ast;
                                    _         -> error
                                end.

ast_([Num  | Tns]) when is_integer(Num) -> {Num, Tns};
ast_(['(+' | Tns])                      -> ast_('(+', [], Tns);
ast_(['(-' | Tns])                      -> ast_('(-', [], Tns);
ast_(['(*' | Tns])                      -> ast_('(*', [], Tns);
ast_(_)                                 -> error.

ast_(_,  _,   []         ) -> error;
ast_(Op, Ods, [')' | Tns]) -> {{Op, lists:reverse(Ods)}, Tns};
ast_(Op, Ods, Tns        ) -> case ast_(Tns) of
                                  {Ast, Tns2} -> ast_(Op, [Ast] ++ Ods, Tns2);
                                  _           -> error
                              end.

sample_1() -> <<"(+ (* 4 4) (* 2 (- 7 5)) 1)">>.
sample_2() -> <<"10">>.
sample_3() -> <<"(* 10 (- 0 1))">>.
sample_4() -> <<"(- (+ 10 10) -5 0)">>.
sample_5() -> <<"(+ (- (* (+ (- (* 1))))))">>.
sample_6() -> <<"(* 2 (+ (- 10 9) (- 3 (* 2 1))) (+ (- 10 9) (- 3 (* 2 1))))">>.
sample_7() -> <<"(+ (* 2 1) (+ 8 8) (- (+ 4 3 2 1) (* 3 3) (* 2 2)) (* 5 7))">>.
sample_8() -> <<"(- (+ (+ 3 3) (- 3 3) (+ 3 3) (- 3 3)) (* 2 2))">>.
sample_9() -> <<"(+ (- 6 1) (+ 0 1 1) (- 7 2) (* 3 4 5) (- 3 1) (+ 2) (- 0 10))">>.

samples1() -> [sample_1(),
              sample_2(), 
              sample_3(), 
              sample_4(), 
              sample_5(), 
              sample_6(), 
              sample_7(), 
              sample_8(), 
              sample_9()].

samples2() -> [[sample_1(), 2],
               [sample_2(), 2],
               [sample_3(), 2],
               [sample_4(), 2],
               [sample_5(), 2],
               [sample_6(), 2],
               [sample_6(), 3],
               [sample_7(), 2],
               [sample_7(), 3],
               [sample_7(), 4],
               [sample_8(), 2],
               [sample_8(), 3],
               [sample_9(), 2]].

