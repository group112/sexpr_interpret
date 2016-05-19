-module(packing).
-export([optimum/2]).
-include("schedule.hrl").


weight(Es) when is_list(Es) -> lists:foldl(fun(X, A) -> X + A end, 0, [0] ++ [W || {_, W} <- Es]);
weight(B)                   -> B#bin.load + weight(B#bin.elms).


split(B, N) -> {L, R} = lists:split(N, B#bin.elms),
               {L, R, weight(L), weight(R) + B#bin.load}.


swap(B1, B2)                                       -> swap(B1, B2, 1, 1).
swap(B1, _,  N1, _ ) when N1 > length(B1#bin.elms) -> no;
swap(B1, B2, N1, N2) when N2 > length(B2#bin.elms) -> swap(B1, B2, N1 + 1, 1);
swap(B1, B2, N1, N2)                               -> {L1, R1, WL1, WR1} = split(B1, N1),
                                                      {L2, R2, WL2, WR2} = split(B2, N2),
                                                      A                  = lists:max([WL1 + WR1, WL2 + WR2]),
                                                      B                  = lists:max([WL2 + WR1, WL1 + WR2]),
                                                      if
                                                          A > B -> {B1#bin{elms = L2 ++ R1}, B2#bin{elms = L1 ++ R2}};
                                                          true  -> swap(B1, B2, N1, N2 + 1)
                                                      end.


minimize(Bs)                              -> minimize(Bs, 1, 2).
minimize(Bs, N1, _ ) when N1 > length(Bs) -> Bs;
minimize(Bs, N1, N2) when N2 > length(Bs) -> minimize(Bs, N1 + 1, N1 + 2);
minimize(Bs, N1, N2)                      -> case swap(?getBin(Bs, N1), ?getBin(Bs, N2)) of
                                                 {B1, B2} -> minimize(?setBin(?setBin(Bs, N1, B1), N2, B2));
                                                 no       -> minimize(Bs, N1, N2 + 1)
                                             end.


setElm(Bs, N, E)      -> B = ?getBin(Bs, N),
                         ?setBin(Bs, N, B#bin{elms = [E] ++ B#bin.elms}).

set(Ls, Es)           -> minimize(set(Ls, Es, {})).
set(Ls, Es,       {}) -> Bs = [{N, #bin{load=L, elms=[]}} || {N, L} <- lists:zip(lists:seq(1, length(Ls)), Ls)],
                         set(Ls, Es, Bs);
set(_,  [],       Bs) -> Bs;
set(Ls, [E | Es], Bs) -> [P | Ps] = [{N, weight(B)} || {N, B} <- Bs],
                         F        = fun({N, W}, {Na, Wa}) -> if
                                                                 W < Wa -> {N,  W };
                                                                 true   -> {Na, Wa}
                                                             end
                                    end, 
                         {N, _}   = lists:foldl(F, P, Ps),
                         set(Ls, Es, setElm(Bs, N, E)).


optimum(Ls, Es) -> [{C, J} || {C, {J, _}} <- [{N, hd(lists:reverse([undefined] ++ B#bin.elms))} || {N, B} <- set(Ls, Es)]].

