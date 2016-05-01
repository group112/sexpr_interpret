-module(schedule).
-export([delay/1, jobs/1, alg/2, printJobs/1, printSchedule/1]).
-include("schedule.hrl").


delay('(+') -> 2;
delay('(-') -> 3;
delay('(*') -> 10.


jobs(Ast)                    -> {Js, _, _} = jobs(Ast, [], 1, undefined),
                                Js.
jobs({Op, Cn}, Js, N, P)     -> {Ns, Js2, Nn} = jobs(Cn, [], Js, N, N + 1),
                                J = #job{num=N, op=Op, pnt=P, chn=Ns},
                                {[{N, J}] ++ Js2, N, Nn};
jobs(I,        Js, N, P)     -> J = #job{num=N, op=int, time=0, res=I, pnt=P},
                                {[{N, J}] ++ Js, N, N + 1}. 
jobs([],       Ns, Js, _, N) -> {lists:reverse(Ns), Js, N}; 
jobs([C | Cn], Ns, Js, P, N) -> {Js2, CN, Nn} = jobs(C, Js, N, P),
                                jobs(Cn, [CN] ++ Ns, Js2, P, Nn).


printJobs(Js)           -> io:format("---------------------------------------------------------------------------------~n", []),
                           io:format("Job |  Op   |  Weight   |   Time    |  Result   |  Parent   |     Children       ~n", []),
                           io:format("---------------------------------------------------------------------------------~n", []),
                           printJob(Js). 

printJob([])            -> ok;
printJob([{_, J} | Js]) -> io:format("~3w | ~5w | ~9w | ~9w | ~9w | ~9w | ~w~n", tl(tuple_to_list(J))),
                           printJob(Js). 


weight(Js)    -> weight(Js, 1).
weight(Js, N) -> case ?getTime(Js, N) of
                     undefined -> Chn = ?getChn(Js, N),
                                  Js2 = lists:foldl(fun(E, A) -> weight(A, E) end, Js, Chn),
                                  W   = lists:max([?getWht(Js2, E) || E <- Chn]),
                                  case {?getWht(Js2, N), W} of
                                      {undefined, _} -> ?setWht(Js2, N, delay(?getOp(Js2, N)) + W);
                                      {_,         0} -> ?setWht(Js2, N, delay(?getOp(Js2, N)));
                                      _              -> Js2 
                                  end;
                     _         -> ?setWht(Js, N, 0)
                 end.


initSchedule(NCs)   -> [{C, [{idle, 0, 0}]} || C <- lists:seq(1, NCs)].

currentTime(Sl)     -> lists:min([E || {_, S} <- Sl, {_, _, E} <- [hd(S)]]).

nextTime(Sl)        -> lists:min([E || {_, S} <- Sl, {_, _, E} <- [hd(S)], E /= currentTime(Sl)]).

totalTime(Sl)       -> lists:max([E || {_, S} <- Sl, {_, _, E} <- [hd(S)]]).

squeezeSchedule(Sl) -> F = fun(S) -> lists:reverse([J || {J, _, _} <- S, J /= idle])
                           end,
                       [{N, F(S)} || {N, S} <- Sl].


printSchedule(Sl            ) -> io:format("---------------------------------------------------------------------------------~n", []),
                                 io:format("CPU |        Schedule                                                            ~n", []),
                                 io:format("---------------------------------------------------------------------------------~n", []),
                                 printSchedule_(Sl).
printSchedule_([]           ) -> ok;
printSchedule_([{N, C} | Sl]) -> io:format("~3w | ", [N]),
                                 printCPU(C),
                                 io:format("~n", []),
                                 printSchedule_(Sl).

printCPU([]      ) -> ok;
printCPU([C | Cs]) -> io:format("~14w", [C]),
                      printCPU(Cs).


availableCPUs(Sl)  -> [C || {C, S} <- Sl, {_, _, E} <- [hd(S)], E =:= currentTime(Sl)].     

workCPU(Sl, JN, CN, Bt, Et)    -> lists:keyreplace(CN, 1, Sl, {CN, [{JN, Bt, Et}] ++ proplists:get_value(CN, Sl)}).

idleCPUs(Sl)                   -> idleCPUs(Sl, currentTime(Sl), nextTime(Sl), availableCPUs(Sl)).
idleCPUs(Sl, _, _, [])         -> Sl;
idleCPUs(Sl, Ct, Nt, [C | Cs]) -> S   = [{idle, Ct, Nt}] ++ proplists:get_value(C, Sl),
                                  idleCPUs(lists:keyreplace(C, 1, Sl, {C, S}), Ct, Nt, Cs).

jobPath(Js, N)     -> jobPath(Js, N, []).
jobPath(Js, N, Ns) -> case ?getPnt(Js, N) of
                          undefined -> [N] ++ Ns;
                          _         -> jobPath(Js, ?getPnt(Js, N), [N] ++ Ns)
                      end. 

cmpJs(Js, J1, J2)                  -> cmpJs_(Js, jobPath(Js, J1#job.num), jobPath(Js, J2#job.num)).
cmpJs_(Js, [N  | Ns1], [N  | Ns2]) -> cmpJs_(Js, Ns1, Ns2);
cmpJs_(Js, [N1 | _  ], [N2 | _  ]) -> ?getWht(Js, N1) >= ?getWht(Js, N2);  
cmpJs_(_,  _,          _         ) -> true.

selectJs(Js, Sl)   -> Pr           = fun(N)      -> ?getTime(Js, N) =< currentTime(Sl)
                                     end,
                      Pa           = fun({_, J}) -> case J#job.time of
                                                        undefined -> not lists:member(false, [Pr(N) || N <- J#job.chn]);
                                                        _         -> false
                                                    end
                                     end,
                      Cmp          = fun({_, J1}, {_, J2}) -> cmpJs(Js, J1, J2)
                                     end,
                      JNs          = lists:sublist([N || {N, _} <- lists:sort(Cmp, [J || J <- Js, Pa(J)])], length(availableCPUs(Sl))),
                      {WCNs, ICNs} = lists:split(length(JNs), availableCPUs(Sl)),
                      {JNs, WCNs, ICNs}.


performJs(Js, Sl, _,  []      ) -> {Js, Sl};
performJs(Js, Sl, Ct, [P | Ps]) -> {Js2, Sl2} = performJs(Js, Sl, Ct, P),
                                   performJs(Js2, Sl2, Ct, Ps);
performJs(Js, Sl, Ct, {JN, CN}) -> Et = delay(?getOp(Js, JN)) + Ct,
                                   {?setTime(Js, JN, Et), workCPU(Sl, JN, CN, Ct, Et)}.


alg(Js, NCs) when is_integer(NCs) -> alg(weight(Js), initSchedule(NCs));
alg(Js, Sl )                      -> {JNs, WCNs, ICNs} = selectJs(Js, Sl),
                                     case {JNs, ?getTime(Js, 1)} of
                                         {[], undefined} -> alg(Js, idleCPUs(Sl));
                                         {[], _        } -> {totalTime(Sl), squeezeSchedule(Sl)};
                                         _               -> {Js2, Sl2} = performJs(Js, Sl, currentTime(Sl), lists:zip(JNs, WCNs)),
                                                            {Js3, Sl3} = if
                                                                             length(ICNs) > 0 -> {weight(Js2), idleCPUs(Sl2)};
                                                                             true             -> {weight(Js2), Sl2}
                                                                         end,
                                                            alg(Js3, Sl3)
                                     end.

