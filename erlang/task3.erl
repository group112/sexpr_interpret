-module(task3).
-export([interpret_cpu/2, cpu/1, test/0]).
-include("schedule.hrl").


cpu(C) -> receive
              quit              -> ok;
              {Pid, J, Op, Ods} -> [V | Vs] = Ods,
                                   F        = case Op of
                                                  '(+' -> fun(X, Y) -> X + Y end;
                                                  '(-' -> fun(X, Y) -> Y - X end;
                                                  '(*' -> fun(X, Y) -> X * Y end
                                              end,
                                   timer:sleep(schedule:delay(Op) * 1000),
                                   Pid ! {C, J, lists:foldl(F, V, Vs)},
                                   cpu(C);   
              _                 -> cpu(C)
          end.


createCPUs(Sl)  -> [{C, #cpu{num=C, use=false, pid=spawn(?MODULE, cpu, [C]), jobs=proplists:get_value(C, Sl)}} || C <- lists:seq(1, length(Sl))].

destroyCPUs(Cs) -> [?getPid(Cs, C) ! quit || C <- lists:seq(1, length(Cs))].


sendJobs(Js, Cs)     -> lists:foldl(fun(E, A) -> sendJob(E, A) end, {Js, Cs}, [C || {C, _} <- Cs, ?getUse(Cs, C) =:= false, length(?getJobs(Cs, C)) > 0]).

sendJob(C, {Js, Cs}) -> J   = hd(?getJobs(Cs, C)),
                        Op  = ?getOp(Js, J),
                        Ods = [?getRes(Js, N) || N <- ?getChn(Js, J)],
                        P   = not lists:member(undefined, Ods),                       
                        if
                            P    -> ?getPid(Cs, C) ! {self(), J, Op, Ods},
                                    {Js, ?setJobs(?setUse(Cs, C, true), C, tl(?getJobs(Cs, C)))};
                            true -> {Js, Cs}
                        end.

recvJob(Js, Cs, St)  -> receive
                            {C, J, R} -> {?setTime(?setRes(Js, J, R), J, timer:now_diff(os:timestamp(), St) / 1000000), ?setUse(Cs, C, false)};
                            _         -> {Js, Cs}
                        end.


interpret_cpu(Bin,   NCs)  when is_binary(Bin) -> interpret_cpu(ast:ast(Bin), NCs);
interpret_cpu(error, _  )                      -> error;
interpret_cpu(Ast,   NCs)                      -> Js       = schedule:jobs(Ast),
                                                  {Tt, Sl} = schedule:alg2(Js, NCs),
                                                  schedule:printSchedule(Sl),
                                                  Cs       = createCPUs(Sl),
                                                  St       = os:timestamp(),
                                                  Js2      = interpret_cpu(Js, Cs, St),
                                                  destroyCPUs(Cs),
                                                  schedule:printJobs(Js2),
                                                  {?getRes(Js2, 1), ?getTime(Js2, 1), Tt}.

interpret_cpu(Js, Cs, St)                      -> case ?getRes(Js, 1) of
                                                      undefined -> {Js2, Cs2} = sendJobs(Js, Cs),
                                                                   {Js3, Cs3} = recvJob(Js2, Cs2, St),
                                                                   interpret_cpu(Js3, Cs3, St);
                                                      _         -> Js
                                                  end.


test() -> [apply(?MODULE, interpret_cpu, S) || S <- ast:samples2()].
