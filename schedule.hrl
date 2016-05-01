-record(cpu, {num, pid, use, jobs}).

-define(getPid(Cs, C),      (proplists:get_value(C, Cs))#cpu.pid).

-define(getUse(Cs, C),      (proplists:get_value(C, Cs))#cpu.use).

-define(setUse(Cs, C, U),   lists:keyreplace(C, 1, Cs, {C, (proplists:get_value(C, Cs))#cpu{use=U}})).

-define(getJobs(Cs, C),     (proplists:get_value(C, Cs))#cpu.jobs).

-define(setJobs(Cs, C, Js), lists:keyreplace(C, 1, Cs, {C, (proplists:get_value(C, Cs))#cpu{jobs=Js}})).


-record(job, {num, op, wht, time, res, pnt, chn}).

-define(getOp(Js, J),      (proplists:get_value(J, Js))#job.op).

-define(getWht(Js, J),     (proplists:get_value(J, Js))#job.wht).

-define(setWht(Js, J, W),  lists:keyreplace(J, 1, Js, {J, (proplists:get_value(J, Js))#job{wht=W}})).

-define(getTime(Js, J),    (proplists:get_value(J, Js))#job.time).

-define(setTime(Js, J, T), lists:keyreplace(J, 1, Js, {J, (proplists:get_value(J, Js))#job{time=T}})).

-define(getRes(Js, J),     (proplists:get_value(J, Js))#job.res).

-define(setRes(Js, J, R),  lists:keyreplace(J, 1, Js, {J, (proplists:get_value(J, Js))#job{res=R}})).

-define(getPnt(Js, J),     (proplists:get_value(J, Js))#job.pnt).

-define(getChn(Js, J),     (proplists:get_value(J, Js))#job.chn).

