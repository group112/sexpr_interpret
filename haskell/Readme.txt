> :l Ast Job Packing Schedule Task1 Task2 Task2_ Task3
Ok, modules loaded: Ast, Job, Packing, Schedule, Task1, Task2, Task2_, Task3.

> Task1.test
[Just 21,Just 10,Just (-10),Just 25,Just 1,Just 8,Just 50,Just 8,Just 66]

> Task2.test
[Just (21,15),Just (10,0),Just (-10,13),Just (25,5),Just (1,30),Just (8,25),Just (50,15),Just (8,13),Just (66,12)]

> Task2_.test
[Just (21,15.01398),Just (10,1.1e-5),Just (-10,13.010983),Just (25,5.006425),Just (1,30.026833),Just (8,25.018254),Just (50,15.009516),Just (8,13.01149),Just (66,12.006052)]

> Task3.test
[Just (21,15,15.011302),Just (10,0,5.6e-5),Just (-10,13,13.010824),Just (25,5,5.00581),Just (1,30,30.023437),Just (8,28,28.012891),Just (8,25,25.017404),Just (50,26,26.017225),Just (50,22,22.007344),Just (50,15,15.010624),Just (8,15,15.019921),Just (8,13,13.006159),Just (66,15,15.015459)]
