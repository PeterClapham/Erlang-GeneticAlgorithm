-module(genetic).
-compile(export_all).

-define(Target, "Hello, World!").
-define(AllowedChars, "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890!'?.,").
-define(MutationRate, 0.1).

run(Pop) ->
	PopMan = spawn(?MODULE, popMan, [[]]),
	spawnCand(Pop, PopMan).

spawnCand(0, Server) ->
	spawn(?MODULE, cand, [Server]);
spawnCand(Pop, Server) ->
	spawn(?MODULE, cand, [Server]),
	spawnCand(Pop-1, Server).

popMan(DB) ->
	% Receive requests from cand: add, remove, get
	receive
		{add, Child} -> popMan(DB++[Child]);
		{get, Pid} -> Pid!lists:nth(rand:uniform(length(DB)), DB), popMan(DB);
		{remove, Pid} -> popMan(lists:delete(Pid, DB))
	end.

% Generates a new candidate solution
cand(Server) ->
	Candidate = manyCandidates(1000, []),
	Fitness = getFitness(Candidate, ?Target, 0),
	Server!{add, Candidate},
	candMain(Candidate, Fitness, Server, 25).
% Generates a new candidate with a known input string
cand(Server, Candidate) ->
	Fitness = getFitness(Candidate, ?Target, 0),
	Server!{add, Candidate},
	candMain(Candidate, Fitness, Server, 25).

manyCandidates(0, Candidates) ->
	bestOne(get_random_string(length(?Target)), Candidates);
manyCandidates(N, []) ->
	manyCandidates(N-1, [get_random_string(length(?Target))]);
manyCandidates(N, Candidates) ->
	manyCandidates(N-1, [get_random_string(length(?Target))|Candidates]).

bestOne(Best, []) ->
	Best;
bestOne(Best, [C|Cs]) ->
	case getFitness(C, ?Target, 0) >= getFitness(Best, ?Target, 0) of
		true -> bestOne(C, Cs);
		_ -> bestOne(Best, Cs)
	end.

candMain(?Target, _Fitness, _Server, _N) ->
	io:fwrite("An agent has found the correct solution!");
candMain(Candidate, Fitness, Server, 0) ->
	io:fwrite("Agent died, had candidate of ~s and fitness of ~w~n", [Candidate, Fitness]),
	Server!{remove, self()};
candMain(Candidate, Fitness, Server, N) ->
	%timer:sleep(1),
	case (rand:uniform() + Fitness/(length(?Target))) > 0.4 of
		true ->
			case (rand:uniform() - Fitness/(length(?Target))) < ?MutationRate of 
				true -> 
					% select a mate
					Server!{get, self()},
					receive 
						Mate ->
							% perform crossover
							NewCand = crossover(Candidate, Mate, 0, rand:uniform(length(Candidate)), []),
							% mutate the result
							MutatedCand = mutate(NewCand, []),
							% create the process
							spawn(?MODULE, cand, [Server, MutatedCand])
					end;
				_ -> failed
			end,
			candMain(Candidate, Fitness, Server, N-1);
		_ ->
			candMain(Candidate, Fitness, Server, 0)
	end.

crossover([], [], _N, _Point, Cand) ->
	Cand;
crossover([C|Cs], [M|Ms], N, Point, Cand) ->
	case N > Point of
		true -> crossover(Cs, Ms, N+1, Point, [C|Cand]);
		_ -> crossover(Cs, Ms, N+1, Point, [M|Cand])
	end.

mutate([], Cand) ->
	Cand;
mutate([C|Cs], Cand) ->
	case rand:uniform() < ?MutationRate of 
		true -> mutate(Cs, get_random_string(1) ++ Cand);
		_ -> mutate(Cs, [C|Cand])
	end.

get_random_string(Length) ->
    [lists:nth(rand:uniform(length(?AllowedChars)), ?AllowedChars) || _ <- lists:seq(1, Length)].

getFitness([C], [C], F) ->
	F+1;
getFitness([_C], [_T], F) ->
	F;
getFitness([C|Cs], [C|Ts], F) ->
	getFitness(Cs, Ts, F+1);
getFitness([_C|Cs], [_T|Ts], F) ->
	getFitness(Cs, Ts, F).
