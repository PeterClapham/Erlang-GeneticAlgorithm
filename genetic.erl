-module(genetic).
-compile(export_all).

-define(TARGET, "Hello, World!").
-define(MUTATE_RATE, 0.1).


run(PopSize) ->
	PopMan = spawn(?MODULE, popMan, [[]]),
	spawnCand(PopSize, PopMan).


popMan(DB) ->
	% Receive requests from candidates (cand/1).
	receive
		{add, Child} ->
			popMan(DB ++ [Child]);
		{get, Pid} ->
			Pid ! lists:nth(rand:uniform(length(DB)), DB),
			popMan(DB);
		{remove, Pid} ->
			popMan(lists:delete(Pid, DB))
	end.


spawnCand(0, Server) ->
	spawn(?MODULE, cand, [Server]);
spawnCand(Pop, Server) ->
	spawn(?MODULE, cand, [Server]),
    spawnCand(Pop - 1, Server).


% Generates a new candidate solution
cand(Manager) ->
	Candidate = manyCandidates(1000, []),
	Fitness = getFitness(Candidate, ?TARGET, 0),
	Manager ! {add, Candidate},
    candMain(Candidate, Fitness, Manager, 25).


% Generates a new candidate with a known input string
cand(Manager, Candidate) ->
	Fitness = getFitness(Candidate, ?TARGET, 0),
	Manager ! {add, Candidate},
    candMain(Candidate, Fitness, Manager, 25).
    

manyCandidates(0, Candidates) ->
	bestOne(getRandomString(length(?TARGET)), Candidates);
manyCandidates(N, []) ->
	manyCandidates(N - 1, [getRandomString(length(?TARGET))]);
manyCandidates(N, Candidates) ->
    manyCandidates(N - 1, [getRandomString(length(?TARGET)) | Candidates]).
    

bestOne(Best, []) ->
	Best;
bestOne(Best, [C | Cs]) ->
	case getFitness(C, ?TARGET, 0) >= getFitness(Best, ?TARGET, 0) of
		true -> bestOne(C, Cs);
		_ -> bestOne(Best, Cs)
	end.


candMain(?TARGET, _Fitness, _Server, _N) ->
	io:fwrite("An agent has found the correct solution!");
candMain(Candidate, Fitness, Server, 0) ->
	io:fwrite("Agent died, had candidate of ~s and fitness of ~w~n", [Candidate, Fitness]),
	Server!{remove, self()};
candMain(Candidate, Fitness, Server, N) ->
	%timer:sleep(1),
	case (rand:uniform() + Fitness / (length(?TARGET))) > 0.4 of
		true ->
			case (rand:uniform() - Fitness / (length(?TARGET))) < ?MUTATE_RATE of 
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
			candMain(Candidate, Fitness, Server, N - 1);
		_ ->
			candMain(Candidate, Fitness, Server, 0)
	end.


crossover([], [], _N, _Point, Cand) ->
	Cand;
crossover([C | Cs], [M | Ms], N, Point, Cand) ->
	case N > Point of
		true  -> crossover(Cs, Ms, N + 1, Point, [C | Cand]);
		false -> crossover(Cs, Ms, N + 1, Point, [M | Cand])
	end.


mutate([], Cand) ->
	Cand;
mutate([C | Cs], Cand) ->
	case rand:uniform() < ?MUTATE_RATE of 
		true  -> mutate(Cs, getRandomString(1) ++ Cand);
		false -> mutate(Cs, [C | Cand])
    end.
    

getRandomString(Size) ->
    [rand:uniform(95) + 31 || _ <- lists:seq(1, Size)].


getFitness([C], [C], F) ->
	F + 1;
getFitness([_C], [_T], F) ->
	F;
getFitness([C | Cs], [C | Ts], F) ->
	getFitness(Cs, Ts, F + 1);
getFitness([_C | Cs], [_T | Ts], F) ->
    getFitness(Cs, Ts, F).


% Implement FPS
% List of tupes storing pid and fitness
% Keep list sorted by fitness for insertion
% receive getMate, add, remove
% Give priority to remove
% getMate sends a pid for server to handle
% total = getTotal(List)
% num = random(Total)
% Recursive function findMate(Num, List)

% FindMate(Num, List) -> FindMate(Num, List, 0).
% FindMate(Num, [X | Xs], Total) when Total + X >= Num -> X 
% FindMate(Num, [X | Xs], Total) -> FindMate(Num, Xs, X + Total).

% Then send that to the thingy.
% X is a tuple of Pid, Fitness.

% Also implement termination if, on adding, it has fitness equal to work length
