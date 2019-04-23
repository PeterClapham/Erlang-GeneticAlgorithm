-module(genetic).
-compile(export_all).

-define(TARGET, "Hello, World!").
-define(MUTATE_RATE, 0.1).

% To run the program, spawns the population manager and starts spawning candidate solutions
run(PopSize) ->
	PopMan = spawn(?MODULE, popMan, [[]]),
	spawnCand(PopSize, PopMan).

% Population manager. Acts as a simulated 'nature', where 'DB' is the pool of candidate solutions
% Receives requests from these candidates and handles the requests
% The terminate case is when a child has a fitness equal to the word
popMan({terminate, Id} ) ->
	doTerminate();
popMan(DB) ->
	receive
		% Add a child to the sorted DB. The terminate case is within this method
		{add, Child} ->
			popMan(addChild(DB, Child));
		% Gets a mate from the DB, TODO: Implement fitness proportional selection
		{getMate, Pid} ->
			Pid ! lists:nth(rand:uniform(length(DB)), DB),
			popMan(DB);
		% Simulation of dying, removes a candidate solution from the DB
		{remove, Pid} ->
			popMan(lists:delete(Pid, DB))
	end.

% Adds a child to the sorted DB, also sends termination message
addChild(DB, {Pid, length(?TARGET)}) -> 
	{terminate, Pid};
addChild([{XPid, XFitness} | Xs], {Pid, Fitness}) when XFitness < Fitness ->
	{Pid, Fitness} ++ [{XPid, XFitness} | Xs];
addChild([], {Pid, Fitness}) -> 
	[{Pid, Fitness}];
addChild([X | Xs], Child) -> 
	[X] ++ addChild(Xs, Child).

% Creates the initial set of candidate solutions
spawnCand(0, Server) ->
	spawn(?MODULE, cand, [Server]);
spawnCand(Pop, Server) ->
	spawn(?MODULE, cand, [Server]),
  spawnCand(Pop - 1, Server).


% Initializes the new candidate solution from scratch
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
    
% Produces a list of potential candidates and returns the best one
manyCandidates(0, Candidates) ->
	bestOne(getRandomString(length(?TARGET)), Candidates);
manyCandidates(N, []) ->
	manyCandidates(N - 1, [getRandomString(length(?TARGET))]);
manyCandidates(N, Candidates) ->
  manyCandidates(N - 1, [getRandomString(length(?TARGET)) | Candidates]).
    
% Generates the best of a list of solutions
bestOne(Best, []) ->
	Best;
bestOne(Best, [C | Cs]) ->
	case getFitness(C, ?TARGET, 0) >= getFitness(Best, ?TARGET, 0) of
		true -> bestOne(C, Cs);
		false -> bestOne(Best, Cs)
	end.

% The process for a candidate solution
% A candidate has a lifecycle of N passes, when it reaches 0 it tells the Candidate Manager that it is dead
% Each pass:
%   - There is a chance that a candidate can die, higher fitness reduces this chance
%   - A candidate has a probability of mating
%   - If it mates, it selects a random other candidate (will be fitness proportional selection)
%   - After selecting another candidate, it performs crossover to get a new DNA and mutates it
%   - After this, it recurses back with N - 1
% TODO: Possibly remove the ?TARGET catch, possibly work on the 'false -> failed' line
candMain(?TARGET, _Fitness, _Server, _N) ->
	io:fwrite("An agent has found the correct solution!");
candMain(Candidate, Fitness, Server, 0) ->
	io:fwrite("Agent died, had candidate of ~s and fitness of ~w~n", [Candidate, Fitness]),
	Server!{remove, self()};
candMain(Candidate, Fitness, Server, N) ->
	%timer:sleep(1),
	% This case command checks if a candidate dies
	case (rand:uniform() + Fitness / (length(?TARGET))) > 0.4 of
		true ->
		  % This case command checks if a candidate mates
			case (rand:uniform() - Fitness / (length(?TARGET))) < ?MUTATE_RATE of 
				true -> 
					% select a mate
					Server!{getMate, self()},
					receive 
						Mate ->
							% perform one point crossover at a random point
							NewCand = crossover(Candidate, Mate, 0, rand:uniform(length(Candidate)), []),
							% mutate the result
							MutatedCand = mutate(NewCand, []),
							% create the process
							spawn(?MODULE, cand, [Server, MutatedCand])
					end;
				false -> failed
			end,
			candMain(Candidate, Fitness, Server, N - 1);
		false ->
			candMain(Candidate, Fitness, Server, 0)
	end.

% Performs crossover between the two solutions
crossover([], [], _N, _Point, Cand) ->
	Cand;
crossover([C | Cs], [M | Ms], N, Point, Cand) ->
	case N > Point of
		true  -> crossover(Cs, Ms, N + 1, Point, [C | Cand]);
		false -> crossover(Cs, Ms, N + 1, Point, [M | Cand])
	end.

% Mutates the candidate, a chance to set a new random character for each character
mutate([], Cand) ->
	Cand;
mutate([C | Cs], Cand) ->
	case rand:uniform() < ?MUTATE_RATE of 
		true  -> mutate(Cs, getRandomString(1) ++ Cand);
		false -> mutate(Cs, [C | Cand])
    end.
    
% Generates a random string of a certain size
getRandomString(Size) ->
  [rand:uniform(95) + 31 || _ <- lists:seq(1, Size)].

% Tail recursively calculates the fitness of a candidate solution compared to T, a target
getFitness([C], [C], F) ->
	F + 1;
getFitness([_C], [_T], F) ->
	F;
getFitness([C | Cs], [C | Ts], F) ->
	getFitness(Cs, Ts, F + 1);
getFitness([_C | Cs], [_T | Ts], F) ->
  getFitness(Cs, Ts, F).

% TODO: 
%   - Implement FPS
%   - List of tuples storing pid and fitness
%   - Keep list sorted by fitness for insertion   DONE
%   - receive getMate, add, remove                DONE
%   - Give priority to remove
%   - getMate sends a pid for server to handle
%   - total = getTotal(List)
%   - num = random(Total)
%   - Recursive function findMate(Num, List)
% Potential code: 
% FindMate(Num, List) -> FindMate(Num, List, 0).
% FindMate(Num, [X | Xs], Total) when Total + X >= Num -> X 
% FindMate(Num, [X | Xs], Total) -> FindMate(Num, Xs, X + Total).

% Then send that to the thingy.
% X is a tuple of Pid, Fitness.

% Also implement termination if, on adding, it has fitness equal to work length

% {terminate, Id} ->
%   Id ! TerminateCommand,
%   sendTerminations(List),
%   ok.

% Hence, throw in a two second offset in the timer:tc

% Perhaps alter the output system to be, every so often, checking the average fitness of the solutions
% Implement by having a running process every time a death is recorded storing two params, average fitness
% and count. When count increases over a threshold, output the average and reset 
