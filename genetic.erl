-module(genetic).
-compile(export_all).

% These are variables you can tweak manually in the source code
-define(TARGET, "Hello, World!").
-define(MUTATE_RATE, 0.1).
-define(BREED_RATE, 0.4).
-define(CROSSOVER_RATE, 0.2).

% TODO: Write and stick to strict type definitions where possible
% -type candidate() :: {pid(), string()}.

% To run the program, spawns the population manager and starts spawning candidate solutions
run(PopSize) ->
	PopMan = spawn(?MODULE, popMan, [[]]),
	spawnCand(PopSize, PopMan).

% Population manager. Acts as a simulated 'nature', where 'DB' is the pool of candidate solutions
% Receives requests from these candidates and handles the requests
% Terminates if, on adding, it discovers that a candidate has found the solutions
% Removing has priority over the two other commands
popMan(DB) ->
	receive
		% Simulation of dying, removes a candidate solution from the DB
		{remove, Pid} ->
			NewDB = lists:keydelete(Pid, 1, DB),
				case length(NewDB) of
					0 -> spawnCand(50, self());
					_ -> popMan(NewDB)
				end
		after 0 ->
			receive
				% Add a child to the sorted DB. The terminate case is within this method
				{add, {Pid, String}} ->
					Fitness = getFitness(String, ?TARGET, 0),
					case Fitness == length(?TARGET) of
						true ->
							exit(Pid, finished),
							sendTerminations(DB);
						false ->
							popMan(addChild(DB, {Pid, String}))
					end;
				% Gets a mate from the DB
				{getMate, Pid} ->
					getMate(DB, Pid),
					popMan(DB)
			end
	end.

% Sends the mate that is decided by findMate to the agent
getMate(List, Pid) ->
	Total = getTotal(List),
	case Total of
		0 -> Pid ! getRandomString(length(?TARGET));
		_ -> 
			Threshold = rand:uniform(Total),
			String = findMate(Threshold, List),
			Pid ! String
	end.

% Implementation of fitness proportional selection, returns the String choice
findMate(Num, List) -> findMate(Num, List, 0).
findMate(Num, [{_Pid, String} | Tail], Total) ->
	Fitness = getFitness(String, ?TARGET, 0),
	case Fitness + Total >= Num of
		true -> String;
		false -> findMate(Num, Tail, Fitness + Total)
	end.

% Gets ths total fitness of the list
getTotal(List) -> getTotal(List, 0).
getTotal([{_Pid, String} | Tail], Total) -> getTotal(Tail, Total + getFitness(String, ?TARGET, 0));
getTotal([], Total) -> Total.

sendTerminations([]) -> finished;
sendTerminations([{Pid, _String} | Others]) ->
	exit(Pid, finished),
	sendTerminations(Others).

% Adds a child to the sorted DB, also sends termination message
addChild([], {Pid, String}) -> 
	[{Pid, String}];
addChild([{XPid, XString} | Xs], {Pid, String}) ->
	case getFitness(String, ?TARGET, 0) < getFitness(XString, ?TARGET, 0) of 
		true -> [{XPid, XString}] ++ addChild(Xs, {Pid, String});
		false -> [{Pid, String}] ++ [{XPid, XString} | Xs]
	end.

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
	Manager ! {add, {self(), Candidate}},
  candMain(Candidate, Fitness, Manager, 10).

% Generates a new candidate with a known input string
cand(Manager, Candidate) ->
	Fitness = getFitness(Candidate, ?TARGET, 0),
	Manager ! {add, Candidate},
  candMain(Candidate, Fitness, Manager, 10).
    
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
candMain(Candidate, Fitness, Server, 0) ->
	io:fwrite("Agent died, had candidate of ~s and fitness of ~w~n", [Candidate, Fitness]),
	Server!{remove, self()};
candMain(Candidate, Fitness, Server, N) ->
	timer:sleep(1),
	% This case command checks if a candidate dies
	case (rand:uniform() + Fitness / (length(?TARGET))) > 0.4 of
		true ->
		  % This case command checks if a candidate mates
			case (rand:uniform() - Fitness / (length(?TARGET))) < ?BREED_RATE of 
				true -> 
					% select a mate
					Server!{getMate, self()},
					receive 
						Mate ->
							% perform one point crossover at a random point
							case rand:uniform() < ?CROSSOVER_RATE of 
								true -> NewCand = crossover(Candidate, Mate, 0, rand:uniform(length(Candidate)), []);
								false -> NewCand = Candidate
							end,
							% mutate the result
							MutatedCand = mutate(NewCand, []),
							% create the process
							spawn(?MODULE, cand, [Server, MutatedCand])
					end;
				false ->
					failed
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

% Throw in a two second offset in the timer:tc

% Perhaps alter the output system to be, every so often, checking the average fitness of the solutions
% Implement by having a running process every time a death is recorded storing two params, average fitness
% and count. When count increases over a threshold, output the average and reset 
