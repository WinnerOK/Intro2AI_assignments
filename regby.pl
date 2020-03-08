% Coondinate system:
% Y
% ^
% | 
% 4|***OT
% 3|*O***
% 2|*O*O*
% 1|HO*O*
% 0|*O***
%   01234 ->X

% ===========================
% Constants
% fieldSize(10,10). % this should be specified either in input file if differs, or here if constant
sight_distance(1).

% move and throw directions: (name, dx, dy)
moveDir(top, 0, 1).
moveDir(btm, 0, -1).
moveDir(left, -1, 0).
moveDir(right, 1, 0).

throwDir(top, 0, 1).
throwDir(btm, 0, -1).
throwDir(left, -1, 0).
throwDir(right, 1, 0).
throwDir(topR, 1, 1).
throwDir(topL, -1, 1).
throwDir(botR, 1,-1).
throwDir(botL, -1,-1).

% ===========================
% Utils

% These wrappers check in runtime whether corresponding predicate exists. If so, refers to it.
ork(X,Y) :- current_predicate(o/2), o(X,Y).
human(X,Y) :- current_predicate(h/2), h(X,Y).
touchdown(X,Y) :- current_predicate(t/2), t(X,Y).

% Print an entity at position (X,Y)
print(X, Y) :-
    ( 
        human(X,Y) -> write("H") 
      ; ork(X,Y) -> write("O")
      ; touchdown(X,Y) -> write("T")
      ; write("█")
    ).

% Utility predicate to implement show_map.
iterate(Y) :-
    write(Y), write(" "), 
    Y > 0, Ynew is Y - 1, 
    iterate(Ynew).


% for (y = Y; y > 0; y --)
    % for (x = X; x < Xsize; x ++)
iterate(X, Y) :-
    % format("~a ~a\n", [X, Y]),
    print(X,Y), % action at X increment
    % Size is duplicated, because otherwise it behaves strangely, working with printing Y*Y*X times
    ( fieldSize(XSize, _), X < XSize - 1, Xnew is X + 1, iterate(Xnew, Y));

    (fieldSize(XSize, _), X =:= XSize - 1, Y > 0, Ynew is Y - 1, 
    write("\n"), % action at Y increment
    iterate(0, Ynew)).

% Check whether last element of new path was visited before
has_not_been(OldPath, NewPath):-
    last(NewPath, [X,Y,_]),
    \+member([X,Y,_], OldPath).

% Check whether ball can be at (X,Y)
is_valid_position(X,Y):-
    is_inbound(X,Y),
    \+ork(X,Y).

% If CheckValidity is true, Checks whether ball can be in position specified by last element of Path
is_valid_path(CheckValidity, Path):-
    CheckValidity -> (
        last(Path, [X,Y,_]),
        is_valid_position(X,Y)
    )
    ; true.

% Calculate how much steps will it take to walk across the map + 1
get_max_steps_count(Cnt) :-
    fieldSize(X,Y),
    Cnt is X*Y + 1.

% set global var for current max path length
set_current_max(CurMax) :-
    nb_setval(current_max, CurMax).

% get current max path length
get_current_max(CurMax) :-
    nb_getval(current_max, CurMax).

% Check whether distance between (X1, Y1) and (X2, Y2) is 1
% Consider diagonal neighbours
around(X1,Y1, X2, Y2) :-
    ((abs(X1-X2)=:= 1) , ( abs(Y1-Y2)=:= 0)), ! ;
    ((abs(X1-X2) =:= 0) , (abs(Y1-Y2) =:= 1)), !;
    ((abs(X1-X2) =:= 1) , (abs(Y1-Y2) =:= 1)), !.

% Utility predicate for random search in order to prevent the agent
% going out of bounds if it is near the boundary
prevent_out_of_bounds_move_from_the_edge(X,Y, NewPath):-
    last(NewPath, [Xnew, Ynew, _]),
    (
        is_inbound(Xnew, Ynew) -> true
    ;   \+around(X,Y, Xnew, Ynew)    
    ).

% predicate telling whether the agent should continue optimized backtrack
% (The agent should stop if current score > Max score)
should_continue_backtrack(Check, Score) :-
    \+Check -> true
    ; get_current_max(MaxScore),
      Score =< MaxScore.

% A predicate to write statistics
time(P) :-
    statistics(walltime, _),
    (P),
    statistics(walltime, [_ , T]),
    format("~w msec\n", [T]).

print_path([]).

print_path([[X, Y, Type]|Tail]) :-
    Type = pass -> (format("P ~w ~w\n", [X,Y]), print_path(Tail)) ; (format("~w ~w\n", [X,Y]), print_path(Tail)).

print_path([_| Path], S) :-
    format("~w\n", [S]),
    print_path(Path).
% ===========================
% Api

% Print the map 
show_map :- 
    fieldSize(_, YMax),
    Y is YMax - 1,
    iterate(0,Y).

% Check whether distance between (X1, Y1) and (X2, Y2) is 1
% Does not consider diagonal neighbours
are_adjacent(X1,Y1,X2,Y2) :-
    ((abs(X1-X2)=:= 1) , ( abs(Y1-Y2)=:= 0)), ! ;
    ((abs(X1-X2) =:= 0) , (abs(Y1-Y2) =:= 1)), !.

% Check whether (X, Y) is in field
is_inbound(X,Y) :-
    fieldSize(XSize, YSize),
    X >= 0, X < XSize,
    Y >= 0, Y < YSize.

% Try to make a pass X,Y in direction Dir, knowing the Path and Current Score.
%  check each cell on the ray.
attempt_pass(X,Y, Dir, Path, CurrentScore, NewPath, NewScore) :-
    throwDir(Dir, Dx, Dy), % choose the direction vector for given dir
    Xnew is X + Dx, Ynew is Y + Dy, % calculate coordinates of new cell
    % is_inbound(Xnew, Ynew),
    (
        % First 2 conditions were used to fail the predicate, but due to random search needs
        % I decided to move check on the upper level
        \+is_inbound(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath), NewScore is CurrentScore + 1
    ;   ork(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath), NewScore is CurrentScore + 1
    ;   human(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath), NewScore is CurrentScore + 1
    ;   attempt_pass(Xnew, Ynew, Dir, Path, CurrentScore, NewPath, NewScore)
    ).

% Try to make a move from X,Y in direction Dir, knowing the Path and Current Score
attempt_move(X,Y, Dir, Path, CurrentScore, NewPath, NewScore) :-
    is_inbound(X,Y),
    moveDir(Dir, Dx, Dy), % choose a direction for moving
    Xnew is X + Dx, Ynew is Y + Dy,
    
    % This check wasn't move since even in random search player knows it's pos
    is_inbound(Xnew, Ynew),
    % \+ork(Xnew, Ynew), % Check moved on upper layer
    append(Path, [[Xnew, Ynew, move]], NewPath),
    (
        human(Xnew, Ynew) -> NewScore = CurrentScore 
    ;   NewScore is CurrentScore + 1     
    ).

% Try to move or give a pass from X,Y in direction Dir, knowing whether it already passed this round and current score.
% Check Validity parameter is used for random. If True - consider only moves that result in acceptable state, otherwise - all
do_action(X,Y,Dir, Path, PassedThisRound, CurrentScore, CheckValidity,  
        NewPath, NewPassedThisRound, NewScore) :-
    (   % try to make a pass
        \+PassedThisRound,  attempt_pass(X, Y, Dir, Path, CurrentScore, NewPath, NewScore),
        is_valid_path(CheckValidity, NewPath), has_not_been(Path, NewPath), 
        prevent_out_of_bounds_move_from_the_edge(X,Y, NewPath),
        NewPassedThisRound = true
    );  % try to move
    (   attempt_move(X, Y, Dir, Path, CurrentScore, NewPath, NewScore), 
        is_valid_path(CheckValidity, NewPath), has_not_been(Path, NewPath), 
        NewPassedThisRound = PassedThisRound
    ).
% A predicate catching cases when ork or touchdown on (0,0). 
% Case with human(0,0) makes no interesting
first_step_check(Solved):-
    ork(0,0) -> ( Solved = die)
    ; touchdown(0,0) -> (Solved = win)
    ; (Solved = continue, true).

% ===========================
% Backtracking
% A base case: If there is a touchdown within 1 cell from agent
go(Optimized, Xstart, Ystart, PathStart, CurrentScore, _, PathFinish, NewScore) :-
    % try all directions
    attempt_move(Xstart, Ystart, _, PathStart, CurrentScore, PathNew, NextScore),
    should_continue_backtrack(Optimized, NextScore), % for optimized BT check whether path is acceptable
    last(PathNew, [Xnew, Ynew, _]),
    touchdown(Xnew, Ynew), !, % if there is a touchdown - stop.
    % format("\n\nTouchDown:\nScore: ~w\n", [NextScore]),
    set_current_max(NextScore),
    PathFinish = PathNew,
    NewScore = NextScore.

% Recursive case: find an intermediate step between current position, such that you can move to touchdown from it.
go(Optimized, Xstart, Ystart, PathStart, CurrentScore, PassedThisRound, PathFinish, NewScore) :-
    % Do action in any direction from given coordinates
    do_action(Xstart, Ystart, _, PathStart, PassedThisRound, CurrentScore, true, PathMid, PassedThisRoundMid, NewScoreMid),
    should_continue_backtrack(Optimized, NewScoreMid), % check for optimization
    last(PathMid, [Xmid, Ymid, _]),
    \+member([Xmid,Ymid, _], PathStart),
    % format("New Path: ~w\nScore: ~w\n\n", [PathMid, NewScoreMid]),
    go(Optimized, Xmid, Ymid, PathMid, NewScoreMid, PassedThisRoundMid,  PathFinish, NewScore).

% a predicate to start backtracking if you don't win or die at (0,0) 
find_touchdown(Optimized, Path, Score) :-
    get_max_steps_count(MaxScore),
    set_current_max(MaxScore),
    first_step_check(Solved),
    ((Solved = win) -> (Path =[[0,0,init]], Score = 0) % the best path will definetely be at (0;0)
    ;(Solved = die) -> false
    ;(Solved = continue) -> go(Optimized, 0, 0, [[0,0,init]], 0, false, Path, Score)).

% In interface for optimized backtracking. Finds the best possible path if any.
find_best_path_optimized(Path, Score) :-
    findall( [P,S], find_touchdown(true, P, S), Bag),
    last(Bag, [Path, Score]).

% a predicate that return the best item based on predicate Goal.
select_element(Goal, [Head | Tail], Selected) :-
    select_element(Goal, Tail, Head, Selected).

% for an empty list return selected item
select_element(_Goal, [], Selected, Selected).

% otherwise select between selected element and list head
select_element(Goal, [Head | Tail], Current, FinalSelected) :-
    call(Goal, Head, Current, Selected),
    select_element(Goal, Tail, Selected, FinalSelected).

% A predicate returning a pair [Path, Score] with less Score
get_better_path(First, Second, Result) :-
    [_, FirstLength] = First, [_, SecondLength] = Second,
    FirstLength < SecondLength -> Result = First; Result = Second.

% An interface to find the best solution obtained by simple backtracking algorithm
find_best_path(Path, Score) :-
    findall( [P,S], find_touchdown(false, P, S), Bag),
    select_element(get_better_path, Bag, [Path, Score]), format("Best Score: ~w\n", [Score]),!.
% ===========================
% Random search
% A predicate to generate all possible moves from X, Y, without checking their validity
get_possible_steps(X,Y, Path, PassedThisRound, CurrentScore, PossibleSteps) :-
    findall(
        [NewPassedThisRound, NewScore, NewPath], 
        do_action(X,Y, _, Path, PassedThisRound, CurrentScore, false, NewPath, NewPassedThisRound, NewScore), 
        PossibleSteps).

% A predicate to make 1 random step from (X, Y)
% Generates list of possible moves, takes 1 at random
make_random_step(X, Y, Path, PassedThisRound, CurrentScore, NewPath, NewPassedThisRound, NewScore) :-
    get_possible_steps(X,Y, Path, PassedThisRound, CurrentScore, PossibleSteps),
    % format("\nPossibleSteps: ~w\n", [PossibleSteps]),
    length(PossibleSteps, PossibleStepsCount),
    random_between(1, PossibleStepsCount, RandomStepIndex),
    nth1(RandomStepIndex, PossibleSteps, RandomStep),
    [NewPassedThisRound, NewScore, NewPath] = RandomStep.

% Main random logic
% Makes random step, then check the validity of move. If invalid or step limit exceed - die, otherwise, continue.
generate_random_path(X,Y,Path, PassedThisRound, CurrentScore, StepsLeft, RandomPath, RandomPathScore) :-
    StepsLeft > 0,
    make_random_step(X,Y, Path, PassedThisRound, CurrentScore, NewPath, NewPassedThisRound, NewScore), 
    is_valid_path(true, NewPath), 
    % format("\nNewPath: ~w\n", [NewPath]),
    !, % Cut to disable change of random move
    last(NewPath, [NewX, NewY, _]),
    (
        (touchdown(NewX, NewY), RandomPath = NewPath, RandomPathScore = NewScore, !);
        (
            NewStepsLeft is StepsLeft - 1, 
            generate_random_path(NewX, NewY, NewPath, NewPassedThisRound, NewScore, NewStepsLeft, RandomPath, RandomPathScore)
        )
    ).
% An interface to run 1 random search
random_search(StepsAllowed, RandomPath, RandomPathScore) :-
    generate_random_path(0, 0, [[0,0,init]], false, 0, StepsAllowed, RandomPath, RandomPathScore).

% an interface to run Times random search
run_random_(Times, StepsAllowed, Path, Score):-
    between(1, Times, _),
    (random_search(StepsAllowed, Path, Score) 
    % ,format("Path:~w\nScore:~w\n\n", [Path, Score])
    ).
% an interface to return the best random search out of Times trials
run_random(Times, StepsAllowed, Path, Score) :-
    findall([P,S], run_random_(Times, StepsAllowed, P, S), Bag), !,
    select_element(get_better_path, Bag, [Path, Score]), !.

out_optimized :-
    time((find_best_path_optimized(P,S), print_path(P,S))).

out_backtracking :-
    time((find_best_path(P,S), print_path(P,S))).

out_random :-
    time((run_random(100, 100, P, S), print_path(P,S))).

test_bt_opt :-
    open("time.txt", append, Out),
    (find_best_path_optimized(_,S), format(Out, "Yes-~w ", [S]); write(Out, "No ")),
    close(Out).


test_rand :-
    open("time.txt", append, Out),
    (
        (run_random(100, 100, _, S), format(Out, "Yes-~w ", [S]); write(Out, "No "))
    ),
    close(Out).

test_bt :-
    open("time.txt", append, Out),
    (find_best_path(_, S), !, format(Out, "Yes-~w ", [S]); write(Out, "No ")),
    close(Out).

:-
    consult("input.pl").
    
