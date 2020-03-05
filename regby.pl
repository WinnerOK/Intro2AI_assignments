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
% fieldSize(5,5).
sight_distance(1).

% moveDirections: (name, dx, dy)
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

ork(X,Y) :- current_predicate(o/2), o(X,Y).
human(X,Y) :- current_predicate(h/2), h(X,Y).
touchdown(X,Y) :- current_predicate(t/2), t(X,Y).

print(X, Y) :-
    ( 
        human(X,Y) -> write("H") 
      ; ork(X,Y) -> write("O")
      ; touchdown(X,Y) -> write("T")
      ; write("█")
    ).

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

has_not_been(OldPath, NewPath):-
    last(NewPath, [X,Y,_]),
    \+member([X,Y,_], OldPath).

is_valid_position(X,Y):-
    is_inbound(X,Y),
    \+ork(X,Y).

is_valid_path(CheckValidity, Path):-
    CheckValidity -> (
        last(Path, [X,Y,_]),
        is_valid_position(X,Y)
    )
    ; true.

get_max_steps_count(Cnt) :-
    fieldSize(X,Y),
    Cnt is X*Y + 1.

set_current_max(CurMax) :-
    nb_setval(current_max, CurMax).

get_current_max(CurMax) :-
    nb_getval(current_max, CurMax).

around(X1,Y1, X2, Y2) :-
    ((abs(X1-X2)=:= 1) , ( abs(Y1-Y2)=:= 0)), ! ;
    ((abs(X1-X2) =:= 0) , (abs(Y1-Y2) =:= 1)), !;
    ((abs(X1-X2) =:= 1) , (abs(Y1-Y2) =:= 1)), !.

prevent_out_of_bounds_move_from_the_edge(X,Y, NewPath):-
    last(NewPath, [Xnew, Ynew, _]),
    (
        is_inbound(Xnew, Ynew) -> true
    ;   \+around(X,Y, Xnew, Ynew)    
    ).

should_continue_backtrack(Check, Score) :-
    \+Check -> true
    ; get_current_max(MaxScore),
      Score =< MaxScore.

time(P) :-
    open("time.txt", append, Out),
    statistics(walltime, _),
    P,
    statistics(walltime, [_ , T]),
    write(Out, T), 
    % write(Out, ' msec'), 
    write(Out, "\n"),
    close(Out).
% ===========================
% Api

show_map :- 
    fieldSize(_, YMax),
    Y is YMax - 1,
    iterate(0,Y).

are_adjacent(X1,Y1,X2,Y2) :-
    ((abs(X1-X2)=:= 1) , ( abs(Y1-Y2)=:= 0)), ! ;
    ((abs(X1-X2) =:= 0) , (abs(Y1-Y2) =:= 1)), !.

is_inbound(X,Y) :-
    fieldSize(XSize, YSize),
    X >= 0, X < XSize,
    Y >= 0, Y < YSize.
    
attempt_pass(X,Y, Dir, Path, CurrentScore, NewPath, NewScore) :-
    throwDir(Dir, Dx, Dy),
    Xnew is X + Dx, Ynew is Y + Dy,
    % is_inbound(Xnew, Ynew),
    (
        % First 2 conditions were used to fail the predicate, but due to random search needs
        % I decided to move check on the upper level
        \+is_inbound(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath), NewScore is CurrentScore + 1
    ;   ork(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath), NewScore is CurrentScore + 1
    ;   human(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath), NewScore is CurrentScore + 1
    ;   attempt_pass(Xnew, Ynew, Dir, Path, CurrentScore, NewPath, NewScore)
    ).

attempt_move(X,Y, Dir, Path, CurrentScore, NewPath, NewScore) :-
    is_inbound(X,Y),
    moveDir(Dir, Dx, Dy),
    Xnew is X + Dx, Ynew is Y + Dy,
    
    % This check wasn't move since even in random search player knows it's pos
    is_inbound(Xnew, Ynew),
    % \+ork(Xnew, Ynew), % Check moved on upper layer
    append(Path, [[Xnew, Ynew, move]], NewPath),
    (
        human(Xnew, Ynew) -> NewScore = CurrentScore 
    ;   NewScore is CurrentScore + 1     
    ).

do_action(X,Y,Dir, Path, PassedThisRound, CurrentScore, CheckValidity,  
        NewPath, NewPassedThisRound, NewScore) :-
    (
        \+PassedThisRound,  attempt_pass(X, Y, Dir, Path, CurrentScore, NewPath, NewScore),
        is_valid_path(CheckValidity, NewPath), has_not_been(Path, NewPath), 
        prevent_out_of_bounds_move_from_the_edge(X,Y, NewPath),
        NewPassedThisRound = true
    );
    (   attempt_move(X, Y, Dir, Path, CurrentScore, NewPath, NewScore), 
        is_valid_path(CheckValidity, NewPath), has_not_been(Path, NewPath), 
        NewPassedThisRound = PassedThisRound
    ).

count_score([], CurrentScore, FinalScore) :-
    FinalScore = CurrentScore.

count_score([[X,Y,Action]| Tail], CurrentScore, FinalScore) :-
    ((Action = move, human(X,Y)) -> NewScore is CurrentScore; NewScore is CurrentScore + 1),
    count_score(Tail, NewScore, FinalScore). 

first_step_check(Solved):-
    ork(0,0) -> ( Solved = die)
    ; touchdown(0,0) -> (Solved = win)
    ; (Solved = continue, true).

% ===========================
% Backtracking
go(Optimized, Xstart, Ystart, PathStart, CurrentScore, _, PathFinish, NewScore) :-
    attempt_move(Xstart, Ystart, _, PathStart, CurrentScore, PathNew, NextScore),
    should_continue_backtrack(Optimized, NextScore),
    last(PathNew, [Xnew, Ynew, _]),
    touchdown(Xnew, Ynew),
    % format("\n\nTouchDown:\nScore: ~w\n", [NextScore]),
    set_current_max(NextScore),
    PathFinish = PathNew,
    NewScore = NextScore.

go(Optimized, Xstart, Ystart, PathStart, CurrentScore, PassedThisRound, PathFinish, NewScore) :-
    do_action(Xstart, Ystart, _, PathStart, PassedThisRound, CurrentScore, true, PathMid, PassedThisRoundMid, NewScoreMid),
    should_continue_backtrack(Optimized, NewScoreMid),
    last(PathMid, [Xmid, Ymid, _]),
    \+member([Xmid,Ymid, _], PathStart),
    % format("New Path: ~w\nScore: ~w\n\n", [PathMid, NewScoreMid]),
    go(Optimized, Xmid, Ymid, PathMid, NewScoreMid, PassedThisRoundMid,  PathFinish, NewScore).
    
find_touchdown(Optimized, Path, Score) :-
    get_max_steps_count(MaxScore),
    set_current_max(MaxScore),
    first_step_check(Solved),
    ((Solved = win) -> (Path =[[0,0,init]], Score = 0) % the best path will definetely be at (0;0)
    ;(Solved = die) -> false
    ;(Solved = continue) -> go(Optimized, 0, 0, [[0,0,init]], 0, false, Path, Score)).

find_best_path_optimized(Path, Score) :-
    findall( [P,S], find_touchdown(true, P, S), Bag),
    last(Bag, [Path, Score]).

select_element(Goal, [Head | Tail], Selected) :-
    select_element(Goal, Tail, Head, Selected).


select_element(_Goal, [], Selected, Selected).

select_element(Goal, [Head | Tail], Current, FinalSelected) :-
    call(Goal, Head, Current, Selected),
    select_element(Goal, Tail, Selected, FinalSelected).

get_better_path(First, Second, Result) :-
    [_, FirstLength] = First, [_, SecondLength] = Second,
    FirstLength < SecondLength -> Result = First; Result = Second.

find_best_path(Path, Score) :-
    % Pass FromAtMost > 0 to consider "FromAtMost" otherwise will consider all solutions
    findall( [P,S], find_touchdown(false, P, S), Bag),
    select_element(get_better_path, Bag, [Score, Path]).
% ===========================
% Random search

get_possible_steps(X,Y, Path, PassedThisRound, CurrentScore, PossibleSteps) :-
    findall(
        [NewPassedThisRound, NewScore, NewPath], 
        do_action(X,Y, _, Path, PassedThisRound, CurrentScore, false, NewPath, NewPassedThisRound, NewScore), 
        PossibleSteps).

make_random_step(X, Y, Path, PassedThisRound, CurrentScore, NewPath, NewPassedThisRound, NewScore) :-
    get_possible_steps(X,Y, Path, PassedThisRound, CurrentScore, PossibleSteps),
    % format("\nPossibleSteps: ~w\n", [PossibleSteps]),
    length(PossibleSteps, PossibleStepsCount),
    random_between(1, PossibleStepsCount, RandomStepIndex),
    nth1(RandomStepIndex, PossibleSteps, RandomStep),
    [NewPassedThisRound, NewScore, NewPath] = RandomStep.


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

random_search(StepsAllowed, RandomPath, RandomPathScore) :-
    generate_random_path(0, 0, [[0,0,init]], false, 0, StepsAllowed, RandomPath, RandomPathScore).

run_random(Times, StepsAllowed):-
    between(1, Times, _),
    ((random_search(StepsAllowed, Path, Score), format("Path:~w\nScore:~w\n\n", [Path, Score])) ; format("No sol\n\n")).
    
test_bt_opt :-
    open("time.txt", append, Out),
    time(find_best_path_optimized(_,_); write(Out, "No ")),
    close(Out).


test_rand :-
    open("time.txt", append, Out),
    time(
        (random_search(100, _, _); write(Out, "No "))
    ),
    close(Out).

test_bt :-
    open("time.txt", append, Out),
    time(find_best_path(_, _); write(Out, "No ")),
    close(Out).

:-
    consult("input.pl"),
    test_bt_opt, halt(0).
    