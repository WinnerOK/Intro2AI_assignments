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
fieldSize(5,5).
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

init :-
    use_module(library(clpb)),
    consult("input.pl").

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
    
% ===========================
% Api

show_map :- 
    fieldSize(_, YMax),
    Y is YMax - 1,
    iterate(0,Y).

are_adjacent(X1,Y1,X2,Y2) :-
    Dx is abs(X1-X2), Dy is abs(Y1-Y2),
    sat(((Dx=:= 1) * ( Dy=:= 0)) + ((Dx =:= 0) * (Dy =:= 1))).

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

% ===========================
% Backtracking
go(Xstart, Ystart, PathStart, CurrentScore, _, PathFinish, NewScore) :-
    attempt_move(Xstart, Ystart, _, PathStart, CurrentScore, PathNew, NextScore),
    get_current_max(MaxScore),
    NextScore =< MaxScore,
    last(PathNew, [Xnew, Ynew, _]),
    touchdown(Xnew, Ynew),
    set_current_max(NextScore),
    PathFinish = PathNew,
    NewScore = NextScore.

go(Xstart, Ystart, PathStart, CurrentScore, PassedThisRound, PathFinish, NewScore) :-
    do_action(Xstart, Ystart, _, PathStart, PassedThisRound, CurrentScore, true, PathMid, PassedThisRoundMid, NewScoreMid),
    get_current_max(MaxScore),
    NewScoreMid =< MaxScore,
    last(PathMid, [Xmid, Ymid, _]),
    \+member([Xmid,Ymid, _], PathStart),
    go(Xmid, Ymid, PathMid, NewScoreMid, PassedThisRoundMid,  PathFinish, NewScore).
    
find_touchdown(Path, Score) :-
    get_max_steps_count(MaxScore),
    set_current_max(MaxScore),
    go(0, 0, [[0,0,init]], 0, false, Path, Score).

find_best_path(Path, Score) :-
    % Pass FromAtMost > 0 to consider "FromAtMost" otherwise will consider all solutions
    findall( [P,S], find_touchdown(P, S), Bag),
    last(Bag, [Path, Score]).
% ===========================
% Random search

get_possible_steps(X,Y, Path, PassedThisRound, CurrentScore, PossibleSteps) :-
    findall(
        [NewPassedThisRound, NewScore, NewPath], 
        do_action(X,Y, _, Path, PassedThisRound, CurrentScore, false, NewPath, NewPassedThisRound, NewScore), 
        PossibleSteps).

make_random_step(X, Y, Path, PassedThisRound, CurrentScore, NewPath, NewPassedThisRound, NewScore) :-
    get_possible_steps(X,Y, Path, PassedThisRound, CurrentScore, PossibleSteps),
    length(PossibleSteps, PossibleStepsCount),
    random_between(1, PossibleStepsCount, RandomStepIndex),
    nth1(RandomStepIndex, PossibleSteps, RandomStep),
    [NewPassedThisRound, NewScore, NewPath] = RandomStep.


generate_random_path(X,Y,Path, PassedThisRound, CurrentScore, StepsLeft, RandomPath, RandomPathScore) :-
    StepsLeft > 0,
    once(make_random_step(X,Y, Path, PassedThisRound, CurrentScore, NewPath, NewPassedThisRound, NewScore)), 
    is_valid_path(true, NewPath), !, % Cut to disable change of random move
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

 :-
    init.