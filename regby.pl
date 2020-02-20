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

% Calculating the best path is adapted from https://stackoverflow.com/questions/1660152/how-do-i-find-the-longest-list-in-a-list-of-lists
select_element(Goal, [Head | Tail], Selected) :-
    select_element(Goal, Tail, Head, Selected).

select_element(_Goal, [], Selected, Selected).

select_element(Goal, [Head | Tail], Current, FinalSelected) :-
    call(Goal, Head, Current, Selected),
    select_element(Goal, Tail, Selected, FinalSelected).

% ===========================
% Api

show_map :- 
    fieldSize(_, YMax),
    Y is YMax -1,
    iterate(0,Y).

are_adjacent(X1,Y1,X2,Y2) :-
    (abs(X1-X2) =:= 1, abs(Y1-Y2) =:= 0);
    (abs(X1-X2) =:= 0, abs(Y1-Y2) =:= 1).

is_inbound(X,Y) :-
    fieldSize(XSize, YSize),
    X >= 0, X < XSize,
    Y >= 0, Y < YSize.
    
attempt_pass(X,Y, Dir, Path, NewPath) :-
    throwDir(Dir, Dx, Dy),
    Xnew is X + Dx, Ynew is Y + Dy,
    is_inbound(Xnew, Ynew),
    (
        ork(Xnew, Ynew) -> false
    ;   human(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, pass]], NewPath)
    ;   attempt_pass(Xnew, Ynew, Dir, Path, NewPath)
    ).

attempt_move(X,Y, Dir, Path, NewPath) :-
    is_inbound(X,Y),
    moveDir(Dir, Dx, Dy),
    Xnew is X + Dx, Ynew is Y + Dy,
    
    is_inbound(Xnew, Ynew),
    \+ork(Xnew, Ynew),
    append(Path, [[Xnew, Ynew, move]], NewPath).

do_action(X,Y,Dir, Path, PassedThisRound,  NewPath, NewPassedThisRound) :-
    (\+PassedThisRound,  attempt_pass(X, Y, Dir, Path, NewPath), NewPassedThisRound = true);
    (attempt_move(X, Y, Dir, Path, NewPath), NewPassedThisRound = PassedThisRound).

count_score([], CurrentScore, FinalScore) :-
    FinalScore = CurrentScore.

count_score([[X,Y,Action]| Tail], CurrentScore, FinalScore) :-
    ((Action = move, human(X,Y)) -> NewScore is CurrentScore; NewScore is CurrentScore + 1),
    count_score(Tail, NewScore, FinalScore). 

% ===========================
% Backtracking
go(Xstart, Ystart, PathStart, _, PathFinish) :-
    attempt_move(Xstart, Ystart, _, PathStart, PathNew),
    last(PathNew, [Xnew, Ynew, _]),
    touchdown(Xnew, Ynew),
    PathFinish = PathNew, !.

go(Xstart, Ystart, PathStart, PassedThisRound, PathFinish) :-
    do_action(Xstart, Ystart, _, PathStart, PassedThisRound, PathMid, PassedThisRoundMid),
    last(PathMid, [Xmid, Ymid, _]),
    \+member([Xmid,Ymid, _], PathStart),
    go(Xmid, Ymid, PathMid, PassedThisRoundMid,  PathFinish).
    
find_touchdown(Path) :- 
    go(0, 0, [], false, Path).

find_best_path(FromAtMost, Path) :-
    % Pass FromAtMost > 0 to consider "FromAtMost" otherwise will consider all solutions
    (FromAtMost =< 0 -> findall( P, find_touchdown(P), Bag); findnsols(FromAtMost, P, find_touchdown(P), Bag)),
    select_element(get_less_score, Bag, Path).

get_less_score(FirstPath, SecondPath, Less) :-
    count_score(FirstPath, 0, FirstScore), count_score(SecondPath, 0, SecondScore),
    (FirstScore < SecondScore -> Less = FirstPath; Less = SecondPath).

% ===========================
% Random search

get_possible_steps(X,Y, Path, PassedThisRound, PossibleSteps) :-
    findall(
        [NewPassedThisRound|[NewPath]], 
        do_action(X,Y, _, Path, PassedThisRound, NewPath, NewPassedThisRound), 
        PossibleSteps).

make_random_step(X, Y, Path, PassedThisRound, NewPath, NewPassedThisRound) :-
    get_possible_steps(X,Y, Path, PassedThisRound, PossibleSteps),
    length(PossibleSteps, PossibleStepsCount),
    random_between(1, PossibleStepsCount, RandomStepIndex),
    nth1(RandomStepIndex, PossibleSteps, RandomStep),
    [NewPassedThisRound, NewPath] = RandomStep.

generate_random_path(X,Y,Path, PassedThisRound, StepsLeft, RandomPath) :-
    StepsLeft > 0,
    make_random_step(X,Y, Path, PassedThisRound, NewPath, _), !, % Cun to disable change of random move
    last(NewPath, [NewX, NewY, _]),
    touchdown(NewX, NewY),  
    RandomPath = NewPath.

% Out of global stack
%  TODO: debug https://marketplace.visualstudio.com/items?itemName=arthurwang.vsc-prolog#debugger-settings
generate_random_path(X,Y,Path, PassedThisRound, StepsLeft, RandomPath) :-
    StepsLeft > 0,
    make_random_step(X,Y, Path, PassedThisRound, NewPath, NewPassedThisRound), !, % Cun to disable change of random move
    last(NewPath, [NewX, NewY, _]),
    NewStepsLeft is StepsLeft - 1, 
    generate_random_path(NewX, NewY, NewPath, NewPassedThisRound, NewStepsLeft, RandomPath).
    
    

:-
    init.

