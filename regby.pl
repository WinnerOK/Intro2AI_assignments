% Coondinate system:
% Y 
% 4|***OT
% 3|*O***
% 2|*O*O*
% 1|HO*O*
% 0|*O***
%   01234 X

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
% Variables

% :- dynamic([
%     ball/2,
%     passed_this_round/1
%     % h/2,
%     % o/2,
%     % t/2
% ]).

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
    ;   human(Xnew, Ynew) -> append(Path, [[Xnew, Ynew, "Throw"]], NewPath)
    ;   attempt_pass(Xnew, Ynew, Dir, Path, NewPath)
    ).

attempt_move(X,Y, Dir, Path, NewPath) :-
    is_inbound(X,Y),
    moveDir(Dir, Dx, Dy),
    Xnew is X + Dx, Ynew is Y + Dy,
    
    is_inbound(Xnew, Ynew),
    \+ork(Xnew, Ynew),
    append(Path, [[Xnew, Ynew, "Move"]], NewPath).
    

do_action(X,Y,Dir, Path, PassedThisRound,  NewPath, NewPassedThisRound) :-
    (\+PassedThisRound,  attempt_pass(X, Y, Dir, Path, NewPath), NewPassedThisRound = true);
    (attempt_move(X, Y, Dir, Path, NewPath), NewPassedThisRound = PassedThisRound).

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
    

find_touchdown(Path) :- go(0, 0, [[0, 0, "Init"]], false, Path).

:-
    init.

