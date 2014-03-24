/* ----------------------------------------------------------
    CSC384 Assignment 2 

% Surname:Lee
% First Name:Vincent
% Student Number:997454419 

  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- ensure_loaded('play.pl').

% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */


% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a piece in this position
%    2 means player two has a piece in this position. 



% given helper: Inital state of the board 
initBoard([ [.,.,.,.,.,.], 
			[.,.,.,.,.,.],
			[.,.,1,2,.,.], 
			[.,.,2,1,.,.], 
			[.,.,.,.,.,.], 
			[.,.,.,.,.,.] ]).
 
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 

initialize(InitialState,1):-
	initBoard(InitialState).

%% Helper Functions
%% define count_pieces( List, Player, Number of pieces)
%% Counts the number of pieces the player owns in a particular row.

count_pieces([], _ , 0).
count_pieces([H|T], H, C):-
	count_pieces(T, H, C2),
	C is C2+1. 
count_pieces([H|T], P, C):-
	H \= P,
	count_pieces(T, P, C2), 
	C is C2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Plyr):-	
	(tie(State) -> false; 
		(terminal(State) -> 
			flatten(State, S),
			count_pieces(S, 1, Score1),
			count_pieces(S, 2, Score2),
			(Score1 > Score2 ->	Plyr is	1 ; Plyr is 2)	
		; false	
		)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State):-
	terminal(State), 
	flatten(State, S),
	count_pieces(S, 1, Score1),
	count_pieces(S, 2, Score2),
	Score2 is Score1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State):- moves(1, State, []), moves(2, State, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moveRow(Plyr, State, MvList, Row ):-
	(Row >= 6 -> 
		MvList = []
	;
		moveCol(Plyr, State, MvList1, Row, 0 ),
		NewRow is Row + 1,
		moveRow(Plyr, State, MvList2, NewRow),
		append(MvList1, MvList2, MvList)
	).

moveCol(Plyr, State, MvList, Row, Col):-
	(Col >= 6 -> 
		MvList = []
	;
		(validmove(Plyr, State, [Row, Col]) ->
			H = [[Row, Col]]; H = []
		), 
		NewCol is Col + 1 ,
		moveCol(Plyr, State, T, Row, NewCol ),
		append(H, T, MvList)
	).


moves(Plyr, State, MvList):-
	moveRow(Plyr, State, MvList, 0).

%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

%%Helper for nextState, flips the pieces if consumable.
flipDir(Plyr, NextPlyr, State, [Row, Col], DX, DY, NewState):-
	(validDir(State, Plyr, [Row, Col], DX, DY) ->
		flip(Plyr, NextPlyr, State, [Row, Col], DX, DY, NewState)
	; NewState = State
	).

flip(Plyr, Opponent, State, [Row, Col], DX , DY, NewState):-
	NewRow is Row + DY,
	NewCol is Col + DX,
	(get(State, [NewRow, NewCol], Opponent) ->
		set(State, Temp, [NewRow, NewCol], Plyr),
		flip(Plyr, Opponent, Temp, [NewRow, NewCol], DX, DY, NewState)
	; NewState = State
	).
	
nextState(Plyr,[Row, Col],State,NewState,NextPlyr):-
	validmove(Plyr, State, [Row, Col]),
	(Plyr is 1 -> NextPlyr = 2; NextPlyr = 1),
	set(State, Temp, [Row, Col], Plyr),		
	flipDir(Plyr, NextPlyr, Temp,  [Row, Col], -1, -1, Temp2),
	flipDir(Plyr, NextPlyr, Temp2, [Row, Col], -1,  0, Temp3),
	flipDir(Plyr, NextPlyr, Temp3, [Row, Col], -1,  1, Temp4),
	flipDir(Plyr, NextPlyr, Temp4, [Row, Col],	 0, -1, Temp5),
	flipDir(Plyr, NextPlyr, Temp5, [Row, Col],	 0,  1, Temp6),
	flipDir(Plyr, NextPlyr, Temp6, [Row, Col],	 1, -1, Temp7),
	flipDir(Plyr, NextPlyr, Temp7, [Row, Col],	 1,  0, Temp8),
	flipDir(Plyr, NextPlyr, Temp8, [Row, Col],	 1,  1, NewState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

%%Move must be within the board
inBound(R,C):-
	R >= 0,
	R < 6, 
	C >= 0,
	C < 6.

%% Valid Position checks if the proposed locaiton can bracket the opponent
validPosition(State, [Row, Col], Opponent, DX, DY) :-
	get(State, [Row, Col], Value),
	Value \= '.',
	(Value \= Opponent -> 
		NewRow is Row + DY, NewCol is Col + DX,
		validPosition(State, [NewRow, NewCol], Opponent, DX, DY)
	; true).

validDir( State, Plyr, [Row, Col], DX, DY):-
	(Plyr is 1 -> Opponent = 2; Opponent = 1),
	NewRow is Row + DY, 
	NewCol is Col + DX, 
	get(State, [NewRow, NewCol], Opponent),
	validPosition(State, [NewRow, NewCol], Plyr, DX, DY).

validmove(Plyr, S, [R,C]):-
	inBound(R, C),
	get(S, [R, C], Value),
	Value == '.',
	(
	validDir( S, Plyr, [R, C],  1,  0);
	validDir( S, Plyr, [R, C],  0,  1);
	validDir( S, Plyr, [R, C], -1,	0);
	validDir( S, Plyr, [R, C],  0, -1);

	validDir( S, Plyr, [R, C],  1,  1);
	validDir( S, Plyr, [R, C],	1, -1);
	validDir( S, Plyr, [R, C], -1,  1);
	validDir( S, Plyr, [R, C], -1, -1)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(_, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.

lowerBound(-100).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(100). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position row R column C
% set(Board, NewBoard, [R, C], Value):
%    : set Value at row R column C in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [0,5], the lower left
% hand corner has index [5,0], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [3,2], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [4,2], 1), set(NB1, NB2, [3,2], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% row R column C (indexing starts at 0).
get( Board, [R, C], Value) :- 
	nth0( R, Board, Row), 
	nth0( C, Row, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [0, C], Value)
    :- setInList(Row, NewRow, C, Value). 

set( [Row|RestRows], [Row|NewRestRows], [R, C], Value) :- 
	R > 0, 
	R1 is R-1, 
	set( RestRows, NewRestRows, [R1, C], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
