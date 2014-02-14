%%Starter code for 384-A1, Last modified: Jan 25 2014 - Sheila McIlraith
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  CSC384 Winter 2014, Assignment 1
%%
%%  NAME:Vincent Lee
%%
%%  STUDENT NUMBER:997454419
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* load the three search algorithms. Please do NOT change these lines */
:- ensure_loaded('astar.pl').
:- ensure_loaded('astarCC.pl').
:- ensure_loaded('idastar.pl').



/*----------------------------------------------------------------------
   Definitions of three sample mazes that you will run your code. 
   Do not change these! 
   You are strongly encouraged to also test your code with other mazes
     of various sizes and start/goal postioins (simply add
    more mazes to the list).
------------------------------------------------------------------------*/

%% TEST CASES
%% A LIST OF MAZES TO BE TESTED:
%% maze1: 9x9 maze, start 1/1, goal 9/9
%% maze2: 9x9 maze, start 1/1, goal 9/9 
%% maze3: 9x9 maze, start 1/1, goal 8/2 
mazesDatabase([
   maze(maze1,9, 9, [4/1, 4/2, 3/3, 2/4, 2/7, 2/8, 1/9, 3/6, 4/5, 5/4, 
                     6/3, 6/2, 7/2, 7/5, 7/6, 8/7, 6/6, 6/8, 9/8], 1/1, 9/9),
   maze(maze2, 9, 9, [1/2, 3/4, 5/5, 6/7, 1/7, 8/5, 9/8, 8/7, 5/9, 
                      6/9], 1/1, 9/9),
   maze(maze3, 9, 9, [2/1, 3/2, 4/3, 4/5, 3/6, 2/7, 5/4, 6/3, 7/2, 6/6, 
                      7/7, 8/8, 5/5], 1/1, 8/2)  ]).



/*----------------------------------------------------------------------
  PUT YOUR IMPLEMENTATION BELOW
  You must define the following functions before you can call any search algorithm:
-------------------------------------------------------------------------*/

%%   1. The successor state function.
%%   successors(+State,-Succs)
%%     Succs is a list of elements (Cost, NewState) where
%%     NewState is a state reachable from State by a move and
%%     Cost is the cost (here simply 1) for that corresponding move.
%% ------------------------------------------------------------------------------------------------------------
%% right, left, up, down

successors(C/R, Succs):-
	maze( _, M, N, O, _ , _),
	(C < N, XR is C+1, not(member(XR/R,O)) -> SR = [(1,XR/R)]; SR = []), 	
	(C > 1, XL is C-1, not(member(XL/R,O)) -> SL = [(1,XL/R)]; SL = []), 	
	(R > 1, XU is R-1, not(member(C/XU,O)) -> SU = [(1,C/XU)]; SU = []), 	
	(R < M, XD is R+1, not(member(C/XD,O)) -> SD = [(1,C/XD)]; SD = []), 	
	append(SR, SL, Temp),
	append(SU, SD, Temp2),
	append(Temp, Temp2, Succs).

%%
%% 2. State equality test when two states are equal.
%%      equality(+State1,+State2) 
%%

equality( A/B, C/D) :-
	A is C, 
	B is D. 	

%%
%% 3. Four different heuristic functions:
%%    * hfnUniform(+State, -Val)   (WE HAVE PROVIDED YOU THE IMPLEMENTATION BELOW)
%%    * hfnManhattan(+State, -Val)
%%    * hfnEuclid(+State, -Val)
%%    * hfnMyHeuristic(+State, -Val)
%%  Feel free to add helperS if needed. Note:  if you decide not implement any 
%%  of the three functions, please comment them so your code still compiles
%%

%% the given NULL Heuristic,  (do not change this!)
hfnUniform(_,0).       % causes search algorithm to do uniform costs search.

%% Implement the Manhattan Distance
hfnManhattan(X/Y, Val) :- 
	maze(_, _, _, _, _, C/R),
	Val is abs(C - X) + abs(R - Y).

%% Implement the Rounded Euclidean Distance  (you may use sqrt and floor)
hfnEuclid(X/Y, Val) :- 
	maze(_, _, _, _, _, C/R),
	sqrt((C - X)*(C - X) + (R - Y)*(R - Y), A ),
	floor(A, Val).

%% Implement your own heuristic function

incr( X, X1) :-
	X1 is X+1.

hfnMyHeuristic(X/Y, F) :-
	maze(_, _, _, O, _, C/R),
	hfnManhattan(X/Y, MD),
	hfnEuclid(X/Y, E),
	successors(X/Y, NS),
	length(NS, L),	
	F is MD + E - L.	


/*-------------------------------------------------------------------------
  Below are helper to run your code and see the results.
  Do not change these!
--------------------------------------------------------------------*/


%% A helper function to print a solution in matrix form
%%   showSol(+SolPathList)
showSol(SolPathList) :- 
     maze(_MazeName, _M, _N, O, S, G),
     member(R,[1,2,3,4,5,6,7,8,9]), nl,   %pick a row 1..9
     member(C,[1,2,3,4,5,6,7,8,9]),       %pci a col  1..9
     Loc = C/R,
     ( Loc = S -> write('S');        %is start position 
         Loc = G -> write('G');    %s goal position 
           member(Loc, SolPathList) -> write('*');  %is on solution path
             member(Loc,O) -> write('X');    %is an obstacle
               write('-')),           %otherwise is a normal position 
     fail;  %this causes to backtrack and loop over. 
     true.  


%% Preparing the "goal" predicate given goal state G
%% by removing any existing "goal" predicate and asserting goal(G) 
%%    prepareGoalPredicate(+G)
prepareGoalPredicate(G):-
	retractall(goal(_)), %retract any pref definitions
        assert(goal(G)).     %the goal state simply is G


%% Loading the maze info for a particulare given maze name
%% we use mazesDatabase to find the particular maze and load
%% it into memeory (removing any previous maze from memory first)
loadMazeInfo(MazeName) :-
        mazesDatabase(MazesList),
        member(maze(MazeName, M, N, O, S, G), MazesList), %finding the maze with name MazeName
        retractall(maze(_,_,_,_,_,_)),   %removing any previous maze defintion from memory
        assert(maze(MazeName, M, N, O, S, G)). %loading the maze info into memory


%% running the astar for a specific maze and heuristic function
%%  go(+MazeName, +HFN)
go(MazeName, HFN) :-
        loadMazeInfo(MazeName), %loading the maze info
        maze(_, _, _, _, S, G), %obtaining the S and G states.
        prepareGoalPredicate(G),
        write('Solving maze '), write(MazeName), writeln(':'),
        astar(S, successors, goal, HFN, Path, equality),
        writeln(Path),  %printing the solution path
        showSol(Path).  %pretty printing the solution in matrix form


%% running the astarCC for a specific maze and heuristic function
%%  goCC(+MazeName, +HFN)
goCC(MazeName, HFN) :-
        loadMazeInfo(MazeName), %loading the maze info
        maze(_, _, _, _, S, G), %obtaining the S and G states.
        prepareGoalPredicate(G),
        write('Solving maze '), write(MazeName), writeln(':'),
        astarCC(S, successors, goal, HFN, Path, equality),
        writeln(Path),  %printing the solution path
        showSol(Path).  %pretty printing the solution in matrix form

%% running the idastar for a specific maze and heuristic function
%%  goIDA(+MazeName, +HFN)
goIDA(MazeName, HFN) :-
        loadMazeInfo(MazeName), %loading the maze info
        maze(_, _, _, _, S, G), %obtaining the S and G states.
        prepareGoalPredicate(G),
        write('Solving maze '), write(MazeName), writeln(':'),
        idastar(S, successors, goal, HFN, Path, equality),
        writeln(Path),  %printing the solution path
        showSol(Path).  %pretty printing the solution in matrix form

