%% Simple example
%% States are positive integers.
%% we can move from n to n+1 and n+2, both with cost 1
%% equality is arithmetic equality.
%% the goal is reaching 20
%% The hval is the difference with 10 divided by two.
%%
%% We need to set up the following functions in order to use Astar


% 1. Successor state function, returns list of successors of a state,
%    annotated by cost.  
%
%    Note structure of neighbours list, a list of pair of the form (Cost,State),
%    where Cost is the cost of getting to State from the current state.

successors(X,[(1,Y), (1,Z)]) :- Y is X +1, Z is X+2.  

% 2. Goal test function. Make this flexible.
%    A* only takes a single function as an argument, but we can make
%    that function recognize different goals as follows:

%% First we define a predicate which sets and gets the current goal state.

setGoalState(X) :- retractall(goalState(_)), assertz(goalState(X)).
getGoalState(X) :- goalState(X).

%% Now the goal test can test against the currently set goal state. 

goal(X) :- getGoalState(G), X=G.

%% We simply pass astar "goal", and use setGoalState to dynamically
%% alter the current goal.

%3. Heuristic functions.

hfn(X,Val) :- goal(Y), Z is (X-Y)//2, abs(Z,Val).
hfnUniform(_,0).              % causes breadth first search.


abs(X,Y) :- X < 0, !, Y is -X.
abs(X,X).

%%Sample Searches

% ?- setGoalState(20), astar(0, successors, goal, hfn, Path, =).

%%Note that astarCC requires a state equality function, in this case
%%the built in "=" works.

% ?- setGoalState(20), astarCC(0, successors, goal, hfn, Path, =).

% ?- setGoalState(20), idastar(0, successors, goal, hfn, Path, =).
