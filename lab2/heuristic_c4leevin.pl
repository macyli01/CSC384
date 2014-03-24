%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CSC384 Introduction to Artificial Intelligence, Winter 2014
% Assignment 2
%
% Instructions for preparing your competition files:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If you decide to participate in the competition, you should
% submit a file named "heuristic_<id>.pl", where <id> should
% be your cdf id. For example, a student John Doe with id "c4leevin" would
% submit the file named "heuristic_c4leevin.pl".
% 
% This file should contain a predicate named "<id>_h/2"
% (John would name it "c4leevin_h"). This predicate
% can rely on the functions you are required to implement (eg. tie, winner,
% nextState etc). Any helper predicates
% should also be included in the heuristic file.
% To avoid name clashes, prefix any helper predicates with your id. Note
% that any calls made to them from this file should
% use the new predicate names.
% 
% You should also duplicate the functions lowerBound/1 and upperBound/1
% in this file, again, prefixing them with your id.
% 
% The following is an example submission by John. He clearly did
% not understand the game and didn't use comments properly,
% but he got the naming convention right!
% 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------------
% Surname: Lee
% First Name: Vincent
% Student Number: 997454419

% Helper predicates
c4leevin_max(C1, C2, C) :- C1 >= C2, !, C=C1.
c4leevin_max(C1, C2, C2) :- C1 < C2.

% The actual heuristic
c4leevin_h(State, Val) :- terminal(State), !, Val=0.
c4leevin_h(State, Val) :- 
	( terminal(State) ->
		moves(1, State, MvList1),
		moves(2, State, MvList2),
		length(MvList1, L1),
		length(MvList2, L2),
		Val is L1 - L2	
	; Val is 0).

% The bounds
c4leevin_lowerBound(-36).
c4leevin_upperBound(36).

