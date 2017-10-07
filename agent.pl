% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% COMP9414 Project 3 Option 2 
% Prolog (BDI Agent)
% By Shikun LI 5126124 and 5029250 Ying Zhou as Group 450
% 28 May 2017
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [Question 1]
% A Prolog procedure trigger(Events, Goals) which takes a single list of events, 
% each of the form truffle(X,Y,S) or restaurant(X,Y,S), and computes the Goals for 
% the agent in the form of two separate lists of items in the form goal(X,Y,S). 
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

%Base case
trigger([],goals([],[])).
trigger([restaurant(X,Y,S)|Percepsts],goals([goal(X,Y,S)|Goals_rest],Goals_truff)):-
        trigger(Percepsts,goals(Goals_rest,Goals_truff)).
trigger([truffle(X,Y,S)|Percepsts],goals(Goals_rest,[goal(X,Y,S)]|Goals_truff)):-
        trigger(Percepsts,goals(Goals_rest,Goals_truff)).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [Question 2]
% A predicate which take three inputs:
% a set of Goals in the form goals(Goals_rest,Goals_truff)
% a set of Beliefs in the form beliefs(at(X,Y),stock(T))
% the current Intentions of the agent, in the form intents(Int_sell,Int_pick) where
% Int_sell, Int_pick are lists of intentions in the form [goal(X,Y,S), Plan].
% In each case, the new goals should be inserted into the existing list in 
% decreasing order of S, using the Manhattan distance from the agent's current 
% position to break ties.
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


%Base case.
incorporate_goals(goals([],[]),_,Intentions,Intentions):- !.

%Case of same goal in list
incorporate_goals(goals([goal(X,Y,S)|Goals_rest],Goals_truff), Beliefs, intents(Int_sell,Int_pick), Intentions1):-
	member(goal(X,Y,S),Int_sell),
	incorporate_goals(goals(Goals_rest,Goals_truff),Beliefs,intents(Int_sell,Int_pick),Intentions1).

%Case of same goal in list
incorporate_goals(goals(Goals_rest,[goals(X,Y,S)|Goals_truff]), Beliefs, intents(Int_sell,Int_pick), Intentions1):-
	member(goal(X,Y,S),Int_pick),
	incorporate_goals(goals(Goals_rest,Goals_truff),Beliefs, intents(Int_sell,Int_pick),Intentions1).
	 	 
%Regular cases
incorporate_goals(goals([goal(X,Y,S)|Goals_rest],Goals_truff), Beliefs, intents(Int_sell,Int_pick), intents(New_sell,New_pick)):-
	Goals_truff \= [],
	find_all(Int_sell,Sell),
	find_all(Int_pick,Pick),
	insert(Goals_rest, New_sell, Int_sell, Sell, X_B, Y_B),
	insert(Goals_truff, New_pick, Int_pick, Pick, X_B, Y_B).

%Using the recursive method to insert new sell and pick to Int_sell and Int_pick.
find_all([],[]).

find_all([[goal(X,Y,S),_]|Goals_rest], [[X,Y,S]|Tail]):-
	find_all(Goals_rest, Tail).

insert([], Int_sell, Int_sell,_,_,_).

%Find out if the same goal occours
insert([goal(X,Y,S)|Goals_rest], Insert_int, Int_sell,Sell, X_B, Y_B):-
	member([X,Y,S],Sell),
	insert(Goals_rest, Insert_int, Int_sell, Sell, X_B, Y_B).
	
insert([goal(X,Y,S)|Goals_rest], Insert_int, Int_sell,Sell, X_B, Y_B):-
	rank([goal(X,Y,S),[]], Int_sell, New_sell, X_B, Y_B),
	insert(Goals_rest, Insert_int, New_sell, Sell, X_B, Y_B),!.

%Ranking. Compares S first and then D.
%Regular cases	
rank([goal(X,Y,S),[]], [], [goal(X,Y,S),[]],_,_).

%Case with different Ss.
rank([goal(X_A,Y_A,S1), []], [[goal(X_C,Y_C,S2), []]|Goals_rest], [goal(X_A,Y_A,S1),[]], [[goal(X_C,Y_C,S2), []]|Goals_rest], X_B, Y_B):-
	S1>S2.

rank([goal(X_A,Y_A,S1), []], [[goal(X_C,Y_C,S2), []]|Goals_rest], [[goal(X_C,Y_C,S2), []]|Goals_rest2], X_B, Y_B):-
	S1<S2,
	rank([goal(X_A,Y_A,S1),[]], Goals_rest, Goals_rest2, X_B, Y_B).

%Case whit different Ds.
rank([goal(X_A,Y_A,S), []], [[goal(X_C,Y_C,S), []]|Goals_rest], [goal(X_A,Y_A,S), []], [[goal(X_C,Y_C,S), []]|Goals_rest], X_B, Y_B):-
	distance((X_A,Y_A),(X_B,Y_B),D1),
	distance((X_C,Y_C),(X_B,Y_B),D2),
	D1>D2.
	
rank([goal(X_A,Y_A,S), []], [[goal(X_C,Y_C,S), []]|Goals_rest], [[goal(X_C,Y_C,S), []]|Goals_rest2], X_B, Y_B):-
	distance((X_A,Y_A),(X_B,Y_B),D1),
	distance((X_C,Y_C),(X_B,Y_B),D2),
	D1<D2,
	rank([goal(X_A,Y_A,S),[]], Goals_rest, Goals_rest2, X_B, Y_B).
	
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [Question 3]
% A predicate get_action(Beliefs, Intentions, Intentions1, Action)
% which takes the agent's Beliefs in the form belief(at(X,Y),stock(T)) 
% and its current Intentions in the form intents(Int_sell,Int_pick) (as described above), 
% and computes an action to be taken by the agent as well as the updated Intentions. 
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% stay.
	get_action(beliefs(at(X_A,Y_A), stock(_T)), [], [], Action):-
    Action = move(X_A, Y_A),!.

% If the list Int_sell is not empty:
get_action(beliefs(at(X_A,Y_A), stock(T)), intents([[goal(X, Y, S), [move(X1,Y1)|Plan_rest]]|Int_sell],[]), Intentions1, Action) :- 
    applicable([at(X_A,Y_A), stock(T)], move(X1,Y1)),
    Action = move(X1,Y1),
    Intentions1 = [[[goal(X, Y, S), Plan_rest]|Int_sell],[]],!.
	
get_action(beliefs(at(X,Y), stock(T)), intents([[goal(X, Y, S), []]|Int_sell],Int_pick), intents,Intentions1,Action) :-
	S1 =< T,
	Intentions1 = [[[goal(X1, Y1, S1), Plan]|Int_sell],Int_pick],
	generate_plan(at(X,Y), goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action|Plan]).

get_action(beliefs(at(_X,_Y), stock(T)), intents([[goal(X, Y, S), [Action|Plan]]|Int_sell],Int_pick), Intentions1, Action) :- 
	S1 =< T,
	Intentions1 = [[[goal(X1, Y1, S1), Plan]|Int_sell],Int_pick],
	applicable(Action),!.

% If the list Int_pick is not empty
get_action(beliefs(at(X,Y), stock(T)), intents([[goal(X1, Y1, S1), _]|Int_sell],Int_pick), Intentions1, Action1) :-
	Intentions1 = [[[goal(X1, Y1, S1), Plan1]|Int_sell],Int_pick],
	generate_plan(at(X,Y), goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action1|Plan1]).

get_action(beliefs(at(X,Y), stock(T)), Intentions, Intentions1, Action):-
	Intentions = [Int_sell,[[goal(X1, Y1, S1), []]|Int_pick]],
	Intentions1 = (Int_sell,[[goal(X1, Y1, S1), Plan]|Int_pick]),
	generate_plan(at(X,Y), goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action|Plan]).

get_action(_Beliefs, intents(Int_sell,[[Goal, [Action|Plan]]|Int_pick]), intents(Int_sell,[[Goal, Plan]|Int_pick]), Action) :- 
	applicable(Action).

get_action(beliefs(at(X,Y), stock(T)), Intentions, Intentions1, Action1):- 
	\+ applicable(Action),
	Intentions = [Int_sell,[[goal(X1, Y1, S1), [Action|_Plan]]|Int_pick]],
	Intentions1 = [Int_sell,[[goal(X1, Y1, S1), Plan1]|Int_pick]], 
	generate_plan(at(X,Y), goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action1|Plan1]).	
	
% Base case of movement when the current location is the same as the goal.
generate_plan([at(X,Y)], goal(X,Y,_), _, [pick(X,Y)]) :- truffle(X,Y,_).
generate_plan([at(X,Y)], goal(X,Y,_), _, [sell(X,Y)]) :- restaurant(X,Y,_).

% Case which the goal is on the right side of the current location.
generate_plan([at(X_A,Y_A)], goal(X,Y,Value), Direction_X, MoveX) :-
    Direction_X > 0,
    X_A1 is X_A + 1,
    MoveX = [move(X_A1,Y_A)|Move_rest_X],
    generate_plan([at(X_A1,Y_A)], goal(X,Y,Value), Direction_X, Move_rest_X),!.

% Case which the goal is on the left side of the current location.
generate_plan([at(X_A,Y_A)], goal(X,Y,Value), Direction_X, MoveX) :-
    Direction_X < 0,
    X_A1 is X_A - 1,
    MoveX = [move(X_A1,Y_A)|Move_rest_X],
    generate_plan([at(X_A1,Y_A)], goal(X,Y,Value), Direction_X, Move_rest_X),!.
    
% Case which the goal is on the top side of the current location.
generate_plan([at(X,Y_A)], goal(X,Y,Value), Direction_Y, MoveY) :-
    Direction_Y > 0,
    Y_A1 is Y_A + 1,
    MoveY = [move(X,Y_A1)|Move_rest_Y],
    generate_plan([at(X,Y_A1)], goal(X,Y,Value), Direction_Y, Move_rest_Y),!.

% Case which the goal is on the bottom side of the current location.
generate_plan([at(X,Y_A)], goal(X,Y,Value), Direction_Y, MoveY) :-
    Direction_Y < 0,
    Y_A1 is Y_A - 1,
    MoveY = [move(X,Y_A1)|Move_rest_Y],
    generate_plan([at(X,Y_A1)], goal(X,Y,Value), Direction_Y, Move_rest_Y).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [Question 4]
% A predicate update_beliefs to compute the new beliefs resulting from the 
% agent's observations and update the agent's intentions
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% the case that the Action is move, the updated Beliefs is the new Beliefs.
update_beliefs(at(X,Y), beliefs(at(_X1,_Y1),stock(T)), beliefs(at(X,Y),stock(T))).

% the case that the Action is pick and sell, the updated Beliefs is the same as Beliefs.
update_beliefs(sold(X,Y,S), beliefs(at(X,Y),stock(T)), beliefs(at(X,Y),stock(T1))) :- T1 is T-S.
update_beliefs(picked(X,Y,S), beliefs(at(X,Y),stock(T)), beliefs(at(X,Y),stock(T1))) :- T1 is T + S.

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [Question 5]
% A predicate update_intentions to update the agent's intentions, based on 
% observation. In the case of a picked() or sold() observation, the agent should
% remove the corresponding plan from its list of intentions (since this plan 
% has now successfully been executed).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% the case that the Action is move, the updated Intentions is the same as new Intentions.
update_intentions(at(X,Y), Intentions, Intentions).

% the case that the Action is pick and sell, the updated Intentions is the same as the rest Intentions.
update_intentions(sold(X1,Y1,_), intents([[goal(X1, Y1, _), _]|Int_sell], Int_pick), intents(Int_sell, Int_pick)).
update_intentions(picked(X1,Y1,_), intents(Int_sell,[[goal(X1, Y1, _), _]|Int_pick]), intents(Int_sell,Int_pick)).
