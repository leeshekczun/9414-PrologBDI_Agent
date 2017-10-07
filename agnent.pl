% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 1]
% A predicate that takes a list of events, each of the form truffle(X,Y,S), and
% computes the corresponding list of goals for the agent, each of the
% form goal(X,Y,S).
% 
% trigger(+Events, -Goals).
%   Takes a list of Events, each of the form truffle(X,Y,S), and computes the
%   corresponding list of Goals for the agent, each of the form goal(X,Y,S).
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

trigger([], []).
trigger([truffle(X, Y, S)|Tail], [goal(X, Y, S)|Goals]) :-
    trigger(Tail, Goals).
trigger([restaurant(X,Y,S)|Events], [goal(X,Y,S)|Goals]) :-
    trigger(Events, Goals).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 2]
% A predicate which has four arguments:
% - a list of goals each of the form goal(X,Y,S),
% - a list of beliefs (containing one term of the form at(X,Y)),
% - the current list of intentions each of the form [goal(X,Y,S), Plan],
% - a list to be computed which contains the new goals inserted into the
%   current list of intentions in decreasing order of value, using the distance
%   from the agent to break ties.
%
% A new goal will be placed immediately before the first goal in the list that
% has a lower value or which has an equal value and is further away from the
% agent's current position, without reordering the current list of goals.
% 
% incorporate_goals(+Goals, +Beliefs, +Intentions, -Intentions1).
%   Takes Goals list and inserts only the new goals into the Intentions list
%   immediately before an intention with a goal of a lower value. By lower value
%   first, the Score is compared with the Manhattan distance if the scores are
%   the same. The plan associated with each new goal is the empty plan.
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


%This procedure should take three inputs, as follows:
a set of Goals in the form goals(Goals_rest,Goals_truff)
a set of Beliefs in the form beliefs(at(X,Y),stock(T))
the current Intentions of the agent, in the form intents(Int_sell,Int_pick) where:
          Int_sell, Int_pick are lists of intentions in the form [goal(X,Y,S), Plan]


%Base case there are no more goals to be incorperated.
incorporate_goals(goals([],[]),_,Intentions,Intentions):- !.

%Case where restaurant goal is already in Restaurants list
incorporate_goals(goals([goal(X,Y,S)|Goals_rest],Goals_truff), Beliefs, intents(Int_sell,Int_pick), Intentions1):-
	member(goal(X,Y,S),Int_sell),
	incorporate_goals(goals(Goals_rest,Goals_truff),Beliefs,intents(Int_sell,Int_pick),Intentions1).

%Case where restaurant goal is already in Restaurants list
incorporate_goals(goals(Goals_rest,[goals(X,Y,S)|Goals_truff]), Beliefs, intents(Int_sell,Int_pick), Intentions1):-
	member(goal(X,Y,S),Int_pick),
	incorporate_goals(goals(Goals_rest,Goals_truff),Beliefs, intents(Int_sell,Int_pick),Intentions1).

%Case where restaurant goal is not in int_sell
incorporate_goals(goals([goal(X,Y,S)|Goals_rest],Goals_truff), Beliefs, [Int_sell,Int_pick] Intentions1):-
	insert_goal(goal(X,Y,S),Beliefs,Int_sell,AccumIntents),
	incorporate_goals(goals(Goals_rest,Goals_truff), Beliefs, AccumIntents, Intentions).

%Base case where there are no goals in the intentions list
insert_goal(goal(X,Y,S),_,[],[[goal(X,Y,S)],[]]):- !.

%Case where truffle goal is not in int_sell.
incorporate_goals(goals(Goals_rest,[goals(X,Y,S)|Goals_truff]), Beliefs, [Int_sell|Int_pick], Intentions1):-

% the recursive case, which the distance between beliefs and new goal is larger than current specified goal.
incorporate_a_goal(goal(X,Y,Value), [at(X_A,Y_A)], [[goal(X1,Y1,Value1),Plan]|RestOfIntentions], [[goal(X1,Y1,Value1),Plan]|RestOfIntentions1]) :-
    distance((X_A,Y_A),(X1,Y1),D_I),    
    distance((X_A,Y_A),(X,Y),D_G),  
    D_G > D_I,
    incorporate_a_goal(goal(X,Y,Value), [at(X_A,Y_A)], RestOfIntentions, RestOfIntentions1),!.

% the recursive case, which the distance between beliefs and new goal is smaller than current specified goal, then 
% insert the new goal with previous Intentions in order and terminate recursion. 
incorporate_a_goal(goal(X,Y,Value), [at(X_A,Y_A)], [[goal(X1,Y1,Value1),Plan]|RestOfIntentions], [[goal(X,Y,Value),[]]|RestOfIntentions1]) :-
     distance((X_A,Y_A),(X1,Y1),D_I),     
     distance((X_A,Y_A),(X,Y),D_G),
     D_G < D_I,
     RestOfIntentions1 = [[goal(X1,Y1,Value1),Plan]|RestOfIntentions],!.

% the recursive case, which the distance between beliefs and new goal is the same as current specified goal, and new 
% goal's value is smaller than the other. 
incorporate_a_goal(goal(X,Y,Value), [at(X_A,Y_A)], [[goal(X1,Y1,Value1),Plan]|RestOfIntentions], [[goal(X1,Y1,Value1),Plan]|RestOfIntentions1]) :-
     distance((X_A,Y_A),(X1,Y1),D_I),     
     distance((X_A,Y_A),(X,Y),D_G),
     D_G is D_I,
     Value =< Value1,
     incorporate_a_goal(goal(X,Y,Value), [at(X_A,Y_A)], RestOfIntentions, RestOfIntentions1),!.

% the recursive case, which the distance between beliefs and new goal is the same as current specified goal, and new 
% goal's value is larger than the other, inserting the new goal with previous Intentions in order and terminate recursion.
incorporate_a_goal(goal(X,Y,Value), [at(X_A,Y_A)], [[goal(X1,Y1,Value1),Plan]|RestOfIntentions], [[goal(X,Y,Value),[]]|RestOfIntentions1]) :-
     distance((X_A,Y_A),(X1,Y1),D_I),     
     distance((X_A,Y_A),(X,Y),D_G),
     D_G is D_I,
     Value > Value1,
     RestOfIntentions1 = [[goal(X1,Y1,Value1),Plan]|RestOfIntentions],!.


% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 3]
% A predicate which takes the agent's beliefs and the list of intentions,
% and computes an action to be taken by the agent and the updated list
% of intentions.
% % 
% get_action(Beliefs, Intentions, Intentions1, Action)
% which takes the agent's Beliefs in the form belief(at(X,Y),stock(T)) 
% and its current Intentions in the form intents(Int_sell,Int_pick) (as described above), 
% and computes an action to be taken by the agent as well as the updated Intentions. 
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% the case that there is no any intentions before, then just give a random move.
	
get_action(beliefs(at(X_A,Y_A), stock(_T)), [], [], Action):-
	X_A_Random is X_A + 1,
    Action = move(X_A_Random, Y_A),!.

get_action(beliefs(at(X_A,Y_A), stock(T)), intents([[goal(X, Y, S), [move(X1,Y1)|RestOfPlan]]|Int_sell],[]), Intentions1, Action) :- 
    applicable([at(X_A,Y_A), stock(T)], move(X1,Y1)),
    Action = move(X1,Y1),
    Intentions1 = [[[goal(X, Y, S), RestOfPlan]|Int_sell],[]],!.
	
get_action(beliefs(at(X,Y), stock(T)), intents([[goal(X, Y, S), []]|Int_sell],Int_pick), intents,Intentions1 Action) :-
	S1 =< T,
	Intentions1 = [[[goal(X1, Y1, S1), Plan]|Int_sell],Int_pick],
	generate_plan_plan(goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action|Plan]).

get_action(beliefs(at(_X,_Y), stock(T)), intents([[goal(X, Y, S), [Action|Plan]]|Int_sell],Int_pick), Intentions1, Action) :- 
	S1 =< T,
	Intentions1 = [[[goal(X1, Y1, S1), Plan]|Int_sell],Int_pick],
	applicable(Action),!.

get_action(beliefs(at(X,Y), stock(T)), intents([[goal(X1, Y1, S1), _]|Int_sell],Int_pick), Intentions1, Action1) :-
	Intentions1 = [[[goal(X1, Y1, S1), Plan1]|Int_sell],Int_pick],
	generate_plan_plan(goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action1|Plan1]).

get_action(beliefs(at(X,Y), stock(T)), Intentions, Intentions1, Action):-
	Intentions = [Int_sell,[[goal(X1, Y1, S1), []]|Int_pick]],
	Intentions1 = (Int_sell,[[goal(X1, Y1, S1), Plan]|Int_pick])
	generate_plan_plan(goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action|Plan]).

get_action(_Beliefs, intents(Int_sell,[[Goal, [Action|Plan]]|Int_pick]), intents(Int_sell,[[Goal, Plan]|Int_pick]), Action) :- 
	applicable(Action).

get_action(beliefs(at(X,Y), stock(T)), intents(Int_sell,[[goal(X1, Y1, S1), [Action|_Plan]]|Int_pick]), intents(Int_sell,[[goal(X1, Y1, S1), Plan1]|Int_pick]), Action1) :- 
	\+ applicable(Action),
	generate_plan_plan(goal(X1, Y1, S1), beliefs(at(X,Y), stock(T)), [Action1|Plan1]).
	
	
% the base case of horizontal movement: when the x of current location is the same as x of the goal.
generate_plan([at(X,_)], goal(X,_,_), _, []) :-!.

% the recursion case, which the goal is on the right side of the current location.
generate_plan([at(X_A,Y_A)], goal(X,Y,Value), Direction_X, HoriMove) :-
    Direction_X > 0,
    X_A1 is X_A + 1,
    HoriMove = [move(X_A1,Y_A)|RestOfHoriMove],
    generate_plan([at(X_A1,Y_A)], goal(X,Y,Value), Direction_X, RestOfHoriMove),!.

% the recursion case, which the goal is on the left side of the current location.
generate_plan([at(X_A,Y_A)], goal(X,Y,Value), Direction_X, HoriMove) :-
    Direction_X < 0,
    X_A1 is X_A - 1,
    HoriMove = [move(X_A1,Y_A)|RestOfHoriMove],
    generate_plan([at(X_A1,Y_A)], goal(X,Y,Value), Direction_X, RestOfHoriMove),!.



% the base case of vertical movement: when the y of the current location is the same as y of the goal.
geberate_plan([at(X,Y)], goal(X,Y,_), _, [pickup(X,Y)]) :-!.
    
% the recursive case, which the goal is on the up side of the current location.
geberate_plan([at(X,Y_A)], goal(X,Y,Value), Direction_Y, VertMove) :-
    Direction_Y > 0,
    Y_A1 is Y_A + 1,
    VertMove =  [move(X,Y_A1)|RestOfVertMove],
    geberate_plan([at(X,Y_A1)], goal(X,Y,Value), Direction_Y, RestOfVertMove),!.

% the recursive case, which the goal is on the down side of the current location.
geberate_plan([at(X,Y_A)], goal(X,Y,Value), Direction_Y, VertMove) :-
    Direction_Y < 0,
    Y_A1 is Y_A - 1,
    VertMove =  [move(X,Y_A1)|RestOfVertMove],
    geberate_plan([at(X,Y_A1)], goal(X,Y,Value), Direction_Y, RestOfVertMove).

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
% [PART 4]
% Two predicates, update_beliefs and update_intentions that will compute the
% lists of beliefs and intentions resulting from the agent's observations.
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% the case that the Action is move. In this case, the updated Beliefs1 is the new Beliefs1 after this move.
update_beliefs(at(X,Y), Beliefs, [at(X,Y)]) :-!.

% the case that the Action is pickup. In this case, the updated Beliefs1 is the same as Beliefs.
update_beliefs(picked(X,Y), Beliefs, Beliefs).
update_beliefs(sold(X,Y), Beliefs, Beliefs).



% the case that the Action is move. In this case, the updated Intentions is the same as new Intentions.
update_intentions(at(X,Y), Intentions, Intentions) :-!.

% the case that the Action is pickup, instead of move. In this case, the updated Intentions is the rest of new Intentions.
update_intentions(picked(X,Y), [FirstIntention | RestOfIntentions], RestOfIntentions).
update_intentions(sold(X,Y), [FirstIntention | RestOfIntentions], RestOfIntentions).