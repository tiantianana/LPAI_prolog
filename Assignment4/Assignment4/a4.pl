
 :- use_module(library(clpfd)).

 %
 % --------------------------------------------------------------------
 % Assignment 4
 % --------------------------------------------------------------------
 %


 % acyclic depth first search with parameter M for the Move
 % adfsmr(+Move,+Source, -Destination, +PathSoFar, -Path)
 adfsmr( _, S, S, P, P ).
 adfsmr( M, S, D, Ptmp, P ) :-
    Move =.. [ M, S, C ] ,            % construct single move to C
    Move ,                            % try the move
    \+ member( C, Ptmp ) ,            % C not yet visited - no cycle
    adfsmr( M, C, D, [C|Ptmp], P ) .  % continue to search from C

 % acyclic depth first search with parameter for Move returning Path
 % adfsm(+Move,+Source, -Destination, +PathSoFar, -Path) -
 adfsmp( M, S, D, P ) :-
    adfsmr( M, S, D, [S], RP ) ,
    reverse( RP, P ) .



 % try: ?- idmr(ic,pb,Destination,Path) . --> [pb], [ks,pb], ...

 % iterative deepening search + Path + variable Move
 % idmr(+Move, +Source, -Destination, -Path_Reverse) -
     idmr( _, S, S , [S] ) .
     idmr( M, S, D ,[D|P]  ) :-
         idmr( M, S, C, P ) ,
         Move =.. [ M, C, D ] ,    % construct the call of move
         Move .                    % call the move

/*
 % try: ?- idmp(ic,pb,Destination,Path) . --> pb, ks, pb, f, ks, pb, f, ...

 % idmp(+Move, +Source, -Destination, -Path) - iterative deepening search + Path + Move
 idmp( M, S, D, P ) :-
    idmr(M,S,D,P_Rev) ,   % use M as move for iterative deepening
    reverse(P_Rev,P).
*/


 % lidm(+Move,+Source, -Destination, -Path) limited iterative deepening
 % search + Path - stops at a depth where 1st solution is found
 % shows all solutions on that depth

 lidm( M, S, D, P ) :-
    length( P1 , N ) ,    % use length of first solution to limit length of other solutions
    idmr(M,S,D,P1) ,      % search first solution by iterative deepening
    ! ,                   % the first solutions exists for length N
    length( RP, N ) ,     % limit reverse solution paths RP_Rev to length N
    idmr(M,S,D,RP) ,      % reverse solution path P may differ from reverse of first path P1 found
       % must not be idm here, because idm does not pass length limit of Path
    reverse(RP,P) .



 % ========================================================================
 % 4.0 The jealous husbands puzzle
 %
 % In the jealous husbands (river-crossing) puzzle three married couples
 % want to cross a river with a boat that can carry at most two persons,
 % with the constraint that no woman shall be in the presence of another
 % man (on the west side or on the east side or on the boat) unless her
 % husband is also present.

 % the jealous husbands (river-crossing) puzzle
 lidm0(P) :-            
    S = s( west , [m(1),m(2),m(3),w(1),w(2),w(3)] , [] ) ,    % all stay west
    D = s( east , [] , [m(1),m(2),m(3),w(1),w(2),w(3)] ) ,    % all stay east
    lidm(move0,S,D,P) .   %  find a shortest path of moves to cross the river

 % move0( +state(Boat,West,East) , -newstate(Boat,West,East) )
 move0( s(west,West,East), s(east,WNew,ENew) ) :- move0FT( West,East,WNew,ENew ) .
 move0( s(east,West,East), s(west,WNew,ENew) ) :- move0FT( East,West,ENew,WNew ) .

 % move0FT( +FromStateOld , +ToStateOld , -FromStateNew , -ToStateNew )

 move0FT( From , To , Fnew , T1 ) :-
    select( P , From , Fnew ) ,          % pick each element of From once, Fnew remain
    safe0( Fnew ) ,                      % From-side is safe in new state
    safe0( [P|To] ) ,                    % To-side is safe in new state
    sort( [P|To] , T1 ) .              % sort needed to have set property of T1

 move0FT( From , To , Fnew2 , T1 ) :-
    select( P , From , From_F ) ,        % select element P from From to get From_F
    select( F2 , From_F, Fnew2 ) ,       % select element F2 from From_F to get Fnew2
    P @< F2 ,                            % select each pair (e.g. m(2)@<w(1)) only once
    safe0( Fnew2 ) ,                     % From-side is safe in new state
    safe0( [P,F2|To] ) ,                 % To-side is safe in new state
    sort( [P,F2|To] , T1 ) .           % sort needed to have set property of T1


 safe0( L ) :- \+ (                      % side L is safe, if the following is NOT true:
                   member( w(X) , L ) ,  % there is a woman w(X) on side L
                   \+ member( m(X),L ),  % here husband m(X) is not on side L
                   member( m(_) , L )    % and a (different) man is on side L
                  ).




 % =============================================================
 % 4.1. The framer (river-crossing) puzzle
 %
 % A farmer has a wolf, a goat, and a cabbage and wants to cross a river
 % from west to east with a boat that can at most carry him and one of
 % his belongings, i.e. the wolf, the goat, or the cabbage.
 % The constraint is that he farmer cannot leave the wolf and the goat
 % alone on a side of the river, as the wolf would eat the goat, and
 % the farmer cannot leave the goat and the cabbage alone, as the goat
 % would eat the cabbage.

 % Use limited iterative deepening search to solve the puzzle!
 % Write a procedure move1(State1,State2) that describes one legal and
 % safe move from a state State1 to a state State2 in this puzzle   AND
 % write a procedure  lidm10(Path)  that describes the legal paths Path
 % of safe moves from the start state, i.e., that all are on the west side
 % to the end state, i.e., that all are on the east side.
 %
 % Model the states as s(P,W,G,C), where
 % P=1 means farmer is west, P=0 means farmer is east
 % W=1 means wolf is west, W=0 means wolf is east
 % G=1 means goat is west, etc.
 % e.g., s(1,1,1,0) means cabbage is east, all others are west
 %
 % Hint:
 % Use the jealous husbands puzzle as a template.
 % Redesign procedures for move and for safety check


   lidm10(P) :-            
      S = s(1,1,1,1) ,    % all are in west
      D = s(0,0,0,0) ,    % all stay east
      lidm(move1,S,D,P) .   %  find a shortest path of moves to cross the river

   move1( s(P,W,G,C),s(F1,W1,G1,C1) ) :- move1FT( [P,W,G,C],[F1,W1,G1,C1] ).

   move1FT( [P,W,G,C], [F1,W,G,C] ) :-
      F1 #= (P+1) mod 2 ,     
      safe1( F1,W,G,C ) .

   move1FT( [P,W,G,C], [F1,W1,G1,C1] ) :-
      F1 #= (P+1) mod 2 ,     
      safe1( F1,W1,G1,C1 ) ,
      select(P, [W,G,C], F1, [W1,G1,C1]) .

   safe1(X,_,X,_).
   safe1(X,X,G,X) :- X #\= G .

   % For this exercise I used videos 4.5-4.7 limited iterative deeping, dfs and the exercise 4.0 husband puzzle as a template.
   % The first rule lidm10 are the first state when all animals and farmer and in west and the goal node is when they all reach east.
   % The move1 rule calls move1FT which are the rules that makes all the posible moves that are allowed and safe for all the animals to not kill respectively.
   % The rule safe is to check that that the wolf and the goat are never alone and the cabbage and the goat are not.
   % I use the select to check all the posibilies that also they are safe.

 % =============================================================
 % 4.2. The Crossing the Bridge at Night - puzzle
 %
 % The puzzle is the following:
 % Sally, Peter, their Grandma, and their Grandpa have to cross an old
 % long bridge across a deep valley from west to east during a dark night.
 % It is dangerous to cross the bridge because the bridge has holes,
 % i.e., you definitely need to have a flashlight with you to see the
 % holes, while crossing the bridge.
 % Furthermore, the old bridge can carry at most two of them at a time
 % and all 4 persons have the same weight.
 % If two persons go over the bridge at the same time, they must stay
 % together as the whole group has only one flashlight, i.e., for
 % crossing the bridge, they need the time that the slower of both would
 % need alone.
 % Now, Sally is fast and can cross the bridge in 5 minutes,
 % Peter is rather fast and can cross the bridge in 10 minutes.
 % However, Gradma is slow and needs 20 minutes to cross the bridge,
 % and Grandpa is even slower and needs 25 minutes.
 % Furthermore, if e.g. the first two of them have crossed the bridge,
 % somebody must bring back the flashlight to the other side, such that
 % the others can also cross the bridge.
 % How can they organize the crossing over the bridge,
 % such that the MCurrent time is not more than 60 minutes?

 % Use acyclic depth-first search to solve the puzzle!
 % Write a procedure move2(State1,State2) that describes one legal and
 % safe move from a state State1 to a state State2 in this puzzle    AND
 % write a procedure adfsmp20(Path) that describes the legal paths Path
 % of safe moves from the start state, i.e. all are on the west side
 % to the end state, i.e., all are on the east side.
 %
 % Model the states as
 % s(Who_does_stay_west, Who_does_stay_east,Where_is_the_flashlight,Time_elapsed).
 % Hereby, Who_does_stay_west and Who_does_stay_east are lists of those family members
 % that stay west or east respectively,
 % and in these lists, each family member is represented
 % by the time that he or she needs to cross the bridge.
 % Furthermore, the values for the variable Where_is_the_flashlight are west or east, and
 % Time_elapsed is the time (in minutes) elapsed so far.
 %
 % For example, if the first move is that Peter and Grandma cross the
 % bridge together, the state reached is s([5,25],[10,20],east,20)
 %
 % Finally, there is no difference whether e.g. Peter and Grandma are on the
 % east side or Grandma and Peter. To avoid that both, [10,20] and [20,10],
 % are considered as two different legal lists of persons on a side of
 % the bridge, you shall represent this list only as [10,20].
 % More general, each list of times representing persons on the same side
 % of the bridge shall contain the times in ascending order!
 %
 % Hints:
 % 1. To simplify the implementation, you can assume that
 %    in each move from west to east, 2 persons go together, and in each
 %    move from east to west, 1 person alone carries the flashlight.
 % 2. To avoid duplicate solutions by considering both [10,20] and [20,10],
 %    use the built-in procedure sort/2 and use the operator #=<.

   
      adfsmp20(P) :-
    		S = s( [5,10,20,25] , [] , west , 0 ) ,    
    		D = s( [] , [5,10,20,25] , east , _ ) ,
    		lidm(move2,S,D,P) .

      move2( s(West,East,west,T0), s(WNew,ENew,east,T1) ) :- 
    		select(P,West,Lst),
    		select(P2,Lst,WNew),
			P@<P2,
    		safe2(T0,T1,P2),
    		sort([P,P2|East],ENew).

 		move2( s(West,East,east,T0), s(WNew,ENew,west,T1) ) :- 
			select(P,East,ENew),
			safe2(T0,T1,P),
			append(West,[P],Lst),
			sort(Lst,WNew).

      safe2(T0,T1,P) :- T1 #= T0 + P, T1 #=< 60.

      % This exercise is similar to 4.0 and I use the same videos as the previous one 4.5-4.7.
      % The start node is when all members of the family are in the left side of the bridge.
      % The final state is when all members cross the river and gets the time that spent crossing the river as the last parameter.
      % The move2/2 consist of 2 rules, one that moves 2 person from west to east, so it uses 2 selects to pick the people with the safe condition
      % and then sorts the list.
      % The second move2 is for the person to return the flashlight back from east to west.
      % In this case I only use one select to pick the person who takes less time to cross the river and safe again to make sure that the time is lower to 60 and accumulate the result.
      % Finally I append the person that came back to the list of people that are in West and sort the list.

 % =============================================================
 %
 % 4.3. The Missionaries and Cannibals-Puzzle
 %
 % The puzzle is the following:
 % 3 missionaries and 3 cannibals have to cross a river from west to east
 % with a single boat that can only carry two persons, i.e., the boat
 % carries one or two persons when crossing the river.
 % However, if the cannibals are in the majority on either side of the
 % river, they eat the missionaries on this same side of the river.
 % That is, missionaries on any side of the river are only safe, if the
 % cannibals never outnumber the missionaries on that side of the river.
 % Which moves (using the boat) do the 6 persons have to do, such that no
 % missionary comes into danger to be eaten?

 % Use limited iterative deepening search to solve the puzzle!
 % Write a procedure move3(State1,State2) that describes one legal and
 % safe move from a state State1 to a state State2 in this puzzle    AND
 % write a procedure  lidm30(Path)  that describes one legal path Path
 % of safe moves from the start state, i.e., that all are on the west side
 % to the end state, i.e., that all are on the east side.
 %
 % Model the states as s(Boat,[M_east,C_east]), where
 % Boat=1 means the boat is west, Boat=-1 means the boat is east
 % M_east denotes the number missionaries on the east side
 % C_east denotes the number cannibals on the east side
 % e.g., s(1,[2,2]) means that the boat is on the west side with
 % one missionary and one cannibal, and all others are east.
 %
 % Write a procedure move3(State1,State2) that describes one legal and
 % safe move from a state State1 to a state State2 in this puzzle   AND
 % write a procedure  lidm3(Path)  that describes one legal path Path
 % of safe moves from the start state, i.e., that all are on the west side
 % to the end state, i.e., that all are on the east side.
 %
 % Hint:
 % The solution is similar to the puzzle of farmer, wolf, goat, and cabbage
 % except for the description of states and moves.

   lidm30(P) :-            
      S = s(1,[0,0]) ,    % all missionaries and cannivals stay in west
      D = s(-1,[3,3]) ,    % all missionaries and cannibals stay east
      lidm(move3,S,D,P) .   %  find a shortest path of moves to cross the river

      move3( s(B,[M,C]) , s(B2,[M2,C2]) ) :-
         B2 #= -B ,                    % change side of the river
         safe3(M,C,M2,C2,B),
         [M2,C2] ins 0..3 ,      
         label([M2,C2]).

      safe3(M,C,M2,C2,B) :-
         M2*(M2-C2)*(M2-3) #= 0 ,      % safe: enough missioners on both sides if M2 = 0, M2 = C2 or M2 = 3
         abs(M-M2)+abs(C-C2) #=< 2 ,   % safe: Max 2 persons per boat
         abs(M-M2)+abs(C-C2) #>= 1 ,   % safe: Min 1 person per boat
         (M-M2) * B #=< 0 ,
         (C-C2) * B #=< 0 .

   % This exercise is a little bit different to the previous one.
   % I am using the same videos as before, however I also had to check the videos from chapter 2.
   % My start node is when all the missionaries are in west and my goal node are when all missionaries are in east.
   % My move3/2 rule checks the boat goes from one side to another side (as B would be the opposite).
   % The safe rule is when the missionaries are enough on both sides, and the boat needs to move at least 1 person and max 2 persons.
   % and when the direction of B multiply by the difference of M-M2 and C-C2 and is equal to 0.
   % Finally I use ins to determinate that the list has values between 0 and 3 and label to get a explicit solution so it sets the letters a corresponding number.

 % =====================================================
 % 4.4. The mug puzzle:
 %
 % The puzzle is the following:
 % You have one 8 liter mug, one 5 liter mug, and one 3 liter mug.
 % At the beginning, the 8 liter mug is full, the other 2 mugs are empty.
 % You can fill the water contained in one mug into another mug,
 % subject to the constraint that there is no scale given on the mugs.
 % Therefore, in each move, you can completely fill a mug or completely
 % empty a mug only.
 % The goal is to get a filling of exactly 4 liter in the 8 liter
 % mug and 4 liter in the 5 liter mug.

 % Model the states as s(Mug8,Mug5,Mug3), where
 % Mug8 means the filling of the 8 liter mug
 % Mug5 means the filling of the 5 liter mug
 % Mug3 means the filling of the 3 liter mug
 % e.g., s(5,0,3) means that the 8 liter mug contains 5 liter,
 % the 5 liter mug is empty, and the 3 liter mug if full.
 % This state can be reached from the initial state by using the 8 liter
 % mug to fill the 3 liter mug.
 %
 % Use limited iterative deepening search to solve the puzzle!
 % Write a procedure move4(State1,State2) that describes one legal and
 % safe move from a state State1 to a state State2 in this puzzle   AND
 % write a procedure  lidm40(Path)  that describes one legal path Path
 % of safe moves from the start state to the end state.
 %
 % Hint:
 % The solution is similar to the puzzle of farmer, wolf, goat, and
 % cabbage except for the description of states and moves.

   lidm40(P) :-
      S = s(8,0,0) ,
      D = s(4,4,0) ,
      lidm(move4,S,D,P) .
   
   move4(s(M8, M5,M3), s(M8New, M5New, M3New)) :-
      select([MCapacity,MCurrent], [[8,M8], [5,M5], [3,M3]], [MCapacity,Temp1], L), 
      select([ACapacity,ACurrent], L, [ACapacity,Temp2], [[_,M8New], [_,M5New], [_,M3New]]),
      Temp1 #= MCurrent - Filled,
      Temp2 #= ACurrent + Filled,
      MCapacity #\= ACapacity,
      safe4(MCurrent,ACapacity,ACurrent,Filled).

   safe4(MCurrent,ACapacity,ACurrent,Filled) :- 
      MCurrent #< ACapacity - ACurrent,
      Filled #> 0,
      Filled #= MCurrent .

   safe4(MCurrent,ACapacity,ACurrent,Filled) :- 
      MCurrent #>= ACapacity - ACurrent,
      Filled #> 0,
      Filled #= ACapacity - ACurrent .

   % In this exercise I use the videos 4.5-4.7 to understand how the lidm works.
   % In the first rule, the move4 recieves a start node that has 8L in the first mug and 0 in the rest.
   % The goal is to have 4 litters in the biggest mug and 4 litters in the middle mug.
   % I use the select to check all the possibles that I have to fill the mugs and all the combinations and store them in L.
   % Then I need to substitute this MCapacity and MCurrent for the new Current which is temp1 and is calculated by MCurrent - Filled.
   % I have to do another select that from the previous list obtained by the previous L I need to substitute ACurrent for Temp2, which is the summary 
   % of te ACurrent + Filled.
   % Finally I check if it is safe or not. The safe4/4 rule is to make sure that the mug current is smaller than the difference of the actual capacity and the actual current.
   % This is to make sure I dont take the possibilities that are not allowed.
   % The second safe checks if the current mug is to make sure that it doesnt over fill, so it stops when the mug is full.

 % =======================================================================
 % 4.5. The shift puzzle
 %
 % The puzzle is the following:
 % Given is a distribution of pieces 1,...,11 in a 3x4 grid with one
 % empty space. For this subtask, use the following puzzle:
 %    3  2
 %    4  1  5
 %    6  7  8
 %    9 10 11
 % The goal is to find a sequence of moves such that the
 % grid with the 11 pieces looks like
 %       1  2
 %    3  4  5
 %    6  7  8
 %    9 10 11
 % Here, the top-left corner is empty.
 % In each move, you can only move an adjacent piece to the empty place.
 % And you can move the pieces left,right,up, or down, but no piece may
 % be moved out of the 3x4 grid.

 % Model each state as List of twelve numbers, e.g. [3,2,0,...],
 % starting with the numbers of the pieces located in the top row.
 % Hereby, 0 represents the empty place.
 %
 % Use limited iterative deepening search to solve the puzzle!
 % Write a procedure move5(State1,State2) that describes one legal and
 % safe move from a state State1 to a state State2 in this puzzle   AND
 % write a procedure  lidm50(Path)  that describes one legal path Path
 % of moves from the start state given above to the final state given
 % above.
 %
 % Attention!
 % Not all pairs of start states and destination states can be
 % transformed into each other, i.e., if such a combination of
 % start state and destination state is given,
 % iterative deepening will search forever.
 % Furthermore, the runtime grows exponentially, depending on the
 % length of the path from start state to end state.
 % This is one of the reasons why adfsmp in general does NOT work here.
 %
 % Hints:
 % 1. The shift puzzle moves the empty piece (the 0) to a neighbour
 %    destination, i.e. swaps it with left, right, above or below
 %    neighbour.
 %
 % 2. Look at program p4.2_swap.pl to see how to swap two elements in a
 %    list that fulfil a particular constraint.
 %

   lidm50(P) :-  
         S = [3,2,0,4,1,5,6,7,8,9,10,11],
         D = [0,1,2,3,4,5,6,7,8,9,10,11],
         lidm(move5,S,D,P) .

   % move right
   move5(L,Lout) :-
      append( L1, [0,Swap|L2] , L ) , 
      safe5(L1,I),
      append( L1, [Swap,0|L2] , Lout ) .

   % move left
   move5(L,Lout) :-
      append( L1, [Swap,0|L2] , L ) , 
      safe5(L1,I),
      append( L1, [0,Swap|L2] , Lout ) .
   
   % move up
   move5(L,Lout) :-
      append( L1, [Swap,Stay1,Stay2,0|L2] , L ) ,
      append( L1, [0,Stay1,Stay2,Swap|L2] , Lout ) .

   % move down
   move5(L,Lout) :-
      append( L1, [0,Stay1,Stay2,Swap|L2] , L ) ,
      append( L1, [Swap,Stay1,Stay2,0|L2] , Lout ) .

   safe5(L1,I) :- length( L1, I ) , I mod 3 #\= 2 .

   % For this exercise I check the videos 4.1, 4.2 and the swap rule from p4.2_swap.pl.
   % My start node is the puzzle unsolved and with a specific set up and my goal node is the puzzle solved in a list format.
   % I use the rule move/5 4 times, one for each move.
   % So for moving left right I swap the 0 with the one that is in the right or left respectively but i need to check that if its Imod3, 
   % which means it is in the edge of the board then this rule cant happen with the rule safe5.
   % For moving up and down the append is similar but i need to keep the 2 next pieces in the same place. In this case I dont have any restrinction.

 % =======================================================================
 % 4.6. The generalized nim game
 %
 %      Nim is a two-player strategy game with the following rules.
 %      There is one box with S tokens, e.g. S=16 or S=23 or S= ... .
 %      Two players (called Max and Min) alternatingly have to take tokens from this box.
 %      Within each move, the player in turn has to take either 4 or 6 or 9 tokens.
 %
 %      The player who is in turn and cannot take anything in a move anymore
 %      because less than 4 tokens are left over, looses N+1 points if N tokens are left over.
 %
 %      Write a program that computes the following for a given number of tokens in the box.
 %      Starting with the given number of tokens in the box, your program shall simulate 2 strong players,
 %      i.e., each player does one of the best possible moves whenever it is his turn.
 %      Your program shall show the list P of intermediate states, i.e.,
 %      how many tokens are in the box after each move.
 %      And your program shall show the number V of points won by the player who does the first move
 %
 %      For example,
 %            ?- eval(25,P,V).  -->   P = [25, 16, 10, 1] , V=2
 %      This answer means:
 %      For the first player starting with 25 tokens in the box, one best move is to take 9 tokens.
 %      Now, for the opponent, starting with 16 tokens in the box, one best move is to take 6 tokens.
 %      Now, for the first player starting with 10 tokens in the box, a best move is to take 9 tokens.
 %      This leaves 1 token in the box.
 %      Now, the opponent cannot move anymore and looses 1+1=2 points and the player wins V=2 points.
 %
 %      Your program shall contain a procedure move6/2 that computes the destination states
 %      for given start states and can e.g. be called like this
 %            ?- move6( 25 , S2 ) .   -->    S2 = 16
 %            ?- move6( 16 , S3 ) .   -->    S3 = 10
 %
 %      Hints:
 %      1. You can copy and reuse the procedure eval from the program tic-tac-toe.pl ,
 %         except that eval/3 should call move6/2 instead of move/2.
 %      2. You need to adapt the implementations of the procedures move6/2 and evalfinal/2 in such a way
 %         that they support the nim game.
 %      3. Your procedures move6/2 and evalfinal/2 could be significantly shorter
 %         than the corresponding procedures in the program tic-tac-toe.pl

 % show_best_move(+State,-NextState) - show the next state that is reached by doing one of the best moves
 showBestMove(S) :- showBoard(S) , eval(S,[_,S2|_],_)  , showBoard(S2).

 % go( +State_to_start_game , -Value for the player who starts in this game (1=win,0=tie,-1=loose)
   go(S,V) :-                 % start game with a state S (player to move depends on state S)
      eval( S , P , V ) ,     % for state S, compute path P of states reached by optimal moves of both players
      maplist(showBoard,P) .  % print each board on such a path


 % The procedure eval checks for a given state S whether it is a final state.
 % If S is a final state, eval returns its value V (not for Max, but) for the player that would have to move
 % and a path P containing only this state

   eval( S , [S] , V ) :-   % path P contains final state S only. V is its value for the player who would move
      evalfinal(S,V) ,      % if you are in a final position, return value V for player who would move
      ! .                   % do not do further moves after a final state has been reached

 % If S is not a final state,
 % eval( +S , -P , -V ) returns one path P of states reachable from S by optimal moves of both players
 % and the value V that Max gets when the moves of both players follow this path P of states

   eval( S , P , V ) :-   % return path P of states reachable by optimal moves of each player
                          % V is the value for player at move if both players follow this path P

                          % for all possible moves from state S to state N, let the other player evaluate N
   findall( p(V2,[S|P2]), ( move6(S,N), eval(N,P2,V2) ) , V2P ) ,   % collect list V2P of pairs p(Value,Path)

   min( V2P,p(V1,P) ) ,  % find the move (i.e. the path P) with minimal value V1 for the other player

   V is -1 * V1 .        % -1 * the minimal value V1 of the other player is the current player's best value V

   %  for a list V2P=[p(V0,P0),...,p(Vn,Pn)] of pairs p(Value,Path),
   %  min( +V2P , -p(V1,P) ) -  finds the move (i.e. the path P) with minimal value V1 for the other player.

   min( [p(V1,P1)] , p(V1,P1) ) .
   min( [p(V1,P1)|T] , p(Vm,Pm) ) :-
      min( T, p(V2,P2) ) ,                            % select minimum value and path of tail
      min( p(V1,P1) , p(V2,P2) , p(Vm,Pm) ) .         % select minimum value and path of both, head and min of tail

   % min( +p(V1,P1) , +p(V2,P2) , -p(Vm,Pm) )         % select minimum value and path of both, head and min of tail
   min( p(V1,P1) , p(V2,_ ) , p(V1,P1) ) :- V1 < V2 . % select path of smaller value
   min( p(V1,_ ) , p(V2,P2) , p(V2,P2) ) :- V2 < V1 .
   min( p(V,P1)  , p(V,P2)  , p(V,Pm)  ) :-           % if values V1,V2 are equal,
      length(P1,L1) , length(P2,L2) ,
      shortORlong(V,L1,L2,P1,P2,Pm) .                 % as a winner select short path, as a looser select long path

   % shortORlong(+V,+L1,+L2,+P1,+P2,-Pm)   as a winner select short path, as a looser select long path
   shortORlong(V,L1,L2,_,P2,P2) :-                    % as a winner select short path, as a looser select long path
      (L1-L2)*V #< 0.                % if value V for other player is -1, we win and select the shorter path P2
   shortORlong(V,L1,L2,P1,_,P1) :-                    % as a winner select short path, as a looser select long path
      (L1-L2)*V #>= 0.         % if value V for other player is -1, we win and select path P1 which is not longer


   % bestMove(+State,-NextState) - show the next state that is reached by doing one of the best possible moves
   bestMove(S,S2) :- eval(S,[_,S2|_],_) .

   evalfinal(Tokens, Pts) :-
         Tokens #< 4,
         Pts #= (-1) * (Tokens + 1).

   move6( Tokens, S ) :-
         (S #= Tokens - 4; S #= Tokens - 6; S #= Tokens - 9),
         S #>= 0.

% For this exercise I use p4.7_tic_tac_toe.pl for the part of the eval is completely identical.
% However evalfinal and move6 are a little bit different.
% For evalfinal I check if the tokens are smaller than 4 because in this case the player loses as the other player cant remove 4,6 or 9 tokens.
% The points are the tokens left +1 and multiplied by -1.
% The rule move6 calculates all the posible moves, so I have to return S as the solution and rest 4, 6 and 9 to the tokens.
% All solutions must be positive so I have to check if S is bigger or equal to 0.
