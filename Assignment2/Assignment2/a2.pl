
 %----------------------------------------------------------------------
 % Exercise 2.: List programming in Prolog
 %----------------------------------------------------------------------
 %
 % do not delete the following line :
 :- use_module(library(clpfd)) . % this embeds library for CLP
 %
 % Requirements and hints:
 % 1. For each exercise, pure Prolog is sufficient,
 %    i.e., the Cut ("!") , negation as failure (\+), "write",
 %    "readln", "is", and "nl" are NOT necessary and shall NOT be used!
 %
 %    Instead of \+ A=B , use A \= B or A #\= B .
 %
 % 2. Furthermore, built-in procedures or operators that have not yet been presented in the lecture
 %    (like append/3, select/3, ...->...;..., etc.) shall NOT be used!
 %    Exception: member/2 shall be used in subtask 2.11.
 %
 % 3. Use the programming schema for recursive procedures for list programming
 %    as presented in the videos (2.6.x).
 %
 %    That is, use one clause for stopping the recursive calls,
 %         p( ... )
 %    and the other clause for recursive reduction of the processed list
 %         p( ... [X|Xs] ... ) --> p( ... Xs ... )
 %
 %    Subtasks 2.8, 2.10, 2.11, 2.12, 2.14, and 2.15 need 3 clauses
 %
 % 4. Always check: do you need to program a Choice Point?
 %
 % 5. If you get too many or too few solutions, use the hints given in the
 %    video 2.7. and repeat the quizzes 2.7.x on declarative debugging
 %
 % 6. Simplify your program, i.e. avoid singleton variables and
 %    avoid calling unnecessary goals (video 1.10)
 %
 %----------------------------------------------------------------------



 % The programming tasks:
 %
 %
 % 2.1. The procedure secondLastE( +L, -E ) shall return the second last element E of list L:
 %
 %      ?- secondLastE( [1,2,3,4,5], X ).       -->    X = 4 .
 
 		secondLastE( [X,_], X)  .
		secondLastE([ _ , Y|Z], X) :- secondLastE( [Y|Z] , X ) .

		% In the recursive termination I check the last two elements and return the X which is the head of the list and I use _ because I dont care about the last element.
		% The recursive reduction I remove one by one all the elements until I have two.

 % 2.2. The procdure  follows( +L, -X, -Y )  shall return all pairs X and Y of
 %      elements of the list L, where Y is the direct successor of X:
 %
 %      ?- follows([1,2,3],X,Y).       -->  X=1, Y=2  ;  X=2, Y=3
 %      ?- follows([2,1,2,2,3],2,Y).   -->  Y=1  ;  Y=2  ;  Y=3

		follows( [ X | [Y|_] ], X, Y).
		follows( [ _|T ] , X ,Y) :-  follows( T, X, Y).

		% For the recursive termination I check that there are more than 3 elements in the list and return the first element (head) and the secondLast element of the tail list.
		% The recursive reduction basically consist of removing the first element (head) each iteration.


 % 2.3. The procedure  myprefix( +List , -Prefix )  shall return all
 %      prefixes  Prefix  of an input list  List:
 %
 %      ?- myprefix( [1,2,3,4] , [1,2] ) .  --> yes
 %      ?- myprefix( [1,2,3] , X ) .        --> X=[] ; X=[1] ; X=[1,2] ; X=[1,2,3]
 %      ?- myprefix( L , [1] ) .            --> L = [1|_Tail]
 %

		myprefix( _, [] ).
		myprefix( [X|T1], [X|T2] ):- myprefix(T1, T2).

		% The recursion termination is to determinate that the minimum prefix is [].
		% In the second recursive line we want to get the first elements until we finish the recursion.


 % 2.4. The procedure  suffix( +List, -Suffix )  shall return all
 %      suffixes  Suffix  of a given list  List:
 %
 %      ?- suffix( [1,2,3,4] , [3,4] ) .    --> yes
 %      ?- suffix( [1,2] , X ) .            --> X=[1,2] ; X=[2] ; X=[]

		suffix( X, X) .
		suffix( [_|T], X ):- suffix(T, X) .

		% In this exercise, the recursion termination determinates that the list that we input is the same as the one we output.
		% The second step is to reduce the list and get the tail ignoring the head until it is empty.


 % 2.5. Write a procedure   mydelete( +X, +HasXs, -OneLessXs )  that returns
 %      for a given element X and a given list HasXs
 %      a list OneLessXs, that contains one X less.
 %      ?- mydelete( 2, [1,2,3,4], L ) .    --> L = [1,3,4]
 %      ?- mydelete( 2, [1,2,3,2], L ) .    --> L = [1,3,2] ; L = [1,2,3]

 		mydelete(X, [X|L], L) .
		mydelete(X, [Y|T], [Y|L] ):- mydelete(X,T,L) .

		% In this case, the recursion termination is when X is the first element and we remove it from our list.
		% The second step is the recursive reduction in which we delete the head of the list.



 % 2.6. Write a procedure   myinsert( +X, +List, -ListWithX )  that inserts
 %      a given element X at all possible positions into a given list
 %      Liste (i.e. before the first element, after the first, ..., last
 %      element) and that returns the thereby generated list
 %      as the 3rd parameter ListWithX.
 %      ?- myinsert( 1, [2,3], L ) .  --> L = [1,2,3] ; L = [2,1,3] ; L= [2,3,1]
 %      ?- myinsert( 4, [], L ) .     --> L = [4]
		
		myinsert( X , T, [X|T]).
		myinsert( X, [Y|T] , [Y|L]):- myinsert( X , T , L).

		% In this example, the recursion termination ends when we insert the value X in the head of the list.
		% The recursive reduction  is mostly the same as before, deleting the head of the list.
		% This example is similar to the previousltly, it only change the recursion termination because instead of deleting the element, we add it to the head of the list.

 % 2.7. Use either the procedure   mydelete/3  or the procedure   myinsert/3
 %      for implementing a procedure     permut( +Lin, -Lout )
 %      which generates all permutations of a given list Lin exactly once:
 %      ?- permut( [1,2] , P ) .     --> P = [1,2] ; P = [2,1]

		permut( [], [] ).
		permut(L, [X|P] ):- mydelete(X, L, Y), permut(Y, P).

		% The permutation basic case is when we have 2 lists empty because this is the end of the recursion.
		% For this perm recursion we need to use the function from before: mydelete to remove an element and then I insert it but in another position so this way we can get all te possibilities of permutations.


 % 2.8. Write a procedure  mydeleteall( +X, +L, -LwithNoX )  that returns
 %      for a given element X and a given list L a list LwithNoX ,
 %      that contains the same elements as L in the same order except
 %      all elements X of L
 %
 %      Hint: The implementation of the procedure  mydeleteall/3  needs three clauses
 %
 %      ?- mydeleteall( 2, [1,2,3,2,5], L ) . --> L = [1,3,5]
 %      ?- mydeleteall( 2, [2,2], L ) . --> L = []
 %      ?- mydeleteall( 2, [], L ) . --> L = []

		mydeleteall( _ ,[] ,[]) .
		mydeleteall( X , [X|T] , T1):- mydeleteall( X , T , T1) .
		mydeleteall(X, [H|T], [H|Y]) :- X \= H , mydeleteall(X, T, Y) .

		% For this problem I had to use 3 clauses. 
		% The first one checks if both lists are empty because this will be the end of the recursion.
		% In the second clause I delete the head if the input list because 
		% Finally in the last clause I check if X (input name to check) is in the list, in this case I delete it from my list.

 % 2.9. Write a procedure  nonmem( +Element , +List )  that checks that
 % a given element Element is NOT member of a given list List
 %
 %      ?- nonmem( 5 , [1,2,3] ) .   --> true
 %      ?- nonmem( 5 , [1,2,5] ) .   --> false
		
		nonmem( _ ,  []  ) .   
		nonmem( X , [Y|T] ) :- X \= Y, nonmem(X, T) .

		% The recursion termination here is when the list is empty because it is the smallest possibility accepted by the program.
		% The recursive clause basically checks if X and Y are equivalent and if they are not then removes the head from the list.


 % 2.10. Write a procedure  rdups( +LwithDuplicates , -LwithoutDuplicates )   that
 %       takes a list as input list and returns a list without duplicates.
 %       Duplicates are removed from the front, i.e., only the last numbers remain.
 %
 %       ?- rdups([1,2,1], R ) .            --> R = [2, 1]
 %       ?- rdups([1,2,2,1,1,1,3], R ) .    --> R = [2, 1, 3]

		rdups([], []).
		rdups([H|T], N) :- not(nonmem(H, T)), rdups(T, N).
		rdups([H|T], [H|N]) :- nonmem(H, T), rdups(T, N). 
 %
 %       Hints: 1: The implementation of the procedure  rdups/2  needs three clauses
 %              2. Use the procedure  nonmem/2  to implement  rdups/2

		% This example contains 3 clauses.
		% The first clause is when the lists are empty because the program ends its execution.
		% The second cluase checks if head of the list is also repeated in the tail and if it is repeated then I remove it from the list. This way I remove all the repeated elements in the list,
		% In the last clause checks if the head of the list is not in the list and in this cause it doesnt remove like previousltly.

 % 2.11. Write a procedure eachMin( +List , -AMinimumMember ) that returns
 %       each minimum element of a list exactly once:
 %       ?- eachMin( [3,2,3,4] , M ) .       -->  M = 2
 %       ?- eachMin( [2,1,1,4] , M ) .       -->  M = 1 ; M = 1
 %       ?- eachMin( [3,1,2,1,4,1] , M ) .   -->  M = 1 ; M = 1 ; M = 1
 %       ?- eachMin( [] , M ) .              -->  false
 %
 %       Hint: use the builtin procedure member/2 as a generator in order to implement  eachMin/2

		eachMin([X],X) .
		eachMin(L, M) :- member(M, L), not(is_bigger(L, M)) .
		is_bigger(L, M) :- member(X, L), M > X .

		% In this example I also used 3 clauses.
		% The first clause is true when the list onlt has one element and it returns this element itself as there is no other element that can be lower than this one.
		% The second clause checks if the element that we want to return is part of the list because we want to have all the minimum elements and if its repeated then we return the sane number the number of times that it is shown.
		% We also want to check if the element is smallest in the list and for this I check if there is an element that is not bigger than it. So basically I want to remove those elements that are bigger than the element I want.


 % 2.12. Write a procedure filter( +GivenList , +Limit, -SmallerList )
 %       that takes a GivenList and a Limit as input and returns
 %       a list SmallerList containing all elements of the given list
 %       that are smaller than the given Limit:
 %       ?- filter( [2,4,2] , 9 , S ) .     -->  S = [2,4,2]
 %       ?- filter( [3,1,7,3,4] , 4 , S ) . -->  S = [3,1,3]
 %       ?- filter( [2,4,2] , 2 , S ) .     -->  S = []
 %
 %      Hint: The implementation of the procedure  filter/2  needs three clauses

		filter([],_,[]).  
		filter([H|T], L, [H|T2]) :- H < L, filter( T, L, T2).  
		filter([H|T], L, T2) :- H >= L, filter( T, L, T2).

		% This example has 3 clauses.
		% The recursion termination is when the given list is empty because it returns an empty list aswell.
		% For the second clause I check if the head of the list is smaller than to the limit then it removes the head from both lists and returns the tail.
		% In the last clause I am checking if the head is bigger or equal to the limit. If that is the case, then I remove the head only from the first list.

 % 2.13. Write a procedure   lastOccurrence( -X, +HasXs, -HasXsWithoutLastOccurrenceOfX )  that returns
 %       for a given element X and a given list HasXs the list  HasXsWithoutLastOccurrenceOfX
 %       without the last occurrence of X.
 %       ?- lastOccurrence(1,[1,2,1,3,1,2,4],L).  -->  L = [1,2,1,3,2,4]
 %       ?- lastOccurrence(E,[6,5,6,5],L).        -->  E=6 , L = [6,5,5]   ;   E=5 , L = [6,5,6]
 %       ?- lastOccurrence(1,[6,5,6,5],L).        -->  false.
 %
 %       Hint:
 %       You can take your implementation of mydelete/3 as a template,
 %       but use a goal  nonmem/2 to modify the recursion termination in such a way
 %       that it only succeeds if the last occurrence of an element is selected

		lastOccurrence(X, [X|L], L) :- nonmem(X, L) .
		lastOccurrence(X, [H|T], [H|L] ):- lastOccurrence(X,T,L) .

		% For this exemple I use the same as mydelete but in the first clause I added nonmem to check if the element is not repeated because I only want to remove those elements that are duplicated once.
		% In the second rule I only delete like previousltly so I remove the head of my list.

 % 2.14. Write a procedure   listIntersection( +L1, +L2, -CommonMembers )  that takes two input lists L1 and L2
 %       and returns a list CommonMembers containing all the common members as often as they occur at most
 %       in each of the lists L1 and L2.
 %       The common members shall occur in the same order as they occur in L1.
 %       ?- listIntersection([1,2,5,3,1],[2,1,1,2,5],Cs).      -->  Cs = [1, 2, 5, 1]
 %       ?- listIntersection([2,1,2,3,1,2,1,1],[3,1,2,1,2,5],Cs).  -->  Cs = [2, 1, 2, 3, 1]
 %
 %       Hint:
 %       In the two rules for the recursive problem reduction, stepwise reduce the length of L1,
 %       and in one of these rules, use mydeletelast/3 to delete a common element only once from L2

		listIntersection([], _ , [] ) .
		listIntersection([H1|T1], L, [H1|T2]) :- not(nonmem(H1, L)), lastOccurrence(H1, L, E), listIntersection(T1, E, T2) .
		listIntersection([H1|T1], L, L2) :- nonmem(H1, L), listIntersection(T1, L, L2) .

		% The recursion termination checks if the first list is empty and it does not match the second value then it returns an empty list.
		% In the second rule I want to check if the head of the first list is member of the list (it is duplicated) then it deletes the last occurrence with the function lastOccurrence created previousltly.
		% and then I remove the head of the list and the new second list is the generated using last occurance and returns the tail.
		% The last clause checks if it is not a member so it is not duplicated then it only removes the head of the list.

 % 2.15. Write a procedure   countMatches( +L1 , +L2 , -NumberOfPositionsWithIdenticalValues )
 %       that returns the number of positions where both lists have identical values.
 %       If both lists have different lengths, countMatches/3 shall return false.
 %       ?- countMatches( [red,blue,white,green,red] , [red,blue,green,white,red] , N ) . --> N=3
 %       ?- countMatches( [red,red,red,red] , [blue,red,black,green] , N ) . --> N=1
 %       ?- countMatches( [white,red] , [red,white] , N ) . --> N=0
 %       ?- countMatches( [red,blue,green,red] , [red,blue,green] , N ) . --> false

		countMatches([], [], 0) .
		countMatches([H1|T1], [H2|T2], N) :- H1=H2, countMatches(T1, T2, M), N #= M+1 .
		countMatches([H1|T1], [H2|T2], N):- H1\=H2, countMatches(T1, T2, N) .

		% The recursion termination checks if the inputs lists are empty and the output is a 0 because if there is no elements in both list then N = 0.
		% The second rule is a recursive reduction where I check if the head from both lists are the same then it increments the counter.
		% Finally if the heads are not equal I keep the recursion without incrementing the counter and removing the head of the list.
		% It continues until I have no elements in both lists.
		% If I have more elements in one list than in the other the program fails.


		





