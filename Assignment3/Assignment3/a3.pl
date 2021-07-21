 % Assignment 3: Permutation-based puzzles


 % Do not delete the following line:
 :- use_module(library(clpfd)) .



 % Try to code as short as possible, i.e.
 % avoid unnecessary rules, unnecessary goals, and singleton variables.
 %
 % Furthermore, AVOID using "\+" (negation as failure), "!" (cut), "not", "is",
 % and avoid using  ">",  "=", "==",  ">=",  "=<",  "<", "\=" , \== .
 % Instead use     "#>", "#=",       "#>=", "#=<", "#<", "#\="     .
 %
 %
 % 3.1. Solve the following puzzle (star puzzle) using a Prolog programm
 %
 %      The puzzle to solve is:
 %      distribute the numbers 1 to 12 to the places marked by O below
 %      in such a way that the sum of each line is 26 and that the
 %      sum of the 6 outer corners is 26 too :
 %
 %                        O
 %                       / \
 %                  O---O---O---O
 %                   \ /     \ /
 %                    O       O
 %                   / \     / \
 %                  O---O---O---O
 %                       \ /
 %                        O
 %
 %      Write a procedure q31 that can be called by
 %
 %      ?- q31(L1,L2,L3,L4,L5).
 %
 %      and that returns 5 lists of numbers, one for each row.
 %
 %      One solution (among many others) should be
 %      L1 =        [ 1 ],
 %      L2 = [ 2 , 12 , 9 , 3 ],
 %      L3 =   [ 6    ,  11 ],
 %      L4 = [ 7 , 10 , 4,  5 ],
 %      L5 =        [ 8 ]
 %
 %      (Pretty printing is not needed.)
 %
 %      Use permutation/2 to get a short solution
 %
 %      Hints:
 %      1. You can modify the program magic square
 %      2. Checking your solution with the tester may need significantly longer than executing your program locally


         q31([A],[B,C,D,E],[F,G],[H,I,J,K],[L]) :- 
         26 #= A+C+F+H, 
         26 #= A+D+G+K, 
         26 #= H+I+J+K, 
         26 #= B+C+D+E, 
         26 #= B+F+I+L, 
         26 #= E+G+J+L,
         26 #= A+B+H+L+K+E, 
         permutation([1,2,3,4,5,6,7,8,9,10,11,12],[A,B,C,D,E,F,G,H,I,J,K,L]).

         % I am using video 2.1 to solve the neighbours so that all line sums 26 and finally the outers whcih are A, B, H, L, K and E also need to sum 26.
         % Then I use permutation/2 which is a predefined list permutation from prolog to generate permutations.


 % 3.2. How can you arrange the numbers 1 to 9 (1, 2, 3, 4, 5, 6, 7, 8, and 9)
 %      in a single fraction that equals exactly 1/3 (one third)?
 %      An example that does NOT work is: 7192/38456 = 0,187
 %
 %      Write a procedure q32 that you can call by
 %
 %      ?- q32(Numerator,Denominator) .
 %
 %      and which returns two lists i.e.
 %          Numerator= [A,B,C,D] ,
 %          Denominator = [E,F,G,H,I]
 %      with the correct values for A,...,I.
 %
 %      The wanted solution may need 5-15 seconds of run-time

         q32([A,B,C,D],[E,F,G,H,I]):- 
                3 *(A*1000 + B*100 + C*10 + D) #= (E*10000 + F*1000 + G*100 + H*10 + I), 
                permutation([1,2,3,4,5,6,7,8,9],[A,B,C,D,E,F,G,H,I]).

         % For this exercise I use video 3.2 that explains how to use the operator '#=' for equivalent of integers.
         % In the rule const I want to check that A, B, C and D is triple of E, F, G, H and I. 
         % Then I need to know the position of each number. As I multiple the Numerator (A, B, C, d) with 3, so I get the Denominator, I need to use the basic rules of the multiplication.
         % That is why I multiple decimals by decimals according to its position.
         % In this case I got 2 solutions Denominator = [1, 7, 4, 6, 9], Numerator = [5, 8, 2, 3] and Denominator = [1, 7, 4, 9, 6], Numerator = [5, 8, 3, 2]


 % 3.3. Number labyrinth
 %
 %      The puzzle to solve is:
 %      Distribute the numbers 1 to 8 to the places marked by O below
 %      in such a way that no two places marked by O that are connected via
 %      a link contain consecutive numbers.
 %
 %           O
 %          /|\
 %         / | \
 %        /  |  \
 %       /   |   \
 %      O----O----O
 %      |\  /|\  /|
 %      | \/ | \/ |
 %      | /\ | /\ |
 %      |/  \|/  \|
 %      O----O----O
 %       \   |   /
 %        \  |  /
 %         \ | /
 %          \|/
 %           O
 %
 %
 %      Write a Prolog procedure q33 that can be called by
 %
 %      ?- q33(L1,L2,L3,L4).
 %
 %      and that returns 4 lists of numbers, one for each row.
 %
 %      Avoid using "\+" (i.e. negation as failure)!
 %      Instead, for checking that N1 and N2 are unequal,
 %      use " N1 #\= N2 " similarly as in program map_coloring
 %
 %      Hint:
 %      The test that two neighbour places are not occupied by
 %      consecutive numbers has to be done very often!
 %      If you use two rules for that check, i.e. implement a choice point,
 %      your program might run slowly, i.e., it may take more than 10 seconds
 %      to find the first solution and more than 5 minutes to find all solutions.
 %      To avoid long run times, try to find an implementation of
 %      the test that two neighbour places are not occupied by consecutive
 %      numbers using a single rule only!


         neighbours( _ ,[]).
         neighbours( L , [H|T] ):- #\ abs(L-H) #= 1 , neighbours(L,T).

         q33([A],[B,C,D],[E,F,G],[H]) :- 
                  neighbours(A,[B,C,D]), 
                  neighbours(B,[C,E,F]), 
                  neighbours(C,[D,E,F,G]),
                  neighbours(D,[F,G]),
                  neighbours(E,[F,H]),
                  neighbours(F,[H,G]),
                  neighbours(G,[H]),
                  permutation([1,2,3,4,5,6,7,8],[A,B,C,D,E,F,G,H]).


         % This exercise is similar to 3.1 but instead of sum of all the lines and outers, I need to check if they are neighbours with the function 'neighbours' that checks if the difference between both numbers is not 1. 
         % If the difference 1 then it returns fails and means that they are neighbours. Otherwise it keeps checking all the list.
         % Finally I permute the numbers 1-8 corresponding to the o to get all the possible solutions.
         % After calling the function q33 I got 4 possible solutions to this problem.



 % 3.4. SEND MORE MONEY
 %
 %      The son of someone how likes puzzles got into financial difficulties
 %      and sent his father the following number puzzle.
 %      Every letter represents a Digit (0,...,9).
 %      Different letters represent different digits.
 %      Leading digits (S and M) can not be 0.
 %
 %           S E N D
 %         + M O R E
 %         ---------
 %         M O N E Y
 %
 %      The task is to write a Prolog program that solves the number puzzle,
 %      i.e., that finds the appropriate values for S,E,N,D,M,O, ... ,
 %      such that the addition is correct.
 %      Write the program in a way that it shows each solution only once
 %      (without using prior knowledge that there is only one soltuion).
 %
 %      This puzzle needs more runtime (approximately 15-30 seconds for the wanted solution).
 %      In order to reduce the testing time, it is recommended
 %      to start with a smaller number puzzle, where we assume
 %      that only the digits 1,2,4,6 and 8 may occur: A B C + A B C = B C D .
 %      As soon as this works, start with S E N D + M O R E = M O N E Y .
 %
 %      Finally, write a procedure q34 that can be called by
 %
 %          ?- q34(Summand1,Summand2,Sum) .
 %
 %      and which returns three lists i.e.
 %          Summand1 = [S,E,N,D] ,
 %          Summand2 = [M,O,R,E] ,
 %          Sum = [M,O,N,E,Y]
 %      with the correct values for S,...,Y.


         q34([S,E,N,D] ,[M,O,R,E] ,[M,O,N,E,Y]) :- 
                        S*1000 + E*100 + N*10 + D + M*1000 + O*100 + R*10 + E #= M*10000 + O*1000 + N*100 + E*10 + Y,
                        [S,E,N,D,M,O,R,Y] ins 0..9, 
                        M #\= 0, S #\= 0 , 
                        all_distinct([S,E,N,D,M,O,R,Y]).


         % This exercise is similar to 3.2 as I have to work with decimals again. However this time I am only sum two numbers.
         % Then I use the equation that the sum of SEND + MORE is equal to MONEY.
         % I need to check that M is not 0 and the same with S, so it a number with 4 digits.
         % I want to match the different letters to numbers 0 to 9 so I use the open list ins 0..9 to to bind multiple variables to domains.
         % However I need to make sure that all the letters that are different have a different number. That is why I use the predicate all_distinct.
 

 % 3.5. This task is an auxiliary step for the next subtask (3.6).
 %
 %      Write a Prolog procedure count(+List,-Number,-Frequency)
 %      that counts and returns as Frequency how often Number occurs in List.
 %
 %      ?- count([5,6,7,5,6,5,6],5,F).  -->  F = 3
 %
 %      ?- count([5,6,7,5,6,5,6],N,3).  -->  N = 5 ; N = 6
 %
 %      Avoid using "\+" (i.e. negation as failure)!
 %      Instead, for checking that two numbers are unequal, use " N1 #\= N2 ".
 %
 %      Hint: Use the techniques that you have learned for list programming


         count([], _ , 0 ).
         count([N|T], N, T0):- count(T, N, F), T0 #= F+1. 
         count([H|T], N, F):- H #\= N, count(T, N, F).

         % In this exercise I only use the video 3.1 for the expression '#=' and '#\'.
         % This exercise is similar to ex 2.15 from the previous assignment in which we had to show the position of the values that are repeated.
         % In this exercise I only need to count how many times is the number in the list.
         % The recursion termination checks if the list is empty and returns 0 because there is no value.
         % The second step is to check if the head (named N) is the number that we are looking for, if it is then we sum 1 to the count F.
         % Otherwise it goes to the last rule and in this case it doesnt sum any number so the count stays the same.
         % It ends when I dont have more elements in the list.


 %  3.6. Magic number
 %
 %       The puzzle is the following:
 %       Find a 10-digit number where the first digit is how many zeros occur
 %       in the number, the second digit is how many 1s occur in the number,
 %       the third digit is how many 2s occur in the number, etc.,
 %       until the tenth digit which is how many 9s occur in the number.
 %
 %       Write a Prolog procedure q36 that can be called by
 %
 %       ?- q36(L) .  --> L=[...] % (concrete values omitted here)
 %
 %       and which returns the list L of digits of this magic number.
 %
 %       The wanted solution may need 10-20 seconds of run-time
 %
 %
 %       Hint: Here, the wanted solution is not permutation-based!
 %             Use your procedure count instead.
 %

        q36(L):- length(L, 10), q36b( L , 0 , L) .             
        q36b( [] , 10 , _ ) .            
        q36b( [H|T] , C , L):- count( L , C , H), C1 #= C+1, q36b( T , C1 , L).         

        % In this exercise I use video 3.1 and 3.2 for the the constraints 
        % First I generate an empty list with lenght 10 because I want a list with numbers from 0 to 9.
        % I use recursion to count the number of elements in the list.
        % My recursion termination is when the list is empty and C is 10.
        % Then my second rule for recursion is when I have a list, C which is a variable used to count from 0 to 1
        % and L which is my final list. 
        % C1 is a temporal variable that I use to determinate which number I am counting in the list.

 % 3.7: Cards puzzle
 %
 %      Solve the cards puzzle (explained in Video 3.c1) using a Prolog program.
 %
 %      You need the cards that you can download from PANDA (3.c2).
 %      Print a paper page with the cards and cut-off the paper into 9 cards.
 %
 %      You shall write a procedure q37 that can be called by
 %
 %      ?- q37(L1,L2,L3).   --> L1 = [N1,N2,N3] ,
 %                              L2 = [N4,N5,N6] ,
 %                              L3 = [N7,N8,N9]
 %
 %      where N1,...,N9 represent the numbers 1,...,9 of the cards in such a
 %      way that the puzzle is solved (see puzzle decription in PANDA).
 %
 %      Hint:
 %      You can extend or modify the program truck.pl
 %
 %      Try to find a solution that is correct!
 %      If you have a solution with more rules and goals,
 %      don't worry, you can get help in the forum for Assignment 3.
        
        q37([N1,N2,N3],[N4,N5,N6],[N7,N8,N9]):-
                permutation(
        [
            [1,'diamonds','hearts','clubs','spades'],             
            [2,'diamonds','clubs','clubs','hearts'],             
            [3,'clubs','diamonds','diamonds','hearts'],
            [4,'clubs','hearts','clubs','spades'],
            [5,'diamonds','spades','clubs','hearts'],
            [6,'hearts','spades','clubs','spades'],
            [7,'diamonds','spades','diamonds','hearts'],
            [8,'diamonds','hearts','clubs','hearts'],
            [9,'clubs','spades','diamonds','spades']
        ],
        [
            [N1 , _ , _ , A , B],
            [N2 , A , _ , C , D],
            [N3 , C , _ , _ , E],
            [N4 , _ , B , F , G],
            [N5 , F , D , H , I],
            [N6 , H , E , _ , J ],
            [N7 , _ , G , K , _ ],
            [N8 , K , I , L , _ ],
            [N9 , L , J , _ , _ ]
        ]).

        % For this exercise I use video 3.7 and p3.7_domino.pl.
        % I use the function build by prolog permutation/2 with a list of all the cards.
        % The card composition is made by the number of the card and the 4 symbols from left, up, right and down as in a clock order.
        % Then a list with the possible solutions with N1-N9 to get the numbers of cards.

 % 3.8. Number shifting puzzle
 %
 %      The number shifting puzzle is as follows:
 %      If we multiply the 6 digit number ABCDEF with G, we get BCDEFA .
 %      Hereby, A,B,C,D,E,F, and G are all different digits from 1 to 9.
 %
 %      Write a procedure q38 that can be called by
 %
 %          ?- q38( ListOf6DigitsABCDEF , SingleDigitG ) .
 %
 %      and that shows each solution of the puzzle exactly once.

        q38([A, B, C, D, E, F], G) :-  
                (A*100000 + B*10000 + C*1000 + D*100 + E*10 + F)*G #= (B*100000 + C*10000 + D*1000 + E*100 + F*10 + A),
                all_distinct([A,B,C,D,E,F,G]),
                [A,B,C,D,E,F,G] ins 1..9,
                label([A,B,C,D,E,F,G]).

        % This example is similar to q32 as I also need check with '#=' that G*[ABCDEF] is equal to [BCDEFA].
        % I need to use video 3.1 for this.
        % Then I need to set the variables A-G to a number in range 1-9 so I use ins for that.
        % Finally I use label to get a explicit solution so it sets the letters A-G a corresponding number.

 % 3.9. This task is an auxiliary step for the next subtask (3.10).
 %
 %      Write a Prolog procedure check1(+Constraint)
 %      that checks whether a Constraint can be fulfilled where
 %       - Constraint is a list of the format [Result,Operator|Operands] ,
 %       - Operator is either * or / ,
 %       - Operands is a list of exactly 2 operands if the Operator is / ,
 %              and is a list of 1 or more operands if the Operator is * .
 %       - Result is the result of applying the Operator to the Operands in any order
 %
 %      Here are some example queries and expected answers:
 %
 %      ?- check1([Result,/,8,4]).  -->  Result = 2
 %      ?- check1([Result,/,4,8]).  -->  Result = 2
 %      ?- check1([2,/,8,H] .     -->  H 4 ; H 16
 %      ?- check1([2,/,H, ) .     -->  H 16 ; H 4
 %      ?- check1([8,/,2,H] .     -->  H 16
 %      ?- check1([Result,/,5,6]).  -->  false
 %
 %      ?- check1([Result,*,5,6]).    -->  Result = 30
 %      ?- check1([Result,*,1,2,4]).  -->  Result = 8
 %      ?- check1([8,*,1,H,).       -->  H 2
 %      ?- check1([240,*,1,2,H,5]). -->  H 6
 %
 %
 %      Hint: To implement the rules for the operator *,
 %            use the techniques that you have learned for list programming
                
        check1([ Result, (/)| [H|[T]]]) :- H #= Result*T; T #= Result*H .
        check1([Res, (*) | T]) :- check1(Res, 1, T) .
        check1(A, A, []) .
        check1(Result, A, [H|T]) :- N #= A * H, check1(Result, N, T).

        % For this task I use videos 3.1 and 3.2.
        % I split this task into 2 subproblems. One for division and one for multiplication.
        % The first one is the division and I use the definition of a division which is the Denominator multiplied with the result is equal to the numerator. 
        % I need to make sure that the numerator is always bigger than the denominator, so in the first case H is the numerator and T the denominator and the other way round when the H is smaller than T.
        % The rest of the rules are for the multiplication. I need to use the knowledge obtained in assignments 2 of recursion.
        % The first rule of the multplication is for my recursive termination, in which I multiply the first term of the list by 1, and store the results in Res.
        % I keep cheching and multipling the result of the previous one with the head of the list until the list is empty so I return the value obtained in A, which is my temporal storage.

 % 3.10. Program a KenKen puzzle solver.
 %
 %       KenKen is explained in Video 3.14. and obeys the following rules:
 %       A KenKen board is a 4 x 4 grid containing only the numbers 1,2,3,4 .
 %       In each row, all numbers are different from each other.
 %       In each column, all numbers are different from each other.
 %       In each of the example puzzles, additional neighbours are given
 %       regarding the product or quotient of certain fields in the grid,
 %       where each Constraint is given in the format that requested by Subtask 2.9.
 %       In each of the example puzzles, the neighbours are given.
 %
 %       You shall program the procedure   kenken( -Rows, +Constraints ) ,
 %       such that a call of the given procedure do_KenKen(N) shows the values for the rows
 %       of a given KenKen puzzle.
 %
 %       For example, together with your procedure   kenken( -Rows, +Constraints ) ,
 %       the following calls should return the following results:
 %
 %       ?-do_KenKen(1,[Row1,Row2,Row3,Row4]). -->  Row1 = [1, 2, 3, 4],
 %                                                  Row2 = [2, 3, 4, 1],
 %                                                  Row3 = [3, 4, 1, 2],
 %                                                  Row4 = [4, 1, 2, 3] ;
 %
 %       ?-do_KenKen(2,[Row1,Row2,Row3,Row4]). -->  Row1 = [1, 4, 2, 3] ,
 %                                                  Row2 = [3, 2, 4, 1] ,
 %                                                  Row3 = [2, 1, 3, 4] ,
 %                                                  Row4 = [4, 3, 1, 2]
 %
 % Hints: 1. Use p13_sudoku.pl as a template to get a square matrix having the properties that
 %           in each row, all numbers are different from each other and
 %           in each column, all numbers are different from each other.
 %           In contrast to the Sudoku solver, the KenKen solver need one additional parameter
 %           in order to pass a list of neighbours from the puzzle to the solver.
 %        2. You can use maplist to apply your procedure check1 from 3.9. to the list of all neighbours
 %        3. When using the tester, it will report whether or not there is a different result,
 %           when procedure do_KenKen is called with certain parameters.
 %           However, if a counter example for do_KenKen is reported, you shall nevertheless
 %           modify your implemented procedure kenken(...,...)




 do_KenKen(N, Rows) :-                    % DO NOT CHANGE THIS PROCEDURE
    problem(N, Rows, Constraints),         % get KenKen puzzle N
    kenken(Rows,Constraints),             % call the kenken solver
    maplist(label,Rows) .                 % bind the values if not yet done

 % IMPLEMENT PROCEDURE  kenken(...,...)   here:

 kenken(Rows, Constraints) :- 
        length(Rows, 4),  
        maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..4,
        maplist(all_distinct, Rows), 
       	maplist(check1, Constraints),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns).


% For this exercise I use video 3.12 sudoku solver as a template.
% I only need to write the function kenken with the parameters Rows and Constraints.
% Then I check that the length of Rows is 4 because I want to have 4 columns as it is a grid of 4x4.
% Then I need to make sure that each row is now also a list of 4 elements.
% The append is the same as in sudoku example but this time I only want to append Vs ins 1..4 as I only have 4 rows this time.
% In each row all numbers must be different.
% Then I use check1 from the previous exercise to apply the Constraints of each rows.
% I transpose square martrix of numbers
% Each Column must be also different like the Rows.

 problem(1, [[A,B,C,D],
             [E,F,G,H],
             [I,J,K,L],
             [M,N,O,P]
            ] ,                           % neighbours     conclusions
            [[6,*,A,B,C],                 % A*B*C #= 6  --> D=4
             [4,*,D,H],                   % D*H   #= 4  --> H=1
             [18,*,E,F,I],                % E*F*I #= 18 --> E=2,I=3,F=3 --> G=4
             [16,*,G,J,K],                % G*J*K #= 16 --> J*K=4 --> L=2 --> P=3,J=4,K=1
             [12,*,L,P,O],                % L*P*O #= 12 --> O=2   --> C=3 --> B=2 --> A=1 --> M=4,N=1
             [4,/,M,N]
            ] ).

 problem(2, [[A,B,C,D],
             [E,F,G,H],
             [I,J,K,L],
             [M,N,O,P]
            ] ,                           % neighbours     conclusions
            [[12,*,M,N,O],                % M*N*O #=12  --> P=2
             [6,*,C,D,H],                 % C*D*H #= 6  --> L=4 , C=2
             [96,*,G,K,L,P],              % ...         --> G=4 --> K=3 --> O=1
             [6,*,E,I,J],                 % ...
             [8,*,A,B,F]                  % A*B*F #= 8  --> ...
            ] ).

 problem(3, [[A,B,C,D],
             [E,F,G,H],
             [I,J,K,L],
             [M,N,O,P]
            ] ,
            [[2,*,A,B,E],                 % neighbours     conclusions
             [48,*,C,D,H],
             [18,*,F,G,J,K],
             [2,/,L,P],
             [96,*,I,M,N,O]
            ] ).

 problem(4, [[A,B,C,D],
             [E,F,G,H],
             [I,J,K,L],
             [M,N,O,P]
            ] ,
            [[6,*,A,B,C],                 % neighbours     conclusions
             [2,/,D,H],
             [6,*,J,M,N],
             [2,/,K,L],
             [12,*,F,G],
             [12,*,O,P],
             [4,/,E,I]
            ] ).










