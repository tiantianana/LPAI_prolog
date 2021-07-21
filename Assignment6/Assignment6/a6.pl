
 % Assignment 6 - programming in domain specific languages
 % 6.1. A rewrite rule systems for solving simple linear equation
 % 6.2. and 6.3. Inheritance programming in natural language



 %
 %      Again, code as short as possible, i.e avoid unnecessary rules, unnecessary goals, and singleton variables.


 % 6.1. The task is to implement a rewrite rule system that isolates variables
 %      in linear inequalities which contain each variable at most once
 %      and contain only the operators '<', '>', '>=', '=<', '+', and '-'.
 %
 %      The precedence and the associativity of the built-in operators
 %      '<', '>', '=<', '>=', '+' , and '-' are defined in such a way that unnecessary
 %      parenthesis are resolved, e.g. as follows:
 %
 %      ?- ( X = ( ( ( ( (a-b) + c ) - d ) + e ) < f )) .
 %      X = ( a-b+c-d+e < f ) .
 %
 %      That is, '+' and '-' bind stronger than '<', '>', '>=', and '=<' .
 %
 %      Define the operators 'if' , '===>' , and ' :-> ' with appropriate operator precedences,
 %      such that parentheses are resolved as follows:
 %
 %      ?- R = ( ( ( (2+a) < 5 )  :->  a ) ===> ( a < 3 ) if X = X , Y = Y ) .     -->
 %      R =  (2+a<5  :->  a ===> a<3 if X=X, Y=Y).
 %
 %      Write a procedure isolate/2 that isolates a variable in simple linear equations,
 %      containing only one 'variable' for which the equation shall be resolved,
 %      and contaning only +,-,<,>,=<, and >= as arithmetic operators.
 %
 %      You shall use the operator ' :-> ' to define according to which 'variable' an equation shall be resolved.
 %      For example, to resovle an equation ' a-(2+x) < c-(5-b) ' to a variable 'x' you should be able
 %      to submit the following query and get the following answers:
 %
 %          ?- isolate( a-(2+x) < c-(5-b)  :->  x , R ).
 %          R =  (a-(2+x)<c-(5-b):->x) ;
 %          R =  (2+x>a-(c-(5-b)):->x) ;
 %          R =  (x>a-(c-(5-b))-2:->x) ;
 %          false.
 %
 %      However, to resolve the same equation to the variable 'b', the program should be able to isolate as follows:
 %
 %          ?- isolate( a-(2+x) < c-(5-b)  :->  b , R ).
 %          R =  (a-(2+x)<c-(5-b):->b) ;
 %          R =  (c-(5-b)>a-(2+x):->b) ;
 %          R =  (5-b<c-(a-(2+x)):->b) ;
 %          R =  (b>5-(c-(a-(2+x))):->b) ;
 %          false.
 %
 %      Hereby, the number and the order of intermediate steps may differ, but the final result should be the same.
 %      Note that 'variables' in the query use lower case letter, e.g. x,
 %      because Prolog treats these 'variables' as atoms.
 %
 %      Try to find a solution with no more than 10 rewrite rules using the operator '===>'.
 %
 %      As soon as your procedure isolate works as intended, write a procedure isolate_final/2 that
 %      returns only the final result of the isolation, i.e.
 %
 %          ?- isolate_final( a-(2+x) < c-(5-b)  :->  x , R ).
 %          R =  (x>a-(c-(5-b))-2:->x).
 %
 %          ?- isolate_final( a-(2+x) < c-(5-b)  :->  b , R ).
 %          R =  (b>5-(c-(a-(2+x))):->b).
 %
 %      Make sure that isolate_final/2 works correctly locally before you try it with the tester6.
 %
 %      Hints:
 %      1. You can program isolate/2 similar to rw/2 in program rw11.pl
 %
 %      2. You can use the procedure occursl.pl in order to find out, whether x occurs on the left hand side
 %         or on the right hand side of an inequality relation.



 % occursl(-Literal,+Term)  - to be called in Exercise 6.1

occursl( N, N ) :- \+ compound(N).
occursl( N, T ) :- compound(T) , T=..L , member(M,L) , occursl( N, M ).

 % ===================   Encode your solution to Assignment 6.1 here:  =================================

:- op( 1150, xfx, 'if'   ).    % Lower precedence value than :- (1200) and ?- but higher value than ; (1100)
:- op( 800,  xfx, '===>' ).    % Precedence between \+ (900) and arithmetic/comparison operators (700)
:- op( 750,  xfx, ':->'  ).    % Binds stronger than ===> (smaller than 800)
 % The operations precedence value are based on the video 6.1 prolog operators.

isolate( Old , Old ) .                                              % rule applyer
isolate( Old , New ) :- subst( Old, Zw ) , ! , isolate( Zw, New ).  % here to weak
% Im using the same function as rewrite from video 6.4 and program p6.4_rw11.pl

% subst(+Old, -New ) - apply any substition to a subtree of Old to get New
subst( Old , New ) :- Old =.. [F,A|As] , substl( [F,A|As], Ns ) , New =.. Ns .  
subst( Old , New ) :- ( Old ===> New if B ) , B .      % inner substitutions first

%   substl( +L , -LwithSubst ) - apply subst to any element of L
substl( [A|As], [N|As] ) :- subst( A, N ) .
substl( [A|As], [A|Ns] ) :- substl( As, Ns ) .
% Subst and substl are functions taken from program p6.4_rw11.pl aswell.

% Rewrite rules
A < B :-> X      ===>  B > A :-> X        if occursl(X, B).
A > B :-> X      ===>  B < A :-> X        if occursl(X, B).
% Bring 'variable' X to left side of equation.

% Rules for >= and =< only.
A + B < C :-> X  ===>  A < C - B :-> X    if occursl(X, A).
A + B > C :-> X  ===>  A > C - B :-> X    if occursl(X, A).
A - B < C :-> X  ===>  A < C + B :-> X    if occursl(X, A).
A - B > C :-> X  ===>  A > C + B :-> X    if occursl(X, A).
% Bring A to the left side of the equation

A + B < C :-> X  ===>  B < C - A :-> X    if occursl(X, B).
A + B > C :-> X  ===>  B > C - A :-> X    if occursl(X, B).
A - B < C :-> X  ===>  B > A - C :-> X    if occursl(X, B).
A - B > C :-> X  ===>  B < A - C :-> X    if occursl(X, B).
% Bring B to the left side of the equation

A =< B :-> X      ===>  B >= A :-> X        if occursl(X, B).
A >= B :-> X      ===>  B =< A :-> X        if occursl(X, B).

A + B =< C :-> X  ===>  A =< C - B :-> X    if occursl(X, A).
A + B >= C :-> X  ===>  A >= C - B :-> X    if occursl(X, A).
A - B =< C :-> X  ===>  A =< C + B :-> X    if occursl(X, A).
A - B >= C :-> X  ===>  A >= C + B :-> X    if occursl(X, A).

A + B =< C :-> X  ===>  B =< C - A :-> X    if occursl(X, B).
A + B >= C :-> X  ===>  B >= C - A :-> X    if occursl(X, B).
A - B =< C :-> X  ===>  B >= A - C :-> X    if occursl(X, B).
A - B >= C :-> X  ===>  B =< A - C :-> X    if occursl(X, B).
% The same as before but with => and =<

isolate_final( In, Out ):- subst(In, Intm), !, isolate_final(Intm, Out).
isolate_final( Out, Out ).
% Same rule to rewrite from p6.4_rw11.pl but swapped because I only want to output the final solution.

% Overall view
% This program is very similar from the Demo programs of Chapeter 6 rewrite.
% However the rules are adapted to this specific case.
% The key for the rewrite rules is to bring the variable wanted, which is specified in the expression
% ':->' to the left. So we can leave this X variable in the left and the rest will be in the right.
% The rule occursl works as an equal for example if occursl(X, B) means that if X=B then this rule is called.
% Another point of this program is that all the rules reduce the '()' but do not remove any term. So basically
% the rewrite rules always rewrites the same subtrees and never removes any expression.
% The limitation of this program is that it does not resolve any mathematical expression, it only simplifies. If we want to solve the
% equation then we would just need to add more rules that reduces and calculates a specific result.

 % 6.2.+6.3.
 %      The goal of assignments 6.2 and 6.3 is to implement a question answering system
 %      that does inferences on instance-of relations, isa-hierachies, and ownership relations.

 %      Both assignments together shall build a question answering system which you can use like this:
 %      ?- dialog(_).
 %      |: every student is a person.
 %      |: every worker is a person.
 %      |: peter is a person.
 %      |: sarah is a student.
 %      |: sven is a student.
 %      |: walter is a worker.
 %      |: werner is a worker.
 %      |: every student has a bike.
 %      |: every worker has a car.
 %      |: every person has a home.
 %      |: sven has a car.
 %      |: what is sven?
 %      sven is a student.
 %      sven is a person.
 %      |: who has a car?
 %      every worker has a car.
 %      walter has a car.
 %      werner has a car.
 %      sven has a car.
 %      |: what does sven have?
 %      sven has a bike.
 %      sven has a home.
 %      sven has a car.
 %      |: who is a person?
 %      peter is a person.
 %      sarah is a person.
 %      sven is a person.
 %      walter is a person.
 %      werner is a person.
 %      |: who has a home?
 %      peter has a home.
 %      sarah has a home.
 %      sven has a home.
 %      walter has a home.
 %      werner has a home.
 %      |: stop.
 %      true.
 %
 %      Note that your program should be programmed in a way that it also works with a different knowledge base, e.g.:
 %      ?- dialog(L).
 %      |: every husband is a human.
 %      |: every husband has a wife.
 %      |: every human has a mother.
 %      |: sven is a husband.
 %      |: what does sven have?
 %      sven has a wife.
 %      sven has a mother.
 %      |: stop.
 %      L = [isa(sven, husband),  (hasa(_13778, mother):-isa(_13778, human)),  (hasa(_13344, wife):-isa(_13344, husband)),  (isa(_12910, human):-isa(_12910, husband))].



 % 6.2. Write a grammar for sentences, such that the sentences above can be parsed. If called e.g. by
 %          ?- sentence( S ,  [sven,has,a,car,'.'] , [] ) .
 %      the desired output is
 %          S = hasa(sven,car)
 %      And for
 %          ?- sentence( S ,  [every,student,is,a,person,'.'] , [] ) .
 %      the desired output is
 %          S =  ( isa(X,person) :- isa(X,student) )
 %      where the variable name may be different from X, but must be the same in the head and the body of the rule.
 %
 %      Furthermore, for
 %          ?- sentence( S ,  [every,student,has,a,bike,'.'] , [] ) .
 %      the desired output is
 %          S =  ( hasa(X,bike) :- isa(X,student) )
 %      where the variable name may be different from X, but must be the same in the head and the body of the rule.
 %
 %      Write a grammar for questions, such that the questions above can be parsed. If called e.g. by
 %          ?- question( Q ,  [what,does,sven,have,'?'] , [] ) .
 %      the desired output is
 %          Q = hasa(sven,X)
 %      where the variable name may be different from X.
 %
 %      Furthermore,for a call
 %          ?- question( Q ,  [who,is,a,person,'?'] , [] ) .
 %      the desired output is
 %          Q = isa(X,person)
 %      where the variable name may be different from X.
 %
 %      Finally, make sure that you can also parse questions like [who,has,a,car,?] and [what,is,sven,?] .
 %
 %
 %      Hints:
 %      1. The grammar for sentences and for questions should be significantly simpler
 %         than the corresponding grammars in the program p5.8_tell_and_ask_more.pl,
 %         as it is sufficient to parse exactly these kinds of sentences and questions.
 %      2. Furthermore, as the sentences are so limited, to sentences without relative clauses,
 %         containing 'is' and 'has' as verbs only, you do not need a complex sentence grammar, i.e.,
 %         4 straight-forward parsing rules for questions and
 %         4 parsing rules sentences should be sufficient to do all the parsing



 % ===================   Encode your solution to Assignment 6.2 here:  =================================


% sentence( -Fact, +WordList, -RemainingWordList )
% '... has a ...' type sentence
sentence( hasa(Subj, Obj) ) --> 
   [Subj, has, a, Obj, '.'].

% '... is a ...' type sentence
sentence( isa(Subj, Class) ) --> 
   [Subj, is, a, Class, '.'].

% 'every ... is a ...' type sentence
sentence( isa(Subj, C2) :- isa(Subj, C1)) -->
   [every, C1, is, a, C2, '.'].

% 'every ... has a ...' type sentence
sentence( hasa(Subj, C2) :- isa(Subj, C1)) --> 
   [every, C1, has, a, C2, '.'].
% 4 sentence, each one for each type of sentence. The 2 first sentence are the type 'hasa' or 'isa'
% and the 2 other sentence are the type 'every .. has a' or 'every .. is a'.

% 'who is a ...' question
question( isa(_, Class) ) -->
    [who, is, a, Class, '?'].

% 'what is ...' question
question( isa(Subj, _) ) -->
    [what, is, Subj, '?'].

% 'who has a ...' question
question( hasa(_, Obj) ) -->
    [who, has, a, Obj, '?'].

% 'what does ... have' question
question( hasa(Subj, _) ) -->                 
    [what, does, Subj, have, '?'].

% 4 question for each type of question. The first 2 questions are the type isa for classes and the last 2 questions
% are the type hasa for objects.

% Lexicon
lex( human,   class ).
lex( husband, class ).
lex( person,  class ).
lex( student, class ).
lex( worker,  class ).

lex( peter,   name ).
lex( sarah,   name ).
lex( sven,    name ).
lex( walter,  name ).
lex( werner,  name ).
lex( bob,  name ).

lex( bike,    attr ).
lex( car,     attr ).
lex( home,    attr ).
lex( mother,  attr ).
lex( wife,    attr ).

 % ===================   Encode your solution to Assignment 6.3 here:  =================================

 % you ONLY need to program the procedure getAnswer/3 (plus most likely an auxiliary procedure called by getAnswer/3)
 % Hint: Both together is possible in 3 rules and 5 goals (in total).

 % 6.3. Implement a question answering system that can do inferences
 %
 %      The language subset shall be restricted to the same words that are given above (before Assignment 6.2.).
 %
 %      In a first step, make sure that you get the correct direct answers (facts),
 %      for all 4 types of questions, i.e.
 %      ?- dialog(L).
 %      |: sven is a student.
 %      |: sven has a car.
 %      |: what is sven?
 %      sven is a student.
 %      |: who is a student?
 %      sven is a student.
 %      |: what does sven have?
 %      sven has a car.
 %      |: who has a car?
 %      sven has a car.
 %      ...
 %
 %      For that purpose, you ONLY have to program the procedure getAnswer/3 (plus most likely
 %                                                 an auxiliary procedure called by getAnswer/3) .
 %
 %      Note that the answer sentences generated by getAnswer should include a fullstop, i.e.,
 %      the desired answer for
 %            ?- getAnswer(isa(sven,X),[isa(sven,student)],Y) .
 %      is
 %             X = student,
 %            _Y = [sven, is, a, student, '.'] ;
 %
 %
 %      In a second step, make sure that you implement an inference rule
 %      for the instance-of relation and the is-a relation,
 %      such that the following question can be answered correctly.
 %      |: every student is a person.
 %      |: sven is a student.
 %      |: what is sven?
 %      sven is a student.
 %      sven is a person.
 %      |: who is a person?
 %      sven is a person.
 %       
 %
 %      In a final step, make sure that you implement an inference rule for the has-a relation.
 %      |: every student is a person.
 %      |: every student has a bike.
 %      |: every person has a home.
 %      |: sven is a student.
 %      |: sven has a car.
 %      |: what does sven have?
 %      sven has a car.
 %      sven has a bike.
 %      sven has a home.
 %      |: who has a home?
 %      sven has a home.
 %
 %      Hints:
 %      1. You can use the procedures for the dialog (below) copied from the program p5.8_tell_and_ask_more
 %         Do NOT change the procedures dialog/1, tellask/2, and sORq/4 !
 %         You need a new implementation of the procedure getAnswer/3.
 %
 %      2. You may need additional inference rules for is-a and has-a relationships
 %
 %      3. If you need to read a rule from the knowledge base, be aware that the operator (:-) has the lowest precedence.
 %         Therefore, for looking up a rule in a list of rules, you may need extra parentheses as e.g. in the goal
 %                 ... ,   member(   ( Head :- Tail )   ,   ListOfRules   )   , ...
 %
 %      4. In the wanted solution, the procedure getAnswer/3 has only 5 rules and 8 goals,
 %         and implements the inference rules for is-a and has-a relationships,
 %         and the answer sentence generation from answers to questions.
 %




 %=================================    Start of the part that shall NOT be changed      ============================
 % The following code is taken from the demo program p5.z_price_dialog.pl and shall NOT be changed

 % do NOT change this procedure (dialog):
 % dialog( -PiecesOfKnowledgeCollectedDuringTheDialog )
 dialog(L) :- tellask( [ ] , L ) , ! .         % the initial state of knowledge is [ ] , the final state is L

 % do NOT change this procedure (tellask/2):
 % ask question on OldState or read sentence and move to NewState
 % tellask( +OldState , -NewState )
 tellask( OldState , NewState ) :-
    readln( SentenceORQuestion ) ,             % read a sentence or question
    SentenceORQuestion  \= [stop|_] ,          % exit loop when input starts with "stop"
    sORq( SentenceORQuestion , Answers,        % process one sentence, question or syntax error
          OldState , InterState ) ,            % and change state from OldState to intermediate state InterState
    ! ,                                        % no backtracking when sentence or when syntax error was processed
    llout(Answers) ,                           % show all answers, empty if it was a sentence
    tellask( InterState , NewState ) .         % process more sentences or questions (or syntax errors)
 tellask( State , State ).                     % at end of loop, copy OldState to the new State, i.e. list L

  % llout( +ListOfAnswers ) - print a list of answer sentences
    llout([]).
    llout([A|As]) :- mylout(A) , llout(As) .
        mylout([]) :- nl .
        mylout([K|R]) :- write(K) , write(' ') , mylout(R) .


 % do NOT change this procedure (sORq) - except for (de-)activating 3rd clause:
 % process one sentence or question or syntax error
 % sORq( +SentenceORQuestion , -Answers , +OldState, -NewState )
 sORq( Sentence , [] , OldState , [S|OldState] ) :-  % Extend OldState by S
    sentence( S, Sentence, [] ) .              % if you read a sentence S.

 sORq( Question , Answers , State, State ) :-  % do not change the State
    question( Q, Question, [] ) ,              % if you read a question Q,
    findall( Answer ,                          % use State to find each Answer to Q
             getAnswer(Q,State,Answer) ,       % find all sentences Answer matching Q in State
             Answers                           % return list of answers in plural form
           ) .

 % Remove the comment of the following clause when your grammar has the correct syntax
 % to simplify the test of procedures move/3 and getAnswer/3 by locating syntax errors in the dialog's input
       sORq(Input, [ Answer , ['(exit with "stop")'] ], State, State ) :- syntaxError( Input, Answer ) .

 % Simplified syntax error message, not locating the syntax error
 sORq( _ , [ ['Syntax error - exit with "stop"'] ], State, State ) .  % do not change the State


 % catches only some syntax errors and returns first found syntax error as Answer
 syntaxError( Input, Answer ) :-
    longPrefix(P,Input) ,                      % try longest prefix P of Input first
    syntaxOK(P) ,                              % until and including P Input still syntactical correct
    append( P, Error, Input ),                 % separate Input into correct part P and errorpart Error
    append( [ 'Syntax error: '| P ] ,          % Syntax error message will become the answer
            [ ' <HERE> ' | Error ] ,
            Answer ) .

 % longPrefix( -Prefix , +Input_)              % used to find longest (correct) Prefix of Input first
 longPrefix([H|P],[H|L]) :- longPrefix(P,L) .  % nearly prefix/2 of Assignment 2.3,
 longPrefix([],_) .                            % but returns longest prefixes first

 % syntayOK( +P )  -  P can be extended to a correct sentence or question
 syntaxOK(P) :- append(P,_,Sent), sentence(_,Sent,[]).  % P can be extended to a correct sentence
 syntaxOK(P) :- append(P,_,Sent), question(_,Sent,[]).  % P can be extended to a correct question


 %=================      End of the part that shall NOT be changed                    ============================

 % getAnswer(+Question,+State,-Answer).

getAnswer( isa(Subj, Class), State, [Subj, is, a, Class, '.']) :- 
   member(isa(Subj,Class), State).
% Case for 'who is' or 'what is' question. Example: What is Sven --> Sven is a student.
% Search for facts that are the type isa(Subj, Class) and returns an answer of type [Subj, is, a, Class, '.'].
% Search if the question is part of the state with the goal member().

getAnswer( hasa(Subj, Attr), State, [Subj, has, a, Attr, '.']) :- 
   member(hasa(Subj, Attr), State).
% Case for 'what does' or 'who has' is the same as before but with the verb has instead of is.
% Again checks if the question is part of the state and returns a list that matches 'Peter has a bike.' for example.

getAnswer( isa(Subj, Class), State, [Subj, is, a, Class, '.']) :- 
   getis(Sub, Class, State),
   member(isa(Subj, Sub), State).
% Case for 'what is' and accepts the state 'every Subj is a Class'.
% The question that recieves is the same as before and again returns the same list.
% However this time we create a function getis which searchs for all the subclasses so it iterates
% over the sentence and checks if the subclass is part of the state.
% This only workds for isa type.

getAnswer( hasa(Subj, Attr), State, [Subj, has, a, Attr, '.'] ):-
   getAnswer(isa(Subj, Class), State, _),
   member(hasa(Subj, Attr):- isa(Subj, Class), State).
% Case for 'who has' or 'what does', same as the previous case but accepts only 
% 'every Subj has a Class', so has instead of is.
% The get answer asks for the question 'what is' and get a set of answers that we will use for the next goal.
% The second one checks if the sentence 'every Subj has a Class' is part of the State.
% If it is the case the answer that will give is a list of 'Subj has a Attr.' for example 'Peter has a bike'.

getis( Sub, Class, State ):-
    member(isa(Subj, Class):- isa(Subj, Sub), State).
getis( Sub, Class, State ):-
    member(isa(Subj, T):- isa(Subj, Sub), State),
    getis(T, Class, State).

% This function checks if the subclass is part of the state recursively.
% In the case that we find that it is part then the function finishes and returns the Subclass.
% If it didnt find the solution then it goes to the second rule and creates an intermade state or temporal
% state and calls the function again as a basic DFS search acyclic. So we iterate until we find a valid solution.

 % ================      Here, write your general comments to the extra questions     ============================
 % ================      mentioned in the file Starting with Assignment 6.pdf :       ============================

% getAnswer/3
% The getAnswer/3 proccedure has two types of rules. This two types of rules are for 'who is' or 'what is' and
% the other type is for 'who does' and 'what does'. However this two rules only read direct answers. To solve this
% problem I have to create two new rules from the same type but more extended that are able to use inference. 
% The function getis is an auxiliary function to help 'what is' type answer.

% Overview of the program 6.2 and 6.3
% This program works similar to the one from assignment 5. The part that we dont need to modify is exactly the same
% as p5.10_price_dialog.pl. The rules 'sentence' and 'questions' are adapted to this program and of course the lexicon
% changed aswell as the rule getAnswer/3 explained before.
% One of the things that makes this problem simplier is that it doesnt check if the class, attribute or person is in the
% list of lexicons, so it doesnt check if the fact exists or contradicts an existing fact, which is a disadvantage 
% or limitation of this program. I dont need to parse from singular to plural or transitive and intransitive etc.
% Also as I only work with 2 verbs: hasa and isa.  If I want to introduce a new verb I would need to create
% new rules instead of adding new lexicons as before. So for example 'every people can sing' would need to add the 
% same rules but with the verb can, which is way larger than adding only a lexicon but easy to implement.
% On the contrary, if I want extend my program adding new attributes and classes, then it would be very easy as I
% only need to add an extra lexicon.
% In general it is a simplier program but more limited in the sentence and answers type.