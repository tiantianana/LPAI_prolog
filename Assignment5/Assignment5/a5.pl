
 :- use_module(library(clpfd)).

:- discontiguous lex/2.   % lexicon facts may be distributed over the file (although NOT recommeded)
:- discontiguous lex/3.
:- discontiguous lex/4.



%=======================================================================================================

% Assignment 5: Question answering systems and semantic domain modelling
%	         Modelling the domain of ownership
%
% This assignment is a larger programming assignment with the following goals:
% You shall get a deeper understanding of question answering systems, text parsing,
% agreements, semantics construction, domain modelling and other aspects of
% syntax-driven text understanding.
% No sub-exercise (5.1.- 5.6.)  is intended to last longer than an hour.
% If you get stuck, you can get hints by asking in the forum.
% You have to combine ideas presented in different demo programs

/* The following is an example of the overall dialog that your system should be able to do:
 *  (this should be achieved at the end of all the subtasks which are described below)
 *
?- dialog(L), llout(L).
|: hans has 10 forks.
|: peter owns 3 plates.
|: hans gives 6 euros to anna.
|: hans buys 2 plates for 7 euros from peter.
|: what does hans have?
hans has -13 euros
hans has 2 plates
hans has 10 forks
|: peter possesses 12 knives.
|: bernd holds 20 euros.
|: peter sells 1 knife for 1 euro to bernd.
|: what does bernd have?
bernd has 1 knife
bernd has 19 euros
|: what does hans own?
hans owns -13 euros
hans owns 2 plates
hans owns 10 forks
|: what does bernd possess?
bernd possesses 1 knife
bernd possesses 19 euros
|: peter who owns 3 forks sells 1 knife for 1 euro to bernd who gives 2 forks to anna who has 5 plates.
|: what does bernd possess?
bernd possesses 2 knives
bernd possesses 18 euros
bernd possesses -2 forks
|: stop.
anna owns 6 euros
anna owns 2 forks
anna owns 5 plates
bernd owns 2 knives
bernd owns 18 euros
bernd owns -2 forks
hans owns -13 euros
hans owns 2 plates
hans owns 10 forks
peter owns 1 plates
peter owns 9 euros
peter owns 10 knives
L = [[anna, owns, 6, euros], [anna, owns, 2, forks], [anna, owns, 5, plates], [bernd, owns, 2, knives], [bernd, owns, 18, euros], [hans, owns, -13, euros], [hans, owns, 2, plates], [hans, owns , 10|...], [peter, owns|...], [peter|...], [...|...]].
*/



%================= The sub-tasks ======================

% 5.1. For this subtask, you should modify some parts of the program p5.12._relative_clauses,
%      and leave other parts of the program as they are.
%      In total, you shall modify the procedure move/3, the grammar for sentences, and the lexicon,
%      such that the call of
%            ?-dialog(L) , llout(L) .
%      is able to do a conversation like:
%            ?-dialog(L).
%            |: hans owns 5 plates.
%            |: peter owns 4 forks.
%            |: anna owns 7 plates.
%            |: hans owns 1 plate.
%            |: stop.
%            anna owns 7 plates
%            hans owns 1 plates
%            peter owns 4 forks
%            L = [[anna, owns, 7, plates], [hans, owns, 1, plates], [peter, owns, 4, forks]].
%
%      Do NOT change the code in the marked region given below (and taken from the program p5.12._relative_clauses),
%      (as otherwise the tester5 might throw unforeseeable errors),
%      but augment the given code only.
%      The program p5.10_price_dialog.pl gives a hint how to integrate numbers into the sentence grammars.
%
%      Your lexicon shall contain only 9 facts for the following words:
%      hans, anna, rita, peter, bernd, plate/plates, fork/forks, knife/knives, owns/own .

%      Take the sentence parser from the program  p5.12._relative_clauses as a starting point.
%
%      First, modify the right-hand side of the DCG rule vp/4 for verb phrases without relative clause, i.e.,
%            vp( YourParameter , [] ) --> ...
%      in such a way that the vp rule can consume a verb phrase like [owns,1,plate] , i.e.,
%            ?- vp( VP , [] , [owns,1,plate] , [] ) .
%      returns
%            VP = [owns,1,plates] .
%      Note that the list VP shall contain the plural form "plates" although it was only 1 "plate" !
%      Like in the demo program p5.10_price_dialog.pl, you will also need auxiliary grammar rules
%            1. for transitive verbs (e.g. "owns"),   and
%            2. for numbered objects (e.g. for "1 plate").
%
%      Having implemented the grammar rule for vp/4, a query
%            ?- sentence( S, [hans,owns,1,plate,'.'] , [] ) .
%      should return
%            S = [ [hans,owns,1,plates] ]
%      i.e., it shall return a list with one element which is a list of words
%      and   it shall return the plural form "plates", although it was only 1 "plate" (!),
%      and   it shall NOT return the '.' occurring in the sentence .

%      The grammar rule for numbered objects shall be written in such way
%      that the sentence grammar accepts only syntactically correct sentences, i.e., the expected answer for
%            ?- sentence( S, [hans,owns,1,plates,'.'] , [] ) .
%      is    false .
%
%      Second, write a new (a different) procedure move/3 using a procedure setVal/3
%      similar to the corresponding procedure in the demo program price_dialog,
%      such that you can use the move/3 procedure as follows
%         ?-OldState = [ [hans,owns,1,plates] ] ,
%           move( [hans,owns,5,plates] , OldState , NewState ) .
%      and you get
%         NewState = [ [hans,owns,5,plates] ]
%      i.e., in NewState, the new number (5) of plates replaces the old number (1) of plates in OldState
%
%      However, make sure that your procedure setVal sorts the lists of sentences alphabetically
%      (by using the built-in procedure sort/2), such that you e.g. can call
%         ?-OldState = [ [anna,owns,7,plates] , [peter,owns,4,forks] ]  ,
%           move( [hans,owns,1,plates] , OldState , NewState ) .
%      and you get
%         NewState = [ [anna,owns,7,plates] , [hans,owns,1,plates] , [peter,owns,4,forks] ]  .
%
%      Third, check that you also can parse nested sentences correctly, e.g.,
%            ?- sentence( Ss, [hans,who,owns,2,plates,owns,1,fork,'.'] , [] ) .
%      should return the following list Ss of subsentences:
%            Ss = [ [hans,owns,2,plates] , [hans,owns,1,forks] ]
%      Hereby, check that the list representing the relative clause precedes the list representing the main sentence


% 5.2. Add two DCG rules for questions to the grammar, one for each type of question, i.e.
%      a query
%            ?- question( Q, [how,many,plates,does,hans,own,'?'] , [] ) .
%      should return a result for Q that is unifyable with an element of the list
%      returned for S of the sentence/3 query (from 5.1) , i.e.,
%            Q = [hans,owns,_,plates]
%
%      Similarly, a query
%            ?- question( Q, [what,does,hans,own,'?'] , [] ) .
%      should also return a result for Q that is unifyable with an element of the list
%      returned for S of the sentence/3 query (from 5.1) , i.e.,
%            Q = [hans,owns,_,_]
%
%      Finally, change (simplify!) the procedure getAnswer( +Q , +State , -Answer ) from the demo program
%      p5.10_price_dialog in such a way that your procedure getAnswer computes one by one all the answers
%      to your question Q for a given state, e.g. for
%         ?- Q = [hans,owns,_,_] ,
%            State = [[hans,owns,2,forks] , [hans,owns,1,plates]] ,
%            getAnswer(Q,State,Answer).
%      the expected answer is
%         Answer = [hans,owns,2,forks]   ;
%         Answer = [hans,owns,1,plate]
%      i.e., the answers shall be returned as lists of words and one by one.
%      Make sure that you get the answer [hans,owns,2,forks] if there are two forks,
%      but get the answer  [hans,owns,1,plate], i.e. the singular form "plate", if there is just 1 plate.
%
%      Hints:
%      1. You can copy most of the program price_dialog.
%         Do NOT change the part of the program price_dialog already given here in program a5.pl, i.e.,
%         you only have to change move/3, setVal/3 and the grammar for sentences and questions, and
%         you should significantly simplify getAnswer/3 (to a clause with 2 goals only).
%
%      2. You may need in total two different grammar rules for transitive verbs,
%         one rule (from 5.1.) being used in sentences to read the 3rd person singular (owns),
%         and another rule being called in the grammar for queries to read the infinitive (own).
%
%      3. Furthermore, to keep your program readable, keep your additional program organized in 3 separate parts:
%          1. the grammar (for sentences and questions)
%          2. the lexicon
%          3. the interpreter changing the state (i.e. the procedures move/3 and setVal/3 )



% ----------------------------------------------------------------------------------

% 5.3. Extend your question answering system in such a way
%      that it can also handle synomyms ownership, i.e., owns = has = holds = possesses .
%      Your lexicon shall contain only 3 more facts (for has, holds, possesses).
%
%      First, treat these words as synonyms, i.e., a query
%            ?- sentence( S, [hans,has,1,plate,'.'] , [] ) .
%      should return
%            S = [ [hans,owns,1,plates] ]
%      i.e., it shall return the verb "owns", although the sentence stated "has".
%
%      To handle synonyms (e.g. owns=has=holds=posseses),
%      you shall extend the lexicon by an extra column (and by three more facts for the 3 new verbs),
%      i.e., avoid the definition of extra clauses or extra grammar rules.
%      As a consequence, in one of the grammar rules for questions,
%      you will have to adapt the call of the lexicon to 4 columns.


%      Second, make the answers "user friendly", i.e., if necessary, change the implementation of
%      your procedure getAnswer/3 in such a way that the dialog answers with the verb being used in the question.
%      For example, a query
%            ?- question( Q, [how,many,plates,does,hans,have,'?'] , [] ) .
%      should return
%            Q = [hans,has,_,plates]
%      However, a query
%            ?- question( Q, [how,many,plates,does,hans,hold,'?'] , [] ) .
%      should return
%            Q = [hans,holds,_,plates]
%
%      Similarly, a query
%            ?- question( Q, [what,does,hans,possess,'?'] , [] ) .
%      should return a result for Q that contains the word "possesses".
%
%      Hints: 1. You may need a third type of grammar rule for transitive verbs
%                that also reads the 3rd person singular (e.g. "has") of a verb,
%                but remembers the word used in the query (e.g. "has", instead of "owns")
%                in order to allow for user friendly answers.
%             2. Program p5.10_price_dialog.pl uses a technique to return the correct synonym


%      Third, add another question type asking for the owners, i.e.
%            ?- question( Q, [who,owns,exactly,4,forks,'?'] , [] ) .
%      should also return a result for Q that is unifyable with an element of the list
%      returned for S of the sentence/3 query (from 5.1) , i.e.,
%            Q = [_,owns,4,forks]
%
%      Finally, the question grammar shall accept only syntactically correct questions,
%      i.e., the expected answer for
%            ?- question( Q, [who,owns,exactly,1,forks,'?'] , [] ) .
%      is    false .
%
%
%      Further hint:
%          To keep your program readable, keep your additional program organized in 3 separate parts:
%          1. the grammar (for sentences and questions)
%          2. the interpreter, changing the state (i.e. the procedures move/3 and setVal/3 )
%          3. the lexicon



% 5.4   Extend your question answering system in such a way
%       that it also handles verbs that modify the number of possessed items,
%       i.e., add the 5 lexicon entries for obtains (= gains = finds)
%       and for looses (= destroys)
%
%       For example, the following dialog should be supported
%       (where we again assume that peter initially has 0 forks):
%       ?- dialog(_).
%       |: hans has 7 forks.
%       |: hans obtains 1 fork.
%       |: how many forks does hans hold?
%       hans holds 8 forks .
%       |: hans looses 3 forks.
%       |: how many forks does hans possess?
%       hans possesses 5 forks .
%       |: peter looses 3 forks.
%       |: how many forks does peter have?
%       peter has -3 forks .
%
%       Reuse the grammar rules for sentence,
%       i.e., do NOT implement a new grammar rule for sentence or for vp (verb phrase) or for tv (transitive verb).
%       (Depending on your previous implementation, it could be necessary to generalize the verb returned.)
%       Again, the grammar rule for sentence should return the synonym verb, such that e.g. a query
%            ?- sentence( S, [hans,finds,1,plate,'.'] , [] ) .
%       returns
%            S = [ [hans,obtains,1,plates] ]
%       and that e.g. a query
%            ?- sentence( S, [hans,destroys,2,plates,'.'] , [] ) .
%       returns
%            S = [ [hans,looses,2,plates] ]
%
%       In order to read the current number of forks, knives, and plates which somebody owns,
%       use a procedure getVal/2, similar to the one in program p5.9_global_state_list.pl, such that a call
%            ?- getVal( [ [anna,owns,2,plates], [anna,owns,1,forks] ] , [anna,_,N,Obj] ) .
%       returns
%            N = 2 , Obj = plates   ;    N = 1 , Obj = forks
%       and that e.g. a query
%            ?- getVal( [ [anna,owns,2,plates], [anna,owns,1,forks] ] , [anna,_,N,knives] ) .
%       returns
%            N = 0
%       In other words, for each person, the initial default amount of plates, knives, and forks is 0.
%       And if someone obtains or looses forks etc. and no previous knowledge is given,
%       we also assume that default amount of plates, knives, and forks is 0.
%
%       Extend your procedure move in such a way, that it can also interpret the new sentences, e.g.
%            ?- sentence( [ S ] , [hans,finds,1,plate,'.'] , [] ) ,
%               move( S,  [ [hans,owns,4,plates] ] , NewState ) .
%       shall return
%               NewState = [ [hans,owns,5,plates] ] .
%
%       Make sure that the sentences in NewState are sorted alphabetically (by using the built-in procedure sort/2),
%       as otherwise the tester5 throws an error message.
%
%       Finally, make sure that your program also interpretes nested sentences correctly, e.g.
%            ?- sentence( Ss , [hans,who,has,2,knives,finds,1,plate,'.'] , [] ) ,
%               moves( Ss,  [ [hans,owns,4,plates] ] , NewState ) .
%       shall return
%               NewState = [ [hans,owns,2,knives] , [hans,owns,5,plates] ] .

%
%       Hints:
%       1. To simplify the assignment, allow also a negative number of forks, etc., e.g. peter has -3 forks
%          as output, but assume that there is NEVER given a negative number as input (e.g. "anna finds -3 forks").
%
%       2. Let the lexicon treat e.g. obtains = gains = finds as synonyms
%
%       3. Implement move/3 for "finds" or "looses" by extending your implementation of move/3 for "owns"
%          by two similar rules.
%          The implementation of move/3 should need only 3 rules in total,
%          such that in total, you only should have 4 more rules and 5 more lexicon entries
%







% 5.5   Extend your question answering system in such a way
%       that it also handles transfer of ownership for verbs like
%           gives = grants  [ something to somebody ]     and
%           takes = gets    [ something from somebody ] ,
%       i.e., your lexicon shall only be extended by 4 facts for these 4 words
%
%       For example, the following dialog should be supported
%       (where we again assume that peter initially possesses 0 forks):
%       ?- dialog(_).
%       |: hans has 7 forks.
%       |: hans gives 1 fork to peter.
%       |: how many forks does hans hold?
%       hans holds 6 forks .
%       |: how many forks does peter possess?
%       peter possesses 1 fork .
%       |: anna gets 4 forks from hans.
%       |: how many forks does hans have?
%       hans has 2 forks .
%
%       Do NOT change the grammar rule for sentence, but write one additional grammar rule for vp (verb phrase).
%       Again, the grammar rule for sentence should return the synonym verb, such that e.g. a query
%            ?- sentence( S, [hans,grants,1,plate,to,anna,'.'] , [] ) .
%       returns
%            S = [ [hans,gives,1,plates,anna] ]
%       and that e.g. a query
%            ?- sentence( S, [hans,gets,2,forks,from,peter,'.'] , [] ) .
%       returns
%            S = [ [hans,takes,2,forks,peter] ]
%
%       Again, extend your procedure move in such a way, that it can also interprete the new sentences, e.g.
%            ?- sentence( [ S ] , [hans,gets,2,forks,from,peter,'.'] , [] ) , move( S,  [ [peter,owns,6,forks] ] , NewState ) .
%       shall return
%               NewState = [ [hans,owns,2,forks],[peter,owns,4,forks] ] .
%       Make sure that the sentences in NewState are sorted alphabetically (by using the built-in procedure sort/2),
%       as otherwise the tester5 throws an error message.
%
%       Finally, check whether you can also add information of nested sentences correctly, e.g.,
%            ?- sentence( Ss , [hans,gets,2,forks,from,peter,who,owns,5,forks,'.'] , [] ) ,
%               moves( Ss,  [ [peter,owns,2,plates] ] , NewState ) .
%       shall return
%               NewState = [ [hans,owns,2,forks],[peter,owns,2,plates],[peter,owns,3,forks] ] .
%
%       Hints:
%       1. Try to implement the move/3 rules for "gives" and for "takes" by
%          your implementation of the move/3 rules for "obtains" and for "looses" in order to save goals.
%
%       2. Now, the implementation of move/3 should need only 5 rules in total






% 5.6.  Extend your question answering system in such a way
%       that it also handles verbs of selling and buying correctly, i.e.
%           sells              [ something for X euros to somebody ]     and
%           buys = purchase    [ something for Y euros from somebody ]
%       i.e., your lexicon shall only be extended by 3 facts for these 3 words (sells,buys,purchases)
%
%       For example, the following dialog should be supported
%       (where we again assume that peter initially possess 0 euros):
%
%       ?- dialog(_).
%       |: hans has 7 plates.
%       |: hans sells 1 plate for 10 euros to peter.
%       |: what does hans hold?
%       hans holds 6 plates .
%       hans holds 10 euros .
%       |: anna buys one plate for 11 euros from peter.
%       |: what does peter possess?
%       peter possesses 1 euro .
%       peter possesses 0 plates .
%
%       Do NOT write an additional rule for sentence, write only one additional grammar rule for vp (the verb phrase).
%       Again, the grammar rules for vp and for sentence should return the synonym verb, such that e.g. a query
%            ?- sentence( S, [anna,purchases,1,plate,for,11,euros,from,peter,'.'] , [] ) .
%       returns
%            S = [ [anna,buys,1,plates,11,euros,peter] ]
%       Similarly, the query
%            ?- sentence( S, [anna,sells,1,plate,for,11,euros,to,peter,'.'] , [] ) .
%       should return
%            S = [ [anna,sells,1,plates,11,euros,peter] ]
%
%       (Ensure that you design the additional grammar rule for vp in such a way
%        that you finally get the same format for S as shown here because otherwise the tester5 might throw errors!)
%
%       Again, extend your procedure move/3 in such a way, that it can also interprete the new sentences, e.g.
%            ?- sentence( S, [anna,purchases,1,plate,for,11,euros,from,peter,'.'] , [] ) ,
%               moves( S,  [ [anna,owns,20,euros],[peter,owns,3,plates] ] , NewState ) .
%       shall return
%            NewState = [ [anna,owns,1,plates],[anna,owns,9,euros],[peter,owns,2,plates],[peter,owns,11,euros] ] .
%
%
%       Hints:
%       1. Try to implement the move/3 rules for "buys" and for "sells" by
%          your implementation of the move/3 rules for "gives" and for "takes" to keep the number of goals small.
%
%       2. Now, the implementation of move/3 should need only 7 rules in total
%




%=============================================================
% The following code is taken from the demo program p5.12_relative_clauses.pl and shall NOT be changed

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
   llout([A|As]) :- lout(A) , llout(As) .
%       lout([]) :- nl .
%       lout([K|R]) :- write(K) , write(' ') , lout(R) .


% do NOT change this procedure (sORq) - except for (de-)activating 3rd clause:
% process one syntence or question or syntax error
% sORq( +SentenceORQuestion , -Answers , +OldState, -NewState )
sORq( Sentence , [] , OldState , NewState ) :-
   sentence( S, Sentence, [] ) ,              % if you read a sentence,
 % % move( S, OldState, NewState ) .            % change the State accordingly
   moves( S, OldState, NewState ) .            % change the State accordingly

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


% ==== processsing a list of subsentences returned by the parser from one (nested or nonnested) sentence

   moves( [] , OldState, OldState ) .            % do NOT change this procedure
   moves( [S|Ss], OldState, NewState ) :-
      move( S , OldState, InterState ) ,
      moves( Ss , InterState, NewState ) .

% ==== End of the code that shall NOT be changed    ===================================================

% ==== start encoding your solution here :          ===================================================

% ==== the new procedure move/3 :                   ===================================================

% move( S, OldState, [S|OldState] ).            % currently only add knowledge to state
            
   % owns
   move( [ Subj, V, N, Obj] , OldState , NewState ) :- 
     lex(V, _, owns, verb), 
     setVal( [ Subj, owns, N, Obj], OldState, NewState ). 

   % Obtains
   move( [ Subj, V, N, Obj], OldState, NewState ):- 
     lex(V, _, obtains, verb),
     getVal(OldState, [Subj, _, A, Obj]),
     Temp #= N + A,
     setVal( [ Subj, owns, Temp, Obj], OldState, NewState ).

   % looses 
   move( [ Subj, V, N, Obj ], OldState, NewState ):-
     lex(V, _, looses, verb), 
     getVal(OldState, [Subj, _, A, Obj]),
     Temp #= A - N,
     setVal( [ Subj, owns, Temp, Obj], OldState, NewState ).

   % gives to
   move( [ Subj, V, N, Obj, Targ ], OldState, NewState ):-    
     lex(V, _, gives, verb),
     move( [Targ, obtains, N, Obj], OldState, Temp ),
     move( [Subj, looses, N, Obj], Temp, NewState ).
     
   % takes from
   move( [ Subj, V, N, Obj, Targ ], OldState, NewState ):- 
     lex(V, _, takes, verb),
     move( [Targ, looses, N, Obj], OldState, Temp ),
     move( [Subj, obtains, N, Obj], Temp, NewState ).

   % sells
   move( [ Subj, V, N, Obj, Price, Curr, Targ ], OldState, NewState ):-
      lex(V, _ , sells, verb),
      move([Subj, gives, N, Obj, Targ], OldState, Temp),
      move([Subj, takes, Price, Curr, Targ], Temp, NewState). 

   % Buys
   move( [ Subj, V, N, Obj, Price, Curr, Targ ], OldState, NewState ):-
      lex(V, _, buys, verb),
      move([Subj, takes, N, Obj, Targ], OldState, Temp),
      move([Subj, gives, Price, Curr, Targ], Temp, NewState).


   % For the rule move I create an oldState and transform to a newState.
   % I have created 7 move rules, one for each case.
   % Case 1: check if the verb is synonym of "owns" and returns a sentence with the same structure but with this verb.
   % Case 2: checks if the verb is synonym of obtains. Gets the old value and sums the old value and the new amount.
   % Case 3: the same as the previous case, but instead of obtain, it looks for looses. Again gets the value but this time substracts the new amount.
   % Case 4: make sure that the verb is "gives". Uses move/3 from case 2 (obtains) to give a target an object and case 1 (looses) to remove it from the subject.
   % Case 5: the opposite as before. Search for the verb "takes" and uses move/3 from cases 2 and 3.
   % Case 6: search for the synonym of "sells" and uses move/3 to give an object to a target and take money from the target.
   % Case 7: same as before but instead searchs for "buys" and again uses move/3 to take and to give.

   setVal( [Subj, V, N, ObjP] , OldState , NewState ) :-  
     mydeleteall( [Subj,V,_,ObjP] , OldState , InterState ) ,
     sort( [ [Subj,V,N,ObjP]  | InterState ] , NewState ) .  
   % Update from OldState to a newState with new information.
   % It removes all the previous information with the function mydeleteall from the old state and 
   % introduce the new information and sorts it alphabetically.

   mydeleteall( _ ,[] ,[]) .
   mydeleteall( X , [X|T] , T1):- mydeleteall( X , T , T1) .
   mydeleteall(X, [H|T], [H|Y]) :- X \= H , mydeleteall(X, T, Y) .
   % This function is the same as in exercise 2, which basically deletes one object from the list.

   getVal( State, Q ):-  member(Q, State). 
   getVal( State, [ Subj, owns, 0, Obj ] ):- not(member([Subj, _, _, Obj], State)).
   % Gets information that is stored in the databse.
   % I use member and not member to check if they are part of the question.
   % So basically searchs if the value that we want to know is part of our database.
   % If is not part, it returns 0 obj.

% ==== getAnswer/3 shall be a simplified version of what you find in program p5.10_price_dialog.pl ====

% get all Answer sentences found in State that are matching to Question
% all State information is stored in plural sentences. e.g. " [hans,owns,1,forks] "
% getAnswer(+Question,+State,-Answer).
   getAnswer( [ Subj, V, N, ObjP ], State, [ Subj, V, N, ObjSP ] ):-
     lex(V,_,Syn,verb),
     member([Subj, Syn, N, ObjP], State),
     singularOrPlural(N, ObjSP, ObjP) .

   % This rule returns all the answers that we ask in the question.e 
   % First I get the synonym of the verb to search in our database, as this verb is only stored by its synonym.
   % Then I check the question (with the verb in syn) is part of the State, using the function member
   % Finally I change the Answer from singular to plural.
  
% ====  the grammar for sentences and for questions ===================================================

   singularOrPlural( 1, S, P ):- lex(S, P, obj).
   singularOrPlural( N, P, P ):-  N #\= 1, lex( _, P, obj).
   % This function checks if it is 1 object or multiple objects.
   % In the case that it is one object, it changes the object from singular to plural.
   % In the case that is multiple objects, it just returns the object in plural.

   sentence( S ) --> np( Subj , C1 ) , vp( VP, C2 ) , { append( [ C1 , C2 , [ [Subj|VP] ] ] , S ) } , ['.'] .
   % This DCG rule recieves a sentence and transform according to the following rules: 
   % Np checks the subject of the sentence.
   % Vp corresponds to the rest of the sentence.

   vp([VSyn, N, ObjP], []) -->  
      [Verb], { lex(Verb, _, VSyn, verb) }, 
      [N, Obj], { singularOrPlural(N, Obj, ObjP) } .
   % This rule checks the structure of the sentence: Verb, Noun, Object.
   % For the verbs checks if the verb is in the lexicons and returns the Synonimum.


   vp( [VSyn, N, ObjP, Targ ], [] ) --> 
      [Verb], { lex(Verb, _ , VSyn, verb) },
      [N, Obj], { singularOrPlural(N, Obj, ObjP) },
      [_],
      np(Targ).
   % This Vp rule checks if the structure of the sentence is: verb, noun, object, target
   % So i have added 2 more clauses apart from the previous ones that checks verb noun and object.
   % This are a [_], that can be "from", "to" and the target that checks if the target is part of our lex.


   vp( [VSyn, N, ObjP, Price, euros, Targ ], [] ) --> 
      [Verb], { lex(Verb, _, VSyn, verb) },
      [N, Obj], { singularOrPlural(N, Obj, ObjP) },
      [for, Price, Curr], { singularOrPlural(Price, Curr, euros) },
      [_],
      np(Targ).

   % Finally the last vp checks the verb, noun, object, price in euros and target.
   % The rule is very similiar but I have to add a price in euros, and this price needs to check if is singular or 
   % plural as in the number-object.
   % The currency returns always euros.

   % np rules, as vp also has 2 rules
   np( Subj , RelClauses )  -->  [ Subj ]  ,  { lex(Subj,name) } , relClause(Subj,RelClauses) . 
   % This np checks the subject and the Relative Clause rule created bellow.
   np( Subj )  -->  [ Subj ]  ,  { lex(Subj,name) } .
   % The second np rule basically checks if the name is part of our lex.

   relClause( _ , [] )  --> [] .
   relClause( Subj, S )  --> [who] , vp( VPrel , RelClause ) , { append( RelClause , [ [Subj|VPrel] ] , S ) } .
   % As mentioned before, relClauses checks sentence contains a "who" and if that is true, then we append first the subject
   % and then the vp


   % Here are the grammar for questions. There is 3 cases:

   question( [ Subj, VerbS, _, ObjP ] ) --> 
      [how, many],
      [ObjP], { lex(_, ObjP, obj) }, 
      [does],
      np(Subj),
      [VerbI], { lex(VerbS, VerbI, _, verb) },
      ['?'].       
   % Case 1: consume how many object does Subj Verb.
   % The object is transformed from singular to plural.
   % The subject searchs in the lex if it exists.
   % The verb returns the infinitive for example owns --> own

   question( [ Subj, VerbS, _, _ ] ) --> 
      [what, does],
      np(Subj),
      [VerbI], { lex(VerbS, VerbI, _, verb) },
      ['?'].
   % Case 2: question of type what does Subj Verb
   % The subject searchs in the lex if it exists.
   % The verb again is in infinitive.

   question( [ _ , VerbS, N, ObjP]) -->
      [who, VerbS, exactly],
      [N, Obj], { singularOrPlural(N, Obj, ObjP) },
      ['?'] .
   % Case 3: question of type who verb exactly number object.
   % The verb is in singular and returns in singular.
   % The number and object returns in plural always


% ====  the lexicon                                 ===================================================

  % lex( name , kind )
  lex( hans , name ) .
  lex( anna , name ) .
  lex( rita , name ) .
  lex( peter, name ) .
  lex( bernd, name ) .


  % lex( sing,   plural , kind )
  lex( plate, plates, obj ).
  lex( fork,  forks,  obj ).
  lex( knife, knives, obj ).
  lex( euro,  euros,  obj ).    % Model euros as objects

  % lex( ThirdPersonSingular, Infinitive, Synonym, verb )
   lex( owns,      own,     owns,    verb ).
   lex( has,       have,    owns,    verb ).
   lex( holds,     hold,    owns,    verb ).
   lex( possesses, possess, owns,    verb ).
   lex( obtains,   obtain,  obtains, verb ).
   lex( finds,     find,    obtains, verb ).
   lex( gains,     gain,    obtains, verb ).
   lex( looses,    loose,   looses,  verb ).
   lex( destroys,  destroy, looses,  verb ).
   lex( gives,     give,    gives,   verb ).
   lex( grants,    grant,   gives,   verb ).
   lex( takes,     take,    takes,   verb ).
   lex( gets,      get,     takes,   verb ).
   lex( sells,     sell,    sells,   verb ).
   lex( buys,      buy,     buys,    verb ).
   lex( purchases, purchase, buys,   verb ).


% ====  Your comments on the extra questions:       ===================================================
% Definite clause grammar is a dialog program language that uses rules such as sentence and 
% question to parse simple natural language in a logical program language like prolog.
% They represent a grammar as a set of definite clauses in first order logic. 
% This assignments helps to understand how to interact with this rules and describe or ask
% for a ownership relation between persons and objects.
% The facts such as owner, number of objects, type of objects and target are stored in a database
% that can be access with the function getValue and setValue to set the amount in the database.
% The rule move changes the state from an old state to a new state by increasing/decreasing/trasnfering
% or changing the verb with a synonym. 
% A strenght of this program is that adding different kind of objects is very simple as well as storing them
% because one only has to store them in a lexicom. For example adding new names like lex(rita, name) is very
% easy, or new verbs like lex(eat, eats, eats, verb).
% However, the sentence structures needs to be very specific and we need to create sometimes too many rules 
% to express something. The structure sentence needs to have the same order otherwise it is not detected by
% the rule. For example "anna has 2 forks and 2 knifes" can not be detected, only "anna has 2 forks" and
% another rule "anna has 2 knifes" and is harder than adding lexicons.