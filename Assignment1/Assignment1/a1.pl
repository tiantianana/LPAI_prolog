 % NOTE: There is a change in subtask 1.19 in order to avoid an ambiguity!
 %========================================================================
 %  Assignment 1:

 %  You should work using 2 open windows:
 %  1. SWI-Prolog-Editor (for this file), i.e.
 %     your queries q1,...,q20 shall be encoded in this file, a1.pl
 %
 %  2. Switch to the SWI-Prolog-Interpreter to test your queries.
 %
 %
 %  You shall encode test queries q1,...,q24 (and the database states for q25,q26,27) in THIS file a1.pl !

 %========================================================================
 % The following is only one possible database state.
 % Of course, your queries shall work correctly also for other database states.
 % To test your queries, you shall modify the database state.


 % We assume that Player is the key of the footballer relation,
 % such that the pair (Country,Born) is unqiue for each Player

 % footballer( Player   , Country , Born )
		footballer(anton,germany,2001) .
		footballer(bert,germany,2001) .
		footballer(chris,poland,2001) .
		footballer(dirk,germany,2001) .
		footballer(emil,poland,2000) .
		footballer(fred,poland,2002) .
 % ...

 % We assume that Clubname is the key of the club relation,
 % i.e., the Funded is unique for each clubname

 % club( Clubname   , Funded ).
   club( hannover96 , 1896   ).
   club( hoffenheim , 1899   ).
   club( schalke04  , 1904   ).
 % ...

 % The relation joined stores which footballer played for which club how many seasons,
 % such that (Clubname, Player) is a key of the joined relation,
 % i.e., Seasons is unique for each pair of (Clubname,Player).
 % There is no entry in the joined relation for a pair of (Clubname,Player),
 % where the Player did not join the Clubname at all.

 % joined( Clubname   , Player , Seasons )
            joined(hoffenheim,anton,3) .
			joined(hoffenheim,bert,2) .
			joined(muenchen,bert,7) .
			joined(hoffenheim,chris,3) .
			joined(muenchen1860,dirk,5) .
			joined(hoffenheim,emil,4) .
 % ...


 % 1.1. Show the players stored in the footballer relation that are born in 1987 or before
 %      The desired solution is e.g.:
           q1( Player ) :-footballer( Player , _ , B ) , B =< 1987 .
 %      or is
 %          q1( Player ) :- footballer( Player , _ , B ) , \+ B > 1987 .
 %
 % Remove the comment symbol '%' before 'q1' in the line above, save the edited file,q
 % and compile the program, e.g. by using [Compile] -> [Compile buffer] in the Editor.
 %
 % Then, in the Prolog interpreter type: "?- q1( Player ) ." and [RETURN].
 % Use the ';' to see all answers.

 % To double-check that the solution is correct, you can also run the shell command:
 % $PathToYourPrologFile> ./tester1 a1 1
 % Hereby, 'a1.pl' should be your filename and 1 is the number of the query q1 checked.



 % Now start with the exercises.
 % First, program a rule "q2(Player,Clubname):- ... . " in the editor,



 % save the file, compile the file, and test your rule q2, by typing
 %    "?- q2( Player,Clubname ) ."  and  [RETURN].
 % Again, use the ';' to see all answers.
 % If the answers are not correct, modify your rule q2 in the Editor, compile again, and test your rule q2 again.
 %
 % When your answers look correct (not earlier!), you should double-check and run the shell command:
 % $PathToYourPrologFile> ./tester1 a1 2

 % 1.2. Show each pair of Player and Clubname of a club, where the club was funded in 1899
 %      and the player joined the club for 3 or more season
 %      i.e.   q2( Player, Clubname ) :- ...

			q2(Player, Clubname) :- club(Clubname, 1899), joined(Clubname, Player, S), S >= 3 .

			% I am using the generate and test approach as shown in Video 1.7 and 1.1.
			% Im using video 1.5 for the union between two subqueries: club with the variable clubname and the founded year as a constant.
			% The test checks that the Player have played 3 o more seasons.
			% This rule returns the Player and the Clubname that I have selected.


 % 1.3. Which clubs where joined by at least one footballer for exactly 5 seasons?
 %      Return the clubnames of these clubs.
 %      Make sure that the clubname of each such club is mentioned exactly once.
 %      Hint: use the generate and test technique

			q3(Clubname)  :- club(Clubname, _ ), q3temp(Clubname) .
			q3temp(Clubname) :- joined( Clubname, _, 5) , !.

			% First I am the relation club for selecting the clubname and _ as an anonymus variable because I dont need to know when the club was founded.
			% I am using video 1.6 Cut and the 4-port-mode for the second rule q3temp because I need to do a cut so it doesnt repeat all the time the same Clubs.
			% I am using the anonymus variable the second parameter of joined because I dont need to know the name of the player.
			% I am using 5 as the 3rd parameter of joined because I want to know what club has a footballer that joined exactly 5 seasons.
			% I am using video 1.5 for the union between club and joined.


 % 1.4. Which players joined multiple different clubs?
 %      Make sure that each such player is mentioned exactly once.

			q4(Player) :- footballer(Player, _ , _ ), q4temp( Player) .
			q4temp(Player) :- joined( C1, Player, _ ), joined( C2, Player, _), \+ C1 = C2, !.

			% I am using video 1.5 to make a union between footballer and q4temp.
			% In q4temp I am using the generate and test approach as shown in Video 1.7 as I want to get the multiple Players that joined different clubs.
			% In the query footballer I use only Player as a variable and the rest as anonymus variable becuase I dont need to know more information about the player such as where and when they borned.
			% In subqueries joined I use C1, C2 for clubnames, different variables because I want to make sure that they are not comparing to the same one all the time and Player as a unique variable because I want to check if the same player is in different clubs.
			% Also I use a cut at the end of q4temp because I dont want to repeat the footballers name and only show it once.

 % 1.5. Which players joined at most one club?
 %      Make sure that each such player is mentioned exactly once.

			q5(Player) :- footballer( Player, _ , _ ), \+ q5temp(Player).
			q5temp(Player) :- joined( C1, Player, _ ), joined( C2, Player, _), \+ C1 = C2, !.

			% I am using video 1.5 to make a difference between footballer and q4temp, because now I want to now all the players except those who are in a different club or in more than one club.
			% In q4temp I am using the generate and test approach as shown in Video 1.7 as I want to get the multiple Players that joined different clubs.
			% In the query footballer I use only Player as a variable and the rest as anonymus variable becuase I dont need to know more information about the player such as where and when they borned.
			% In subqueries joined I use C1, C2 for clubnames, different variables because I want to make sure that they are not comparing to the same one all the time and Player as a unique variable because I want to check if the same player is in different clubs.
			% Also I am using a cut at the end of q5temp because I dont want to repeat the footballers name and only show it once.

%1.6. Which footballers born in Germany joined at most one club?
%      Make sure that the name of each such footballer is mentioned exactly once.

			q6(Player) :- footballer( Player, germany , _ ), \+ q6temp(Player).
			q6temp(Player) :- joined( C1, Player, _ ), joined( C2, Player, _), \+ C1 = C2, !.

			% This example is the same as before but instead of using anonymus variable in the third parameter of footballer, I am using the constant germany because I only want to knwo the german players.
			% I am using video 1.5 to make a difference between footballer and q4temp, because now I want to now all the players except those who are in a different club or in more than one club.
			% In q4temp I am using the generate and test approach as shown in Video 1.7 as I want to get the multiple Players that joined different clubs.
			% In the query footballer I use only Player as a variable and the rest as anonymus variable becuase I dont need to know more information about the player such as where and when they borned.
			% In subqueries joined I use C1, C2 for clubnames, different variables because I want to make sure that they are not comparing to the same one all the time and Player as a unique variable because I want to check if the same player is in different clubs.
			% Also I use a cut at the end of q6temp because I dont want to repeat the footballers name and only show it once.

 % 1.7. Show the names of footballers born in Germany that joined at least 2 clubs for 3 or more seasons each
 %      Make sure that the name of each such footballer is mentioned exactly once.

			q7(Player) :- footballer( Player, germany , _ ), q7temp(Player).
			q7temp(Player) :- joined( C1, Player, S1 ), joined( C2, Player, S2), S1 >= 3, S2 >= 3, \+ C1 = C2, ! .

			% I am using video 1.5 for an union between footballer and q7temp.
			% In footballer I use the variable player and _ as anonymus variable as I dont need to know when they borned but the second parameter is a constant germany because I only want to select the footballers from germany.
			% In q7temp I am using video 1.7 to generate and test approach. I want to select those who joined a club for more than 3 seasons but I need to make sure that the clubs are not the same and that is why I make a difference (minus) those clubs that are the same.
			% Also I use a cut at the end of q7temp because I dont want to repeat the footballers name and only show it once.


 % 1.8. Show the names of footballers born in Germany that joined at most 1 club for 3 or more seasons
 %      (i.e. joined 0 clubs for 3 or more seasons or joined 1 club for 3 or more seasons).
 %      The answer set should include footballers not mentioned to join a club at all.
 %      Make sure that the name of each such footballer is mentioned exactly once.

			q8(Player) :- footballer( Player, germany , _ ), \+ q8temp(Player).
			q8temp(Player) :- joined( C1, Player, S1 ), joined( C2, Player, S2), S1 >= 3, S2 >= 3, \+ C1 = C2, ! .

			% This rule is similar to the previous one so I need to use the same videos (video 1.5, 1.6 and 1.7) but instead of using an union I need to use a difference.
			% I need to get the footballers that are from germany, using Player as a variable and _ and germany as a constant minus those who joined more than one club.
			% In q8temp is exacltly as q7temp because I want to select those Players that joined more than 1 club.
			% Again I am using a cut so the footballer is only shown once.

 % 1.9. For which footballers born in Germany there is a club not joined by that footballer?
 %      Make sure that the name of each such footballer is mentioned exactly once.

			q9(Player) :- footballer( Player, germany, _ ), q9temp( Player ) .
			q9temp(Player) :-  club(Clubname, _) , \+ joined( Clubname, Player, _ ), !.

			% In this rule I am using video 1.5 for an union between footballers and q9temp.
			% The footballers querie has Player as a variable, _ as an anonymus variable because I dont need to know where they borned and germany as a constant.
			% Rule q9temp is a difference between club with clubname as a variable and joined with Clubname and Player as a variable. Both have an anonymus variable.
			% For q9temp I need to use a cut so footballers is not mentioned multiple times as in video 1.6.

 % 1.10. Which footballers born in Germany have joined every club?
 %       Make sure that the name of each such footballer is mentioned exactly once.

			q10(Player) :- footballer( Player, germany, _ ), \+ q10temp( Player ) .
			q10temp(Player) :- club(Clubname, _) , \+ joined(Clubname, Player, _ ) .

			% In this example I am using again video 1.5 for a difference between footballer and q10temp.
			% In q10temp I use again another difference for club and joined because I want to know if in a club no one joined.
			% I am selecting all footballers that borned in germany so germany is a constant and removing those who didnt join all the clubs.


 % 1.11. For which footballer born in Germany there is a club that the footballer
 %       did not join for at least 3 seasons?
 %       Return the name of the footballer.
 %       Make sure that the season of each such footballer is mentioned exactly once.
 %       Also return names of footballers for which a club exists that the footballer did not join at all.

			q11(Player) :- footballer( Player, germany, _ ), q11temp(Player) .
			q11temp(Player) :- club(Clubname, _ ), joined( Clubname, Player, S), S =< 3, ! ; club(Clubname, _ ), \+ joined( Clubname, Player, _), ! .

			% I am using video 1.5 for an union between footballer and q11temp.
			% In q11 temp I am using an OR to select both cases in which there are clubs that footballers joined for less than 3 seasons or clubs in which no players joined at all.
			% I use germany again as a constant becuase I only want to do my selection between the players that borned in germany.
			% I need to use a cut so I dont repeat footballers.


 % 1.12. Which footballers born in Germany joined all existing clubs for at least 3 seasons?
 %       Return the name of the footballer.
 %       If no club is in the database table club, the name of each footballer born in Germany
 %       shall be in the answer set.
 %       Make sure that the name of each such footballer is mentioned exactly once.

			q12(Player) :- footballer( Player, germany, _ ), \+ q12temp( Player ).
			q12temp(Player) :- club(Clubname, _ ) , \+ q12_3seasons(Clubname, Player, _ ) .
			q12_3seasons(Clubname, Player, _) :- joined( Clubname, Player, S), S >= 3 .

			% I am using a difference between footballer and q12temp such as in video 1.5.
			% In q12temp again I need to use another difference between clubs and those that joined for more than 3 seasons.
			% In q12_3seasons I only select those who joined for more than 3 months, using Clubname and Player as variables and S also a variable in which I check that it is higher or equal than 3 seasons.
			

 % 1.13. For which footballers born in Germany, it holds that if they joined a club
 %       then they joined the club for at least for 3 seasons?
 %       Return the name of the footballer.
 %       The name of a footballer shall be in the answer set if and only if all facts in the joined table
 %       with his player name state that the player joined the club for 3 seasons or longer.
 %       If a footballer did not join at least one club at all, his name shall also be shown in the answer set.
 %       Make sure that the name of each returned footballer is mentioned exactly once.


			q13(Player) :- footballer(Player, germany, _) , \+ q13temp(Player) .
			q13temp(Player) :- joined( _ , Player, S), S < 3.

			% This rule is very simple. I use video 1.5 for a difference between footballers from germany and those who joined for less than 2 seasons.
			% In q13temp I use Player and S as a variable and Clubname as an anonymus variable. For S i need to check that is smaller than 3.

 % 1.14. For which club, there is at least one footballer born in Germany who
 %       joined this club for less than 3 seasons?
 %       Make sure that each such club is mentioned exactly once.

			q14(Clubname) :- club(Clubname, _ ), q14temp(Clubname).
			q14temp(Clubname) :- footballer( Player , germany, _ ), joined(Clubname, Player , S), S =< 3, ! .

			% Im using video 1.5 for a union between querie clubs and q14temp rule.
			% In q14 rule I use clubname as variable because I am only interested on the clubs name.
			% In q14temp I use a join between footballer with variables Player and anonymus Born and constant for germany.
			% I am selecting only those who have seasons less or equal than 3 and using the cut from video 1.6 to make sure I only mentioned the club once.



 % 1.15. For which club, all footballers born in Germany either joined this club at least 3 seasons or did not join the club at all?
 %       Make sure that each such club is mentioned exactly once.
 %       A clubname shall be in the answer if there is no German footballer that joined the club for less than 3 seasons.
 %       Make sure that each such club is mentioned exactly once.

			q15(Clubname) :- club(Clubname, _ ), \+ q15temp(Clubname) .
			q15temp(Clubname) :- footballer( Player, germany , _ ), joined( Clubname, Player, S), S < 3, !.

			% I am using a difference between club and q15temp as in video 1.5 for q15 rule.
			% In q15temp I use an union between footballers from germany and joined, but only those who joined for less than 3 seasons.
			% So basically I want to select all clubs and remove those who have joined for less than 3 seasons.
			% I need to highlight that I use the cut in q15temp to mention only once the club as in video 1.6, however in the examples of the teste1 there is no need at all.


 % 1.16. Which clubs have been joined by all footballers born in Germany for at least 3 seasons,
 %       i.e., the club has been joined by all footballers born in Germany,
 %       and all these joinings have been for 3 seasons or longer?
 %       If no footballer is born in Germany, each clubname shall be in the answer set.
 %       Make sure that each such clubname is mentioned exactly once.

			q16(Clubname) :- club(Clubname, _ ), \+ q16temp(Clubname) .
			q16temp(Clubname) :- footballer(Player , germany, _), \+ q16germany(Player, Clubname) .
			q16germany(Player, Clubname) :- footballer( Player, germany , _ ), joined( Clubname, Player, S), S >= 3 .

			% I am using a difference between club and q16temp as in video 1.5. 
			% In q16temp again we use a difference but this time with footballer from germany and q16germany.
			% q16germany is a union between footballers from germany and joined for more than 3 seasons. I am using Players, Clubname and S as a variable and _ for the year that they borned.
			% I am not using a cut in this rules because the rules already take one data, I could use it but it only generates more goals so I removed it.

 % 1.17. Which footballers joined the club hannover96 for the longest time,
 %       i.e., at least for so many seasons as other players joined hannover96?
 %       If no footballer joined hannover96, then NO Player shall be returned.
 %       Make sure that the name of each such footballer is mentioned exactly once.

			q17(Player) :- joined(hannover96, Player, S), \+ q17temp(S) .
			q17temp(S) :- joined(hannover96, _ , S2), S2 > S .

			% In this rule I am using a difference between joined and q17temp.
			% In joined I use the constant hannover96 because I am only interested in the clubs with that name and Players as a variable.
			% For q17temp I need to pass S as a parameter because I want to compare the players seasons with the rest of the players (different to them) seasons.
			% That is why in q17temp I use a comparasion of S2 and S, only accepting those who have been longer. And again only between those who joined hannover96.
			% The aim for this previous rule is to remove from all players that joined hannover96, those who have been less time in the team and then I can get the exact Player that joined for the longest time.

 % 1.18. When we consider only footballers who joined the club hannover96 for the longest time,
 %       i.e., at least for so many seasons as other players joined hannover96,
 %       which among these footballers has the youngest year of birth?
 %       If no footballer joined hannover96, then NO Player shall be returned.
 %       Make sure that the name of each such footballer is mentioned exactly once.

			q18(Player) :- footballer(Player, _ , B), q18temp(Player), \+ q18youngest(B) .
			q18youngest(B) :- q18temp(Player), footballer(Player, _ , B2), B2 > B .

			q18temp(Player) :- joined(hannover96, Player, S), \+ q18temp2(S) .
			q18temp2(S) :- joined(hannover96, _ , S2), S2 > S .


			% For this new rule I am using the same rule as before q18temp and q18temp2 are exacltly the same as q17 and q17temp, so I have to use the same videos.
			% In q18 I am using an union between footballers with known Player name, but this time I am not interested on the Country and B as a parameter to get the youngest, which means the one that its Born date is the highest and q18temp.
			% For the youngest rule if there is an older footballer than the one I have selected and if that is the case I removed it from my list till I only have one left.
			% I pass B as a parameter of youngest because I want to compare only Born dates, Im not interested in the countries.


 % 1.19. Return the names of those clubs with the longest "minimum join period",
 %       where the mininum join peroid of a club is the smallest number of seasons that a player joined this club.
 %
 %       Example:
 %       Assume that hannover96, schalke04, and hoffenheim are club names
 %             occuring in the club relation.                                                                        << ADDED
 %       Assume further that the relation joined contains only the following tuples:
 %         joined(hannover96,baumann,2)
 %         joined(hannover96,beck,4)
 %         joined(hannover96,volland,6)
 %         joined(schalke04,bruma,3)
 %         joined(schalke04,baumann,5)
 %         joined(augsburg,weber,6)                                                                                  << ADDED
 %       Then, the minimum join period of hannover96 is 2 seasons.
 %       Furthermore, the minimum join period of schalke04 is 3 seasons.
 %       Furthermore, hoffenheim does not have a minimum join period.
 %       Furthermore, augsburg is not a clubname occuring in the club relation and therefore should not be reported. << ADDED
 %       Therefore, the desired answer is "schalke04" only.

			q19(Player):- club(Clubname, _), q19temp(Clubname) .
			q19temp(Clubname):- q19temp2(Clubname, S), \+ q19mostseasons(S), !.
			q19mostseasons(S) :- q19temp2(Clubname, S2), S2 > S .
			q19temp2(Clubname, S1) :- club(Clubname, _) , joined(Clubname, _ , S1), \+ q19lessseasons(Clubname, S1) .
			q19lessseasons(Clubname, S1) :- joined(Clubname , _ , S2), S2 < S1 .

			% This rule was harder than the previous ones. I used more videos.
			% For the first part in which I make a union between club and q9temp, I used video 1.5
			% In rules q19lessseasons I only check what clubs have the players who played less seasons.
			% In rule q19temp I select all clubs in which players joined and remove those who played less seasons.
			% In q19 most seasons I check using the previous rule if that club is the one that has the highest seasons played but only from the one selected previously
			% Finally in q19 temp I get those who played the less seasons and removed the previous rule. 
			% I am using a cut in this rule because I dont want to select the same player multiple times.


 % 1.20. Which club was joined by at least one footballer from each country
 %       (regarding the countries listed in the relation footballer)?
 %       Make sure that you list each such club exactly once.
 %       If there are no footballers listed in the footballer relation, the result set should return all clubs.

			q20(Clubname):- club(Clubname, _ ), \+ q20temp(Clubname) .
			q20temp(Clubname):- footballer( _ ,C, _ ), \+ q20none(Clubname, C).
			q20none(Clubname, C):- footballer( Player, C, _ ), joined(Clubname, Player , _ ).

			% This rule is a bit simplier than the previous one because I only need to check if there is one player that did not joined a club.
			% For this rule I use all clubs and remove q9temp.
			% I need to use q20none becuase the questions sais that if there are no footballers that joined that club it has to shown up in the result.
			% In q20temp I am only removing those footballers that did not join any Club at all. 

 % 1.21 Which club was joined by each footballer from at least one country found in the footballer relation?
 %      That is, for which club there is at least one country,
 %      such that each footballer born in this country has joined the club?
 %      Make sure that you list the clubname of each such club exactly once.
 %      If there are no footballers listed in the footballer relation, the result set should be empty.

			q21(Clubname):- club(Clubname, _ ), q21temp(Clubname) .
			q21temp(Clubname):- footballer( _ , Country, _ ), joined(Clubname, _ , _ ), \+ q21none(Clubname, Country), ! .
			q21none(Clubname, Country):- footballer(Player, Country, _ ), \+ joined(Clubname, Player, _ ) .

			% For rule q21 I am using again union for club and q21temp.
			% The rule q21temp checks if all the players that joined a club and removes those who didnt.
			% In q21none I select all footballers that did not join any club
			% I need to use a cut in q21temp because I dont want to repeat any Clubnames, according to video 1.6.

 % 1.22  List the names of the footballers, who joined all their clubs for the same number of seasons,
 %       i.e., those footballers for which the number of joined seasons is unique for all the clubs they joined.
 %       If a footballer joined less than two clubs, this footballer should be included in the answer set.

		    q22(Player) :-  footballer(Player, _ , _ ), \+ q22equal(Player) .
		    q22equal(Player) :- joined( _ , Player, S) , joined( _ , Player , S2), S2 \= S.

			% This rule is very short, I only need to do a difference, selecting footballers and removing those footballers that have the same numbers of seasons.
			% I use the the operator "\=" to check if the players joined the same number of seasons with a union.
			% I only use video 1.5 for the difference and video 1.7 for testing and comparing.


 % 1.23  When considering only the youngest footballers of all footballers
 %       and further whether or not and how long they played for hoffenheim,
 %       who among the youngest footballers had the highest number seasons with hoffenheim?
 %       List the footballer's names.
 %       If none of the youngest footballers joined hoffenheim, list no footballer's name.

		    q23(Player):- joined(hoffenheim, Player, S), q23youngest(Player), \+ q23temp(S).
			q23temp(S1):- joined(hoffenheim, Player, S2), S2 > S1, q23youngest(Player) .
			q23youngest(Player):- footballer(Player, _ , B ), \+ q23someoneolder(B).
			q23someoneolder(B1):- footballer( _ , _ , B2 ), B2 > B1.

			% For rule q23 I have used a union with rule q23youngest and q23temp. 
			% q23temp checks if the players that joined have joined for the longest time and they are the youngest.
			% I want to check all footballers and remove those who are older. To get the youngest player I only check if the year of borned is higher.
			% I am using hoffenheim as a constant because I only want to know the footballers that are from that club.

 % 1.24  When considering only the youngest footballers of all footballers who joined hoffenheim,
 %       and further considering how long these footballers joined hoffenheim,
 %       who among them had the highest number seasons with hoffenheim?
 %       List the footballer's names.
 %       If no footballers joined hoffenheim, list no footballer's name.

		    q24(Player) :- q24youngest(Player, S), \+ q24temp(S) .
			q24temp(S1):- q24youngest(Player, S2), S1 < S2 .
			q24youngest(Player, S):-  joined(hoffenheim, Player, S ), footballer(Player, _ , B), \+ q24someoneolder(B) .
			q24someoneolder(B) :- joined(hoffenheim, Player, _ ), footballer(Player, _ , B1), B < B1 .

			% This rule is similar to the previous one. The difference here is that instead of checking all the players, I only want to check those who joined hoffenheim.
			% So first I get all the youngest player from all the countries and I check if one player has played for less seasons than another one.
			% That is why I add a union of joined to the rule q24youngest and q24someoneolder.
			% Hoffenheim is again a constant because I am only instered in that club. 

 % >>> The last three questions are of a different type, i.e.,                         <<<
 % >>> you are requested to define a database state that fulfills a certain condition. <<<


 % 1.25. Find a database state that shows that
 %       the answer to q20 returns a result that is not contained in the answer to q21
 %
 %       Define the state as follows:
 %       state(25, DataBaseTuple_1).  % e.g. state(25, joined(hsv,freddy,3) ).
 %       state(25, DataBaseTuple_2).  % e.g. state(25, footballer(freddy,germany,1998) ).
 %       ...                            ...
 %       where DatabaseTuple_X is a tuple of one of the relations club/2, joined/3, footballer/3.

			state(25, club(Club1, 1999)) .
			state(25, club(Club2, 2000)) .
			state(25, footballer(Elena, China, 2002)) .
			state(25, footballer(Juan, China, 2001)) .
			state(25, footballer(Sepu, England, 1998)) .
			state(25, joined(Club1, Elena, 3)) .
			state(25, joined(Club2, Juan, 5)) .
			state(25, joined(Club2, Sepu, 1)) .


 % 1.26. Find a database state that shows that
 %       the answer to q21 returns a result that is not contained in the answer to q20

 %       Define the state as follows:
 %       state(26, DataBaseTuple_1).
 %       state(26, DataBaseTuple_2).
 %       ...
 %       where DatabaseTuple_X is a tuple of one of the relations club/2, joined/3, footballer/3.

			state(26, club(Club1, 1999)) .
			state(26, club(Club2, 2000)) .
			state(26, footballer(Elena, China, 2002)) .
			state(26, footballer(Juan, China, 2001)) .
			state(26, footballer(Sepu, England, 1998)) .
			state(26, joined(Club1, Elena, 3)) .
			state(26, joined(Club2, Juan, 5)) .
			state(26, joined(Club2, Sepu, 1)) .


 % 1.27. Find a single database state that shows both,
 %       the answer to q20 returns a result that is not contained in the answer to q21 AND
 %       the answer to q21 returns a result that is not contained in the answer to q20

 %       Define the state as follows:
 %       state(27, DataBaseTuple_1).
 %       state(27, DataBaseTuple_2).
 %       ...
 %       where DatabaseTuple_X is a tuple of one of the relations club/2, joined/3, footballer/3.

			state(27, club(Club1, 1999)) .
			state(27, club(Club2, 2000)) .
			state(27, footballer(Elena, China, 2002)) .
			state(27, footballer(Juan, China, 2001)) .
			state(27, footballer(Sepu, England, 1998)) .
			state(27, joined(Club1, Elena, 3)) .
			state(27, joined(Club2, Juan, 5)) .
			state(27, joined(Club2, Sepu, 1)) .
