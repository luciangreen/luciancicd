% O=cube,traverse1(O,A,B),object(O,_,A).


a_to_m2(N1,Functions1,Pred_numbers,
Ordered_pred_nums1) :-
%trace,
%find pred nums in sm (done)

% find occurrence of pred calls in preds regardless of clause, for finding modes in bottom-up order
% find order in terms of pred name, arity
% x modify find_pred_numbers

find_pred_numbers_dependencies(Functions1,[],Functions2a,Pred_numbers),
%N=0,
%member([N,P],Functions2a),
%delete(Functions2a,[N,P],F),

% order_preds_bottom_up1_post_order_dfs(_L1,[N],Functions2a,[N],Ordered_pred_nums0,[N]),
% Ordered_pred_nums0=[0, [1, 2, [loop, 1]]]
% In test 7, query box (predicate 0) and predicate 1 are called, where predicate 1 calls predicate 2 and itself.


%order_preds_bottom_up1_post_order_dfs(1,Functions2a,[],Ordered_pred_nums0),
%trace,

 find_deps(N1,Functions2a,Ordered_pred_nums1).


%dfs_post_order(N1,Functions2a,[],Ordered_pred_nums1,[],_All1).


%order_preds_bottom_up1_post_order_dfs(_L1,[N],Functions2a,[],Ordered_pred_nums01,[N]).



object(cube,[["yellow",["red","blue"]],["red",[]],["blue",[]],[0,["yellow","green"]],["green",["purple"]],["purple",[]]],
["red", "blue", "yellow", "purple", "green", 0]).

object(cube1,[[0,[0]]],
[0]).

object(cube2,[[0,["red"]],["red",[0]]],
[[loop1, [0, "red"]]]).

object(cube3,[["yellow",["red","blue"]],["red",[]],["blue",[]],[0,["yellow","green"]],["green",["purple"]],["purple",[0]]],
["red","blue","yellow",[loop1,[0,"green","purple"]]]).

object(cube4,[[0,["red","blue"]],["red",[0]],["blue",[0]]],
[[loop1, [0, "blue", "red"]]]).

object(cube5,[[0,["red","blue"]],["red",[0]],["blue",[]]],
["blue", [loop1, [0, "red"]]]).


object(cube6,[[0,["red","blue"]],["red",["orange"]],["orange",[0]],["blue",[]]],
["blue", [loop1, [0, "orange", "red"]]]).

object(cube7,[[0,["red"]],["red",["orange"]],["orange",[0]]],
[[loop1, [0, "orange", "red"]]]).


object(t15,[[0, [1]], [1, [3, 4]], [2, []], [3, [2]], [4, [2, 6, 7]], [5, []], [6, [5, 8, 9, 10, 11, 24]], [7, [6, 7, 8, 9, 10, 11]], [8, [16, 17]], [9, [13, 14]], [10, [19, 20]], [11, [3, 4]], [12, []], [13, [12, 21, 22]], [14, [13, 14]], [15, []], [16, [15, 23]], [17, [16, 17]], [18, []], [19, [18, 21, 22]], [20, [19, 20]], [21, [24]], [22, [24]], [23, [24]], [24, []]],

[18, 24, 22, 21, 19, 20, 10, 12, 13, 14, 9, 5, 23, 15, 16, 17, 8, [loop1, [6, 7, 11, 4]], 2, 3, 1, 0]
).

object(t151,[[0, [6, 7]], [6, []], [7, [6, 7]]],
[6,7,0]).

object(t152,[[0, [6, 7]], [6, []], [7, [6, 7,0]]],
[6, [loop1, [0, 7]]]).

object(t153,[[0, [6, 7]], [6, []], [7, [8, 7,0]],[8,[6]]],
%[6, [loop1, [0, 7]]]).
[6,8, [loop1, [0, 7]]]).

%traverse(cube,Items,A).
% L = ["red", "blue", "yellow", "purple", "green", "white"].

%/*


traverse%(%Object,
%Items11%,All1
%)
 :-
 findall(Items1,(object(Object,Items2,Items1),
 %writeln1(dfs_post_order("white",Items2,[],Items1,[],All1)),
 dfs_post_order0(0,Items2,[],Items12,[],_All1),
 (Items1=Items12->S=success;S=fail),
 nl,writeln1([Object,S,"\n",Items1,"\n",Items12])),Items11),
 writeln1(Items11).
 
traverse1(Object,Items1,All1) :-
 object(Object,Items2,_),
 dfs_post_order0(0,Items2,[],Items1,[],All1).

%*/

dfs_post_order0(Curr,Items_all,Items2,Items31,Items2_all1,Items3_all1) :-

dfs_post_order(Curr,Items_all,Items2,Items31,Items2_all1,Items3_all1)
/*,


list_to_set(Items32,Ordered_pred_nums14),
%reverse(Ordered_pred_nums12,Ordered_pred_nums14),
reverse(Ordered_pred_nums14,Ordered_pred_nums141),
remove_dups_from_loops(Ordered_pred_nums141,Ordered_pred_nums151),
reverse(Ordered_pred_nums151,Ordered_pred_nums15),
%trace,
findall(Ordered_pred_nums19,(member(Ordered_pred_nums16,Ordered_pred_nums15),
(Ordered_pred_nums16=[loop1,Ordered_pred_nums17]->(list_to_set(Ordered_pred_nums17,Ordered_pred_nums18),Ordered_pred_nums19=[loop1,Ordered_pred_nums18]);Ordered_pred_nums19=Ordered_pred_nums16
)),Ordered_pred_nums20),

%delete(Ordered_pred_nums20,loop,Ordered_pred_nums21),

 findall(E,(member(F,Ordered_pred_nums20),
 (F=[loop1,[A1]]->E=A1;E=F)),Items31),

!
*/
.
dfs_post_order(Curr,Items_all,Items2,Items31,Items2_all1,Items3_all1) :-
 member([Curr,Items],Items_all),
 %delete(Items_all,[Curr,Items],Items_all1),
 Items_all=Items_all1,
 dfs_post_order2(Curr,Items,Items_all1,[]
 ,Items6,Items2_all1,Items6_all1),
 
 %trace,
 (member(Curr,Items6_all1)->
 (Items6=Items3,
 Items6_all1=Items3_all1);
 (%trace,
 append(Items6,[Curr],Items3),
 append(Items6_all1,[Curr],Items3_all1))),
 
 %trace,
 findall(C,(member(C,Items3),not(C=[loop1,_])),A1),
 findall([loop1,A],member([loop1,A],Items3),A2),
 append(A1,A2,A3),
 append(Items2,A3,Items31),
 !.
dfs_post_order([],_,Items2,Items2,Items2_all1,Items2_all1) :- !.

dfs_post_order2(_,[],_,Items,Items,Items_all1,Items_all1) :- !.
dfs_post_order2(_,_,[],Items,Items,Items_all1,Items_all1) :- !.
dfs_post_order2(Curr,Item7,Items_all,Items2,Items8,Items2_all1,Items8_all1) :-
 Item7=[Item1|Items3],
 %if item1 has been covered, end
 (member(Item1,Items2_all1)->
 (Items2=Items8,
 Items2_all1=Items8_all1);

 (%trace,
 (contains_loop_dfs1(Curr,Item1,Items_all,[%Item1%Curr,
 %cycle(Item1, Cycles,Noncycles) :-

 ],Items41,[],Not_on_line)
 %,writeln([not_on_line,Not_on_line])
 )->
 (append(Items3,Not_on_line,Items31),
 %if loop item
 %dfs_post_order(Item1,Items_all,[],Items41,Items2_all1,Items4_all1),
 length(Items41,L),
 (L=<1->append(Items2,Items41,Items4);
 (
 %trace,
 
 sub_term_wa([loop1,_],Items2,A),
 findall(C,(member(C,A),C=[_Add,[loop1,B]],not(intersection(Items41,B)=[])),D),
 
 findall(F,member([_,[_,F]],D),G),
 append([Items41],G,J),
 foldr(append,J,J1),
 sort(J1,H),

 findall(K,member([K,_],D),M),
 delete_sub_term_wa(M, Items2, E),
 
 append(E,[[loop1,H
 ]],Items4)
 
 %*/
 )),
  %append(Items2,[[loop1,Items41
 %]],Items4)
 %),
 append(Items2_all1,Items41,Items43_all1
 ),
 sort(Items43_all1,Items4_all1)
 );
 %if non loop item
 (Items3=Items31,
 dfs_post_order(Item1,Items_all,Items2,Items4,Items2_all1,Items4_all1))),
 dfs_post_order2(Curr,Items31,Items_all,Items4,Items8,Items4_all1,Items8_all1)),!.

contains_loop_dfs1(Curr1,Curr,Items_all
,Items2_all1,Items61_all1,Not_on_line1,Not_on_line21) :-
contains_loop_dfs(Curr1,Curr,Items_all
,Items2_all1,Items6_all1,Not_on_line1,Not_on_line2),
%not
%trace,
subtract(Not_on_line2,Items6_all1,Not_on_line21),
subtract(Items6_all1,Not_on_line21,Items61_all1),
(member(Curr,Items61_all1)%->true;
%member(Curr,Items6_all1)
),!.

contains_loop_dfs(Curr1,Curr,Items_all
,Items2_all1,Items6_all1,Not_on_line1,Not_on_line2) :-
 member([Curr,Items],Items_all),

 (member(Curr,Items2_all1)->
 (Items2_all1=Items7_all1,
 Not_on_line1=Not_on_line3);
 %findall(Items7_all1,
 contains_loop_dfs2(Curr1,Items,Items_all%,Items2,Items6
 ,Items2_all1,Items7_all1,Not_on_line1,Not_on_line3)),
 %),Items71_all1),
 %trace,
 %/*
 
 %trace,
 %writeln(lead_to_empty_list(Items,Items_all,[],_)),
 (true%,lead_to_empty_list(Items,Items_all,[],_)
 ->(
 %writeln(lead_to_empty_list(Items,Items_all,[],_)),
 %trace,
 append([Curr|Items],Not_on_line3,Not_on_line2)
 %Items7_all1=Items6_all1
 );
 Not_on_line3=Not_on_line2),

 (member(Curr,Items7_all1)->
 Items7_all1=Items6_all1;
 append(Items7_all1,[Curr],Items6_all1))%);
 %Items2_all1=Items6_all1),
 %*/
 %flatten(Items6_all1,Items61_all1),
 %append(Items6_all1,[Curr],Items3_all1),
 %append(Items6,[Curr],Items3),
 .
%contains_loop_dfs([],_,Items2,Items2,Items2_all1,Items2_all1) :- !.

% collect loops, path in cont loop

% list all arms of loop

contains_loop_dfs2(_C,[],_%,Items,Items
,A,A,B,B%Items_all1,Items_all1
%
) :- %append(B1,[C],B2),
!%,fail
.
contains_loop_dfs2(Curr,Item7,Items_all%,Items2,Items8
,Items2_all1,Items8_all1,Not_on_line1,Not_on_line2
) :- 
%trace,
  (member(Curr,Item7)->%->fail;
  %Items2_all1=Items6_all1;%->true;member(Curr1,)
  %trace,
  %* find 8 etc with lead to empty list
 (member(Curr,Items2_all1)->
 (Items2_all1=Items8_all1,
 Not_on_line1=Not_on_line2
 );
 (append(Items2_all1,[Curr],Items8_all1),
 Not_on_line1=Not_on_line2));

%  !.
%contains_loop_dfs2(Curr,Item7,Items_all%,Items2,Items8
%,Items2_all1,Items8_all1
%) :- 
%writeln1(contains_loop_dfs2(Curr,Item7,Items_all%,Items2,Items8
%,Items2_all1,Items8_all1)),
 %findall([Items4_all1,Items3],(
 %member(Item1,Item7),%
 (
  %(intersection(Items0,[Curr],[])->

 
 Item7=[Item1|Items3],
 %trace,
 %(Item1=Item7->

 %(member(Item1,Items2_all1)->
 %Items2_all1=Items62_all1;
 %append(Items2_all1,[Item1],Items62_all1)); 
 
 %delete(Item7,Item1,Items3),
 %if item1 has been covered, end
 
 %(
 contains_loop_dfs(Curr,Item1,Items_all%,Items2,Items4
 ,Items2_all1
 ,Items6_all1,Not_on_line1,Not_on_line3),%->true;*)
 
 %append()%),Items5_all1),
 %findall(A,member([A,_],Items5_all1),Items51_all1), findall(A,member([_,A],Items5_all1),Items31),
 %flatten(Items31,Items32),
 %append(Items2_all1,Items4_all1,Items6_all1),%->true;
 flatten(Items6_all1,Items61_all1),
  %writeln(Items61_all1),
 ((%false,
 Items2_all1=Items61_all1
 %member(Item1,Items61_all1)%true%trace,
 %intersection(Item7,Items61_all1,[])
 )->
 
 %member(Item1,Items61_all1)->
 (Items61_all1=Items8_all1,
 Not_on_line3=Not_on_line2);
 %append(Items61_all1,[Item1],Items62_all1)),
 contains_loop_dfs2(Curr,Items3,Items_all%,Items4,Items8
 ,Items61_all1,Items8_all1,Not_on_line3,Not_on_line2))))%;
 
 %(%member(Items61_all1,Items61_all1)->
 %Items61_all1=Items8_all1
 %append([],%Items61_all1,
 %Item7,Items63_all1),
 %sort(Items63_all1,Items8_all1)
%))))
 .

lead_to_empty_list([],_,A,A) :- !.
lead_to_empty_list(Items,Items_all,A,B) :-
 Items=[Items1|Items2],
 (member(Items1,A)->fail;
 (append(A,[Items1],C),
 member([Items1,Items3],Items_all),
 lead_to_empty_list(Items3,Items_all,C,D),
 lead_to_empty_list(Items2,Items_all,D,B))). 


% cycles
path(Node, Node, Tree, Cycles1, A4) :-
    findall(Cycles2,path00(Node, Node, Tree, Cycles1, Cycles2),Cycles3),
    flatten(Cycles3,A3),list_to_set(A3,A4),
    not(A4=[]),!.


% Predicate to check if there's a path from Start to End
path00(Start, End, Tree, Visited, [End|Visited]) :-
    member([Start, Ends], Tree),
    member(End, Ends),
    \+ member(End, Visited).

path00(Start, End, Tree, Visited, Path) :-
    member([Start, Ends], Tree),
    member(Next, Ends),
    %edge(Start, Next),
    \+ member(Next, Visited),
    path00(Next, End, Tree, [Next|Visited], Path).

% Predicate to check if there's a cycle starting and ending at Node
cycle(Node, Tree,Cycles,Noncycles) :-
 	% cycles
 	%trace,
    path(Node, Node, Tree, [], Cycles),
    % non-cycles
    %trace,
	path1([Node],%Cycles,
	Node,
	Tree,[],A1),
	flatten(A1,A2),
	list_to_set(A2,A3),
	subtract(A3,Cycles,Noncycles).

% non-cycles
path1(Cycles,First,Tree,A0,A5) :-
%trace,
 findall(A1,path10(Cycles,First,Tree,A0,A1),A2),
 flatten(A2,A3),list_to_set(A3,A4),
 subtract(A4,[First],A5)
 %A4=A5
 .

%path10(Stop,First,_Tree,A,A) :-
 %member(First,Stop),
 %*append(A0,[First],A1),
 %!.
path10(_Stop,First,Tree,A0,A1) :-
 member([First,[]],Tree),
 %trace,
 %member(Second1,Second),
 append(A0,[First],A1).
 %path1(Second1,A,Tree,A2,A1).
path10(Stop,First,Tree,A0,A1) :-
 member([First,Second],Tree),
 member(Second1,Second),
 append(A0,[First],A2),
 append(Stop,[First],Stop1),
 (member(Second1,Stop)
 %(Second1=Stop
 ->A2=A1;
 path10(Stop1,Second1,Tree,A2,A1)).


find_deps(Nodes,Tree,Deps) :-
 findall(Deps2,(member(Node,Nodes),
 cycle1(Node,Tree,[],Deps2)),Deps1a),
 foldr(append,Deps1a,Deps1),
 
 
list_to_set(Deps1,Ordered_pred_nums14),
%reverse(Ordered_pred_nums12,Ordered_pred_nums14),
reverse(Ordered_pred_nums14,Ordered_pred_nums141),
remove_dups_from_loops(Ordered_pred_nums141,Ordered_pred_nums151),
reverse(Ordered_pred_nums151,Ordered_pred_nums15),
%trace,
findall(Ordered_pred_nums19,(member(Ordered_pred_nums16,Ordered_pred_nums15),
(Ordered_pred_nums16=[loop1,Ordered_pred_nums17]->(list_to_set(Ordered_pred_nums17,Ordered_pred_nums18),Ordered_pred_nums19=[loop1,Ordered_pred_nums18]);Ordered_pred_nums19=Ordered_pred_nums16
)),Ordered_pred_nums20),

%delete(Ordered_pred_nums20,loop,Ordered_pred_nums21),

 findall(E,(member(F,Ordered_pred_nums20),
 (F=[loop1,[A1]]->E=A1;E=F)),Deps2),
 delete(Deps2,[loop1,[]],Deps).

%cycle1(Node,Tree,Deps1,Deps2) :-
cycle1(Node,Tree,Deps1,Deps2) :-
 (cycle(Node, Tree,Cycles,Noncycles)->
 (%append(Deps1,[[loop1,Cycles]],Deps3),
 %findall(A,(member(A,Noncycles),
 cycle2(Noncycles,Tree,% Cycles1,Noncycles1,
 Deps1,Deps3),
 (Cycles=[A]->
 (member(A,Deps3)->Deps3=Deps2;
 append(Deps3,[A],Deps2));
 append(Deps3,[[loop1,Cycles]],Deps2)));
 (% find noncycles
 %trace,
 path1([Node],Node,Tree,[],Noncycles),
 %append(Deps1,[Node],Deps3),
 cycle2(Noncycles,Tree,% Cycles1,Noncycles1,
 Deps1,Deps3),
 (member(Node,Deps3)->
 Deps3=Deps2;
 append(Deps3,[Node],Deps2))
 )),!.

cycle2([], _,Deps,Deps) :-!.
cycle2(Noncycles, Tree,Deps1,Deps2) :-
 Noncycles=[A|B],
 cycle1(A,Tree,Deps1,Deps3),
 (member(A,Deps3)->Deps3=Deps31;
 append(Deps3,[A],Deps31)),
 cycle2(B, Tree,Deps31,Deps2).

test(N) :- 
 tests(Tests),
 member([N,Tree,Ans],Tests),
 find_deps(0,Tree,Deps),
 (Deps=Ans->S=success;S=fail),
 %nl,
writeln1([N,S,"\n",result,Deps,"\n",answer,Ans]),!.

tests([
 [1,[[0,[]]],
   [0] ],
 [2,[[0, [0]]],
   [0] ],
 [3,
 [["yellow",["red","blue"]],["red",[]],["blue",[]],[0,["yellow","green"]],["green",["purple"]],["purple",[]]],
["red", "blue", "yellow", "purple", "green", 0]
  ],
 [4,[[0,["red"]],["red",[0]]],
[[loop1, [0, "red"]]]],
 [5,[["yellow",["red","blue"]],["red",[]],["blue",[]],[0,["yellow","green"]],["green",["purple"]],["purple",[0]]],
 ["red","blue","yellow",[loop1,[0,"purple","green"]]]],
 
 [6,[[0,["red","blue"]],["red",[0]],["blue",[0]]],
[[loop1, [0,"red","blue"]]]],

 [7,[[0,["red","blue"]],["red",[0]],["blue",[]]],
["blue", [loop1, [0, "red"]]]],

 [8,[[0,["red","blue"]],["red",["orange"]],["orange",[0]],["blue",[]]],
 ["blue", [loop1, [0, "orange", "red"]]]],
 
[9,[[0, [1]], [1, [3, 4]], [2, []], [3, [2]], [4, [2, 6, 7]], [5, []], [6, [5, 8, 9, 10, 11, 24]], [7, [6, 7, 8, 9, 10, 11]], [8, [16, 17]], [9, [13, 14]], [10, [19, 20]], [11, [3, 4]], [12, []], [13, [12, 21, 22]], [14, [13, 14]], [15, []], [16, [15, 23]], [17, [16, 17]], [18, []], [19, [18, 21, 22]], [20, [19, 20]], [21, [24]], [22, [24]], [23, [24]], [24, []]],

[2,3,5,15,24,23,16,17,8,12,21,22,13,14,9,18,19,20,10,[loop1,[7,4,11,6]],1,0]],

[10,[[0, [6, 7]], [6, []], [7, [6, 7]]],
[6,7,0]],

[11,[[0, [6, 7]], [6, []], [7, [6, 7,0]]],
[6, [loop1, [0, 7]]]],

[12,[[0, [6, 7]], [6, []], [7, [8, 7,0]],[8,[6]]],
[6,8, [loop1, [0, 7]]]]]
).
tests :-
 tests(Tests),
 findall(_,(
 member([N,Tree,Ans],Tests),
 find_deps(0,Tree,Deps),
 (Deps=Ans->S=success;S=fail),
 nl, writeln1([N,S,"\n",result,Deps,"\n",answer,Ans])),_),!.



