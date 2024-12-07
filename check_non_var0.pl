/*
lp2p1_a(A,B) :-
 sub_term_wa([v,_],A,A1),
 findall([A2,A3],(member([A2,A4],A1),
 ((not(A4=[v,'_']),
 A4=[v,A5],
 atom_concat('_',_,A5),
 get_cnv(A6),A3=[v,A6]
 )->true;A4=A3)),A7),
 %trace,
 foldr(put_sub_term_wa_ae,A7,A,A8),
 lp2p1([[[n, a], ":-", [A8]]],C),
 string_concat("a:-",B1,C),
 string_concat(B2,".\n",B1),
 foldr(string_concat,["(",B2,")"],B),
 %square_to_round([B2],B),
 !.
*/

no_vars(A16) :-

 foldr(string_concat,["a:-",A16,"."],C111),
 fastp2lp2(C111,C112),
 C112=[[[n, a], ":-", [[[n, _],V]]]],
 not(member([v,_],V)),!.

% (c(1,A),A=5)
% -> (c(1,A),not(var(A)),A=5)
check_non_var0(C1,C2) :-

 dynamic(cnvn/1),
 retractall(cnvn(_)),
 assertz(cnvn(1)),
 
 dynamic(cnv/1),
 retractall(cnv(_)),
 assertz(cnv([])),


 %term_to_atom(C1,C110),
 foldr(string_concat,["a:-",C1,"."],C111),
 fastp2lp2(C111,C112),
 C112=[[[n, a], ":-", C11]],
 
 % [[[n, c], [1, [v, '_449476'], [v, c]]], [[n, =], [[v, '_449476'], 1]], [[n, =], [[v, c], 2]]]
 
%trace,
 %square_to_round(C11,C1),
 C11=[C12|C13],
 
 (C13=[]->lp2p1_a(C12,C2);
 (
 
 %functor(C12,Name,Arity),
 %numbers(Arity,1,[],Ns),

 (C12=[Name]->
 C23=C12;
 (
 C12=[Name, Args],
 %length(Args,Arity),
 
 %functor(C1A,Name,Arity),
 %* needs to get from A not _
 findall(CNV41,(%member(N,Ns),arg(N,C12,Arg),%copy_term(Arg,Arg1),
 member(Arg,Args),
 (Arg=[v,_]->
 (get_cnv(CNV4),
 %arg(N,C1A,CNV4),
 cnv(CNV6),
 retractall(cnv(_)),
 %delete(CNV6,[Arg,CNV4],CNV61),
 append(CNV6,[[Arg,CNV4]],CNV7),
 assertz(cnv(CNV7)),
 CNV41=[v,CNV4]);
 CNV41=Arg)%arg(N,C1A,Arg))
 %cnv(CNV1),
 ),CC1),
 %foldr(append,CC1,CC2),
 %append(CC3,[_],CC2),
 %foldr(append,[[Name,"("],CC3,["),"]],CC31),
 %foldr(string_concat,CC31,CC4),
 CC4=[Name, CC1],
 %dynamic(cnv/1),
 %retractall(cnv(_)),
 %assertz(cnv([])),
 cnv(CNV9),
 %trace,
 
 /*findall([[n,not],[[[n,var],[[v,CNV8]]]]],(%member(N,Ns),arg(N,C12,Arg),%copy_term(Arg,Arg1),
 %var(Arg),%cnv(CNV1),
 member([Arg,CNV8],CNV9), %trace,
 not((%not(var(Arg)),
 % [[n, =], [[v, '_449476'], 1]]
 member([[n, =],[Arg,B2]],C13),B2=[v,_]
 %append(CNV1,[not(var(Arg))],CNV2)%retractall(cnv(_)),
 %assertz(cnv(CNV2))
 ))),CC5),
 %foldr(append,Ns1,CC5),
 %foldr(string_concat,Ns11,CC5),
*/
CC5=[],
 %cnv(CNV3),
 findall([[n, =],[[v,CNV10],Num1]],(%member(Arg,C13),%N,Ns),arg(N,C13,CNV11=Num),% 
 (member([[n, =],[CNV11,Num]],C13)->true;
 member([[n, equals4],[CNV11,Num]],C13)),
%copy_term(Arg,Arg1),
 %var(Arg),%cnv(CNV1),
 member([CNV11,CNV10],CNV9),
 
 
 ((Num=[v,A5],
 atom_concat('_',_,A5))->Num1=[v,'_'];Num1=Num)
 %append(CNV1,[not(var(Arg))],CNV2)%retractall(cnv(_)),
 %assertz(cnv(CNV2))
 ),CC7),
 %trace,
 %foldr(append,Ns2,CC7),
 %foldr(string_concat,Ns21,CC6),
 %string_concat(CC7,",",CC6),
 C21=[[CC4],CC5,CC7],
 foldr(append,C21,C23)
 ),

 lp2p1([[[n, a], ":-", C23]],C),
 string_concat("a:-",B1,C),
 string_concat(C2,".\n",B1)
 %foldr(string_concat,["(",C22,")"],C2)
 %square_to_round([C22],C2)
 ))),
 %square_to_round(C111,C2),
 !.
 
 /*
get_cnv(CNV42) :-
 cnvn(CNV4),
 CNV41 is CNV4+96,
 char_code(CNV42,CNV41),
 retractall(cnvn(_)),
 CNV5 is CNV4+1,
 assertz(cnvn(CNV5)).
*/