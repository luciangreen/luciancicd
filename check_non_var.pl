% (c(1,A),A=5)
% -> (c(1,A),not(var(A)),A=5)
check_non_var(C1,C2) :-
%trace,
 dynamic(cnvn/1),
 retractall(cnvn(_)),
 assertz(cnvn(1)),
 
 dynamic(cnv/1),
 retractall(cnv(_)),
 assertz(cnv([])),
 square_to_round(C11,C1),
 C11=[C12|C13],
 
 functor(C12,Name,Arity),
 numbers(Arity,1,[],Ns),

 %functor(C1A,Name,Arity),
 %* needs to get from A not _
 findall([CNV4,","],(member(N,Ns),arg(N,C12,Arg),%copy_term(Arg,Arg1),
 (var(Arg)->
 (get_cnv(CNV4),
 %arg(N,C1A,CNV4),
 cnv(CNV6),
 retractall(cnv(_)),
 %delete(CNV6,[Arg,CNV4],CNV61),
 append(CNV6,[[Arg,CNV4]],CNV7),
 assertz(cnv(CNV7)));
 CNV4=Arg)%arg(N,C1A,Arg))
 %cnv(CNV1),
 ),CC1),
 flatten(CC1,CC2),
 append(CC3,[_],CC2),
 foldr(append,[[Name,"("],CC3,["),"]],CC31),
 foldr(string_concat,CC31,CC4),
 %dynamic(cnv/1),
 %retractall(cnv(_)),
 %assertz(cnv([])),
 cnv(CNV9),
 findall(["not(var(",CNV8,")),"],(%member(N,Ns),arg(N,C12,Arg),%copy_term(Arg,Arg1),
 %var(Arg),%cnv(CNV1),
 member([Arg,CNV8],CNV9)
 %append(CNV1,[not(var(Arg))],CNV2)%retractall(cnv(_)),
 %assertz(cnv(CNV2))
 ),Ns1),
 flatten(Ns1,Ns11),
 foldr(string_concat,Ns11,CC5),

 %cnv(CNV3),
 findall([CNV10,"=",Num,","],(member(N,Ns),arg(N,C13,CNV11=Num),%copy_term(Arg,Arg1),
 %var(Arg),%cnv(CNV1),
 member([CNV11,CNV10],CNV9)
 %append(CNV1,[not(var(Arg))],CNV2)%retractall(cnv(_)),
 %assertz(cnv(CNV2))
 ),Ns2),
 flatten(Ns2,Ns21),
 foldr(string_concat,Ns21,CC6),
 string_concat(CC7,",",CC6),
 foldr(string_concat,["(",CC4,CC5,CC7,")"],C2),
 %square_to_round(C111,C2),
 !.
 
get_cnv(CNV42) :-
 cnvn(CNV4),
 CNV41 is CNV4+64,
 char_code(CNV42,CNV41),
 retractall(cnvn(_)),
 CNV5 is CNV4+1,
 assertz(cnvn(CNV5)).
