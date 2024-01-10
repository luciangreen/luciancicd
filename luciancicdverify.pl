% move gh2 contents to tmp folder
% copy each test folder set to gh2 (strings in this file)
% run lc, test against right result
% move tmp back to gh2

% a command to delete lc/_x.txt before each test

% test each lc feature


%% lc_test(Total,Score).

%%:- use_module(library(time)).

%% Test cases, NTotal=output=total cases, Score=output=result

% * check if lc fails

lc_test(NTotal,Score) :- 
 findall(_,(lc_test0(_N,_At_start,_Start_files,_End_files)),B),length(B,NTotal),
 gh2tmp,
 lc_test(0,NTotal,0,Score,[],List),
 findall(_,(member(L,List),writeln(L),nl),_),
 tmp2gh,
 !.
lc_test(NTotal,NTotal,Score,Score,List,List) :- !.
lc_test(NTotal1,NTotal2,Score1,Score2,List1,List2) :-
	NTotal3 is NTotal1+1,
	lc_test0(NTotal3,At_start,Start_files,End_files),
	((luciancicd(At_start,Start_files,End_files)
	%writeln1([result1,Result1]),
	%Result=Result1	
	)->(Score3 is Score1+1,append(List1,[[lc_test,NTotal3,passed]],List3));(Score3=Score1,append(List1,[[lc_test,NTotal3,failed]],List3))),
	writeln0(""),
	lc_test(NTotal3,NTotal2,Score3,Score2,List3,List2),!.

%% lc_test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

lc_test1(N,Passed) :-
 gh2tmp,
	lc_test0(N,At_start,Start_files,End_files),
	((luciancicd(At_start,Start_files,End_files)
	%writeln1([result1,Result1]),
	%Result=Result1
	)->(Passed=passed,writeln0([lc_test,N,passed]));(Passed=failed,writeln0([lc_test,N,failed]))),
	 tmp2gh,
!.

lc_test0(1,true,
% Start_files
[["c/c.pl","%c(A).\n%A=1.\nc(1).\n%d(A).\n%A=1.\nd(A):-A=1."],
["c/main_file.txt","[[\"c.pl\",[[c,1],[d,1]]]]"]],
% End_files
[["c/c.pl","%c(A).\n%A=1.\nc(1).\n%d(A).\n%A=1.\nd(A):-A=1."],
["c/main_file.txt","[[\"c.pl\",[[c,1],[d,1]]]]"]]
).

