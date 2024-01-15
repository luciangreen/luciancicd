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

% - reuse deps in tests - say this in lc verify.pl

lc_test(NTotal,Score) :- 
 findall(_,(lc_test0(_N,_At_start,_Start_files,_End_files)),B),length(B,NTotal),
 lc_test(0,NTotal,0,Score,[],List),
 findall(_,(member(L,List),writeln(L),nl),_),
 !.
lc_test(NTotal,NTotal,Score,Score,List,List) :- !.
lc_test(NTotal1,NTotal2,Score1,Score2,List1,List2) :-
	NTotal3 is NTotal1+1,
	%gh2tmp,
	lc_test0(NTotal3,At_start,Start_files,End_files),
	((luciancicd(At_start,Start_files,End_files)
	%writeln1([result1,Result1]),
	%Result=Result1	
	)->(Score3 is Score1+1,append(List1,[[lc_test,NTotal3,passed]],List3));(Score3=Score1,append(List1,[[lc_test,NTotal3,failed]],List3))),
	writeln0(""),
	 %tmp2gh,
	lc_test(NTotal3,NTotal2,Score3,Score2,List3,List2),!.

%% lc_test individual cases, Debug=trace=on or off, N=case number, Passed=output=result

lc_test1(N,Passed) :-
 %gh2tmp,
	lc_test0(N,At_start,Start_files,End_files),
	((luciancicd(At_start,Start_files,End_files)
	%writeln1([result1,Result1]),
	%Result=Result1
	)->(Passed=passed,writeln0([lc_test,N,passed]));(Passed=failed,writeln0([lc_test,N,failed]))),
	 %tmp2gh,
!.

lc_test0(1,true,
% Start_files
[["c/c.pl","%c(A).\n%A=1.\nc(1).\n%d(A).\n%A=1.\nd(A):-A=1."],
["c/main_file.txt","[[\"c.pl\",[[c,1],[d,1]]]]"]],
% End_files
[["c/c.pl","%c(A).\n%A=1.\nc(1).\n%d(A).\n%A=1.\nd(A):-A=1."],
["c/main_file.txt","[[\"c.pl\",[[c,1],[d,1]]]]"]]
).

lc_test0(2,true,
[["c/c.pl","%a([a,b,c],[],A).\n%A = [a, b, c].\na([],A,A):-!.\na(A,B,C):-A=[D|E],append(B,[D],F),a(E,F,C),!."],
["c/main_file.txt","[[\"c.pl\",[[a,3]]]]"]],
[["c/c.pl","%a([a,b,c],[],A).\n%A = [a, b, c].\na([],A,A):-!.\na(A,B,C):-A=[D|E],append(B,[D],F),a(E,F,C),!."],
["c/main_file.txt","[[\"c.pl\",[[a,3]]]]"]]
).

lc_test0(3,true,
[["c/c.pl","%a(A).\n%A=1.\na(1).\n%b(A).\n%A=1.\nb(1).\n%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"c.pl\",[[a,1],[b,1],[c,1]]]]"]],
[["c/c.pl","%a(A).\n%A=1.\na(1).\n%b(A).\n%A=1.\nb(1).\n%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"c.pl\",[[a,1],[b,1],[c,1]]]]"]]
).

lc_test0(4,true,
[["c/a.pl",":-include('b.pl').\n:-include('c.pl').\n%a(A).\n%A=1.\na(1)."],
["c/b.pl","%b(A).\n%A=1.\nb(1)."],
["c/c.pl","%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"a.pl\",[[a,1]]],[\"b.pl\",[[b,1]]],[\"c.pl\",[[c,1]]]]"]],
[["c/a.pl",":-include('b.pl').\n:-include('c.pl').\n%a(A).\n%A=1.\na(1)."],
["c/b.pl","%b(A).\n%A=1.\nb(1)."],
["c/c.pl","%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"a.pl\",[[a,1]]],[\"b.pl\",[[b,1]]],[\"c.pl\",[[c,1]]]]"]]
).

lc_test0(5,true,
[["c/a.pl",":-include('b.pl').\n:-include('c.pl').\n%a(A).\n%A=1.\na(1)."],
["c/b.pl","%b(A).\n%A=1.\nb(1)."],
["c/c.pl","%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"a.pl\",[[a,1]]],[\"b.pl\",[[b,1]]],[\"c.pl\",[[c,1]]]]"]],
[["c/a.pl",":-include('b.pl').\n:-include('c.pl').\n%a(A).\n%A=1.\na(1)."],
["c/b.pl","%b(A).\n%A=1.\nb(1)."],
["c/c.pl","%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"a.pl\",[[a,1]]],[\"b.pl\",[[b,1]]],[\"c.pl\",[[c,1]]]]"]]
).

/*
lc_test0(5,true,
[["d/d.pl",":-include('../e/e.pl').\n%d(A).\n%A=1.\nd(1)."],
["e/e.pl","%e(A).\n%A=1.\ne(A):-d(A)."],
["d/main_file.txt","[[\"d.pl\",[[e,1]]]]"],
["e/main_file.txt","[]"]],
[["d/d.pl",":-include('../e/e.pl').\n%d(A).\n%A=1.\nd(1)."],
["e/e.pl","%e(A).\n%A=1.\ne(A):-d(A)."],
["d/main_file.txt","[[\"d.pl\",[[e,1]]]]"],
["e/main_file.txt","[]"]]
).
*/
lc_test0(6,true,
[["c/c.pl","%a(0,A).\n%A=1.\n%a(1,A).\n%A=1.\na(1,1) :- !.\na(A,B):-b(A,B).\n%b(0,A).\n%A=1.\nb(A,B):-c(A,B).\n%c(0,A).\n%A=1.\nc(A,B):-C is A+1,a(C,B)."],
["c/main_file.txt","[[\"c.pl\",[[a,2]]]]"]],
[["c/c.pl","%a(0,A).\n%A=1.\n%a(1,A).\n%A=1.\na(1,1) :- !.\na(A,B):-b(A,B).\n%b(0,A).\n%A=1.\nb(A,B):-c(A,B).\n%c(0,A).\n%A=1.\nc(A,B):-C is A+1,a(C,B)."],
["c/main_file.txt","[[\"c.pl\",[[a,2]]]]"]]
).

lc_test0(7,true,
[["c/c.pl","%c(A).\n%A=1.\nc(1).\nc(2)."],
["c/main_file.txt","[[\"c.pl\",[[c,1]]]]"]],
[["c/c.pl","%c(A).\n%A=1.\nc(1)."],
["c/main_file.txt","[[\"c.pl\",[[c,1]]]]"]]
).

lc_test0(8,true,
[["c/c.pl","%c(A).\n%A=2.\nc(C):-A=1,B=1,C is A+B."],
["c/main_file.txt","[[\"c.pl\",[[c,1]]]]"]],
[["c/c.pl","%c(A).\n%A=2.\nc(C):-A=1,B=1,C is A+B."],
["c/main_file.txt","[[\"c.pl\",[[c,1]]]]"]]
).
/*
lc_test0(9,false,
[["c/c.pl","%c(A).\n%A=3.\nc(C):-A=2,B=2,C is A+B."],
["c/main_file.txt","[[\"c.pl\",[[c,1]]]]"]],
[["c/c.pl","%c(A).\n%A=3.\nc(C):-A=2,B=1,C is A+B."],
["c/main_file.txt","[[\"c.pl\",[[c,1]]]]"]]
).
*/
