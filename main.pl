main :- %time(lc_test(A,B)).

time((lc_test1(8,A),sleep(2.0),lc_test1(9,B))),!.
%time((lc_test1(9,A))),!.
