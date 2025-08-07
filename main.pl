main :- %time(lc_test(A,B)).
%time((lc_test1(1,A),sleep(2.0),lc_test1(2,A1),sleep(2.0),lc_test1(3,B)
%)),!.

time((lc_test1(8,A),sleep(2.0),lc_test1(9,B))),!.
%time((lc_test1(9,A))),!.
