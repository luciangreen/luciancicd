main :- %time(lc_test(A,B)).

%time((lc_test1(8,A),sleep(1),lc_test1(9,B))),!.
time((lc_test1(4,A))),!.
