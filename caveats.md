# Lucian CI/CD Caveats

When writing tests, it is better to use different variable names for each test, for example:

```
% c(1,A).
% A=1.
c(0,1):-!.
c(A,1):-A1 is A-1,c(A1,1).
% d(1,B).
% B=2.
d(0,2):-!.
d(A,2):-A1 is A-1,d(A1,2).
```
