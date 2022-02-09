
ordenar(L,LO):- trocar(L,L1),!,  ordenar(L1,LO).
ordenar(LO,LO).
trocar([X,Y|C],[Y,X|C]):- maior(X,Y).
trocar([Z|C],[Z|C1]):- trocar(C,C1).
maior(A,B):-A>B.

tiraUltimo([_],[]).
tiraUltimo([C|L],[C|LU]):- tiraUltimo(L,LU).

mei([M],M).
mei([P,U],M):- M is (P+U)/2.
mei([_|L],Mei):- tiraUltimo(L,LU), mei(LU,M1),Mei is M1.

meio(L,R):- ordenar(L,LO), mei(LO,R).
