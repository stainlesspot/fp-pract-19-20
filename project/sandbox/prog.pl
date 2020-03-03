nat(z).
nat(s(X)):-nat(X).

one(s(z)).
two(s(s(z))).
three(s(s(s(z)))).
four(s(s(s(s(z))))).
five(s(s(s(s(s(z)))))).
five(succ(succ(succ(succ(succ(zero)))))).

plus(z,Y,Y).
plus(s(X),Y,s(R)) :- plus(X,Y,R).

mult(z,Y,z).
mult(s(X),Y,R) :- mult(X,Y,P), plus(Y,P,R).

endless(s(X)):-endless(X).

person(me).

always(X).

likes(peter,S):-student_of(S,peter).
likes(peter,Y):-likes(peter,X),likes(X,Y).
likes(maria,paul).
student_of(S,T):-follows(S,C),teaches(T,C).
teaches(peter,ai_techniques).
follows(maria,ai_techniques).

student_of(X,T):-follows(X,C),teaches(T,C).
follows(paul,computer_science).
follows(paul,expert_systems).
follows(maria,ai_techniques).
teaches(adrian,expert_systems).
teaches(peter,ai_techniques).
teaches(peter,computer_science).     trailing_space(ws).

no_space(a).no_space(between(a)).no_space(face).
