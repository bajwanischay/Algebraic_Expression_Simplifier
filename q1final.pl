/**
Solution to Question 1 - Logic in Computer Science Assignment
BY: Aayushmaan Jain (2015A7PS043P) and Nischay Singh (2015A7PS059P)
*/

% 	*****GRAMMAR*****

simplifymain(Inexp,Outexp):-
	simplifyDiff(Inexp,Res1),simplify(Res1,Res),Outexp=Res.
simplifyDiff(Diffexp,Res) :-
	(A-B=Diffexp) -> (simplifyDiff(A,C),simplifySum(B,D),simplify(C-D,E),Res=E);(simplifySum(Diffexp,C),Res=C).
simplifySum(SumExp,Res) :-
	(A+B=SumExp) -> (simplifySum(A,C),simplifyProd(B,D),simplify(C+D,E),Res=E);(simplifyProd(SumExp,C),Res=C).
simplifyProd(ProdExp,Res) :-
	(A*B=ProdExp) -> (simplifyProd(A,C),simplifyDiv(B,D),simplify(C*D,E),Res=E);(simplifyDiv(ProdExp,C),Res=C).
simplifyDiv(DivExp,Res) :-
	(A/B=DivExp) -> (simplifyDiv(A,C),simplifyLit(B,D),simplify(C/D,E),Res=E);(simplifyLit(DivExp,C),Res=C).
simplifyLit(LitExp,Res) :-
	atom(LitExp) -> Res = LitExp.
simplifyLit(LitExp,Res) :-
	integer(LitExp) -> Res = LitExp.
simplifyLit(LitExp,Res) :-	
	((A)=LitExp) -> simplifyDiff(A,B),Res=B.

%	*****BASIC*****

simplify(P,Q) :-
	atom(P) -> Q = P.
simplify(P,Q) :-
	integer(P) -> Q = P.

%	*****ADDITION*****

simplify(P+0,Q) :- Q = P.
simplify(0+P,Q) :- Q = P.
simplify(A*P+B*P,R) :-	
	(integer(A),integer(B) -> S is A+B, R = S*P).
simplify(P+A*P,R) :- 								
	integer(A) -> S is A+1, ((S==1) -> R=P;R = S*P).
simplify(A*P+P,R) :-
	integer(A) -> S is A+1, ((S==1) -> R=P;R = S*P).
%simplify(A*P+X+C*P) :-	(integer(A),integer(C)) -> S is A+P, Q= S*P+X.
simplify(P+X-P,Q) :- Q = X.
simplify(P-X-P,Q) :- Q = -X.
simplify(P+X+P,Q) :- Q = 2*P+X.
simplify(P-X+P,Q) :- Q = 2*P-X.

simplify(P+X+A*P,Q) :-
	integer(A) -> S is A+1,Q=S*P+X.
simplify(P-X+A*P,Q) :-
	integer(A) -> S is A+1,Q=S*P-X.
simplify(P+X-A*P,Q) :-
	integer(A) -> S is 1-A,Q=S*P+X.
simplify(P-X-A*P,Q) :-
	integer(A) -> S is 1-A,Q=S*P-X.

simplify(A*P+X+B*P,Q) :-
	(integer(A),integer(B)) -> S is A+B, Q = S*P+X.
simplify(A*P-X+B*P,Q) :-
	(integer(A),integer(B)) -> S is A+B, Q = S*P-X.
simplify(A*P+X-B*P,Q) :-
	(integer(A),integer(B)) -> S is A-B, Q = S*P+X.
simplify(A*P-X-B*P,Q) :-
	(integer(A),integer(B)) -> S is A-B, Q = S*P+X.

simplify(P+Q,R) :- 	
	(P==Q) -> 
		(integer(P)-> (R is 2*P);R=2*P);							%check
	\+ (P==Q) ->
		((integer(P),integer(Q)) -> (R is P+Q); (integer(P),atom(Q))-> R=Q+P;(atom(P),integer(Q))->R=P+Q;R=P+Q).
		

%	*****SUBSTRACTION*****

simplify(P-0,Q) :- Q = P.
simplify(0-P,Q) :- Q = -P.
simplify(A*P-B*P,R) :-	
	(integer(A),integer(B) -> S is A-B, R = S*P).
simplify(P-A*P,R) :- 								
	integer(A) -> S is 1-A, ((S==1) -> R=P;R = S*P).
simplify(A*P-P,R) :-
	integer(A) -> S is A-1, ((S==1) -> R=P;R = S*P).
simplify(P-Q,R) :- 	
	(P==Q) -> R= 0;
	\+ (P==Q) ->	
		((integer(P),integer(Q)) -> (R is P-Q); (atom(P),integer(Q))->R=P-Q;R=P-Q).	

%	*****MULTIPLICATION*****

simplify(1*P,Q) :- Q=P.
simplify(P*1,Q) :- Q=P.
%simplify(1*1,Q) :- Q=1.
simplify(0*P,Q) :- Q=0.
simplify(P*0,Q) :- Q=0.
%simplify(A*B,Q) :-	integer(A),integer(B),Q is A*B.
simplify(A*(B*P),Q) :- 
	((integer(A),integer(B),\+ integer(P),\+ (A==1)) -> S is A*B, Q = S*P).  
simplify(A*(B+C),Q) :- simplify(A*B + A*C,R), Q=R.
simplify((B+C)*A,Q) :- simplify(A*B + A*C,R), Q=R.
simplify(P*Q,R) :- 	
	(P==Q) -> 																	
		(integer(P)-> (R is P*P);R=P^2);			%Do More 					
	\+ (P==Q) ->	
	 	((integer(P),integer(Q)) -> (R is P*Q); (integer(P),atom(Q))-> R=P*Q;(atom(P),integer(Q))->R=Q*P;R=P*Q).

%	*****DIVISION*****

simplify(P/1,Q) :- Q=P.
simplify(A*P/P,Q) :-
	((integer(A),\+ integer(P)) -> Q = A).
simplify(A*B/C,Q) :-
		((integer(A),integer(C),\+ integer(B)) -> S is A/C, Q = S*B).
simplify(P/Q,R) :- 	
	(P==Q) -> R=1;					
	\+ (P==Q) ->	
	 	((integer(P),integer(Q)) -> (R is P/Q);R=P/Q).

%	*****EQUALITY*****
is_equal(P,Q,Equal) :- simplifymain(P,P1),simplifymain(Q,Q1),((P1==Q1)->Equal=true;Equal=false),!.
