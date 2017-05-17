**Solution: Question 1 - Logic in Computer Science**
by - Aayushmaan Jain (2015A7PS043P)
     - Nischay Singh (2015A7PS053P)

Database name: q1final.pl
--------------------------------------------------------------------------------------------------------------

PREDICATES:

*To simplify an expression, use simplifymain(_,_) as explained below:

	?-simplifymain(Input,Output).

->the atomic entities should be given as lowercase letters.
->the last argument passed to simplifymain() has to be a variable where the final expression will be stored.

	?-simplifymain(1+10/(1*2)-5,X).	
//if an arithematic expression is given as input, it will evaluate the expression in accordance with the rule of BODMAS and left associativity.

	?-simplifymain(a+a,X).

	?-simplifymain(a+b,X).

	?-simplifymain(x+3*x+(y+y),Q).

	?-simplifymain(2*3*a,X).	
//please give all integer multiplicands prior to the atom. For eg: avoid (2*x*3). Rather, give it as (2*3*x).

	?-simplifymain(24*x/x,X).	//can handle this as well

	?-simplifymain(x-1*x+x,X)	//evaluates expression is accordance with the rule of BODMAS and left associative

	?-simplifymain(x+y-x,X)		//some cases of commutativity and associativity have also been modelled



*To check equality of two expressions, use is_equal(_,_,_) as explained below:

	?-is_equal(exp1,exp2,Equal).

For eg:
	?-is_equal(2*x,x+x,Equal).

->the last argument passed to is_equal() has to be a variable which will store either TRUE or FALSE depending on the whether the expressions are equivalent or not.

------------------------------------------------------------------------------------------------------------------
**Note to instructor**

#The program doesn't deal with all cases of commutativity and associativity. In some instances, it will simplify the expression but there might still scope for further simplification. That can be achieved by recursively calling simplifymain() until the expression can't be simplified further. 

#Because the program doesn't deal with all cases of commutativity and associativity, in case of multi-variate expressions, the program yields better results (i.e. more simplified expressions) if terms of the same variable are clubbed together in parenthesis.
For eg.
	?-simplifymain(x+3*x+(y+y),Q).

