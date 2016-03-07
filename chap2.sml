fun gcd(0,n) = n
  | gcd(m,n) = gcd(n mod m,m);

gcd(12,27);

gcd(36, 116);

fun fct 0 = 1 
  | fct n = fct(n-1)*n;

fun nameAge(name,age) =   name ^ " is " 
                        ^ (Int.toString age) ^ " years old";

nameAge("Diana",15+4);

nameAge("Peter Olsson",25-5);

nameAge("Philip",1-4);

fun even n = n mod 2 = 0;

fun isLowerCaseLetter ch = #"a" <= ch andalso ch <= #"z";

fun adjString s = if even(size s) then s else " "^s;

adjString "123";

adjString "1234";

fun gcd(m,n) = if m=0 then n      
               else gcd(n mod m,m); 

fun square x = x * x

fun square (x:real) = x * x
