fun circleArea r = Math.pi * r * r; 

circleArea 1.0; 

circleArea (2.0); 

fun fact 0 = 1 
  | fact n = n * fact(n-1);

fun power(x, 0) = 1.0                (* 1 *)
  | power(x, n) = x * power(x,n-1)   (* 2 *);

val a = (2.0, 3);

power a;

power(4.0, 2);

