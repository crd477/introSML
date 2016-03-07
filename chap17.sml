fun fact 0 = 1 
  | fact n = n * fact(n-1);

fun naive_rev [] = []
  | naive_rev (x::xs) = naive_rev xs @ [x];

fun itfact(0,m) = m
  | itfact(n,m) = itfact(n-1,n*m)

fun itrev([], ys)    = ys
  | itrev(x::xs, ys) = itrev(xs, x::ys)

fun itfact(n,m) = if n<>0 then itfact(n-1,n*m) else m;

fun itrev(xs,ys) = 
     if not (null xs) then itrev(tl xs, (hd xs)::ys) else ys;

fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib(n-1) + fib(n-2);

fun itfib(n,a,b) = if n <> 0 then itfib(n-1,a+b,a)
                             else a;

fun power(x, 0) = 1.0             
  | power(x, n) = x * power(x,n-1);

fun power2(x,0) = 1.0
  | power2(x,n) = 
           if n mod 2 = 1 then x*power2(x,n-1)
           else let 
                   val z = power2(x,n div 2)
                in
                   z * z
                end ;
