type qnum = int * int;

exception QDiv;

fun mkQ (_,0) = raise QDiv
  | mkQ pr    = pr: qnum; 

infix 6 ++   
infix 6 --   
infix 7 **   
infix 7 //   
infix 4 ==   

fun (a,b) ++ (c,d) = (a*d + b*c, b*d);
fun (a,b) -- (c,d) = (a*d - b*c, b*d);            
fun (a,b) ** (c,d) = (a*c, b*d);
fun (a,b) // (c,d) = (a,b) ** mkQ(d,c);
fun (a,b) == (c,d) = (a*d = b*c);

fun gcd(0,n) = n
  | gcd(m,n) = gcd(n mod m,m);

fun toString(p,q) = 
   let val sign = if p*q<0 then "\verb/~/" else ""
       val ap = abs p
       val aq = abs q
       val d  = gcd(ap,aq)
   in sign ^ (Int.toString(ap div d)) 
           ^ "/" ^ (Int.toString(aq div d))
   end;

type qnum = int*int;  (* (a,b) where b > 0 and gcd(a,b) = 1 *) 

fun canc(p,q) =
   let val sign = if p*q < 0 then ~1 else 1
       val ap = abs p
       val aq = abs q
       val d  = gcd(ap,aq)
   in (sign * (ap div d), aq div d) 
   end;              

exception QDiv;

fun mkQ (_,0) = raise QDiv
  | mkQ pr    = canc pr;

infix 6 ++ --  (* addition and subtraction of rational numbers    *)
infix 7 ** //  (* multiplication and division of rational numbers *)
infix 4 ==     (* equality of rational numbers                    *)

fun (a,b) ++ (c,d) = canc(a*d + b*c, b*d);
fun (a,b) -- (c,d) = canc(a*d - b*c, b*d);
fun (a,b) ** (c,d) = canc(a*c, b*d);
fun (a,b) // (c,d) = (a,b) ** mkQ(d,c);
fun (a,b) == (c,d) = (a,b) = (c,d);
fun toString(p:int,q:int) = (Int.toString p) ^ "/" ^ (Int.toString q);
