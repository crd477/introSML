let  fun circleArea r = Math.pi * r * r in  circleArea  end;

val f = let fun circleArea r = Math.pi * r * r 
        in circleArea  
        end;

fn n => 2 * n;

(fn n => 2 * n) 3;

fn 0 => false | _ => true;

(fn 0 => false | _ => true) 0;

(fn 0 => false | _ => true) 7;

fn r => Math.pi * r * r;

(fn r => Math.pi * r * r)  2.0;

val circleArea = fn r => Math.pi * r * r;

fun fact 0 = 1
  | fact n = n * fact(n-1); 

val rec fact = fn 0 => 1
                | n => n * fact(n-1);

op +;

op+(2,3);

fun map f = fn []      => []
             | (x::xs) => f x :: map f xs;

val g = map Int.toString;

fun posList []      = []
  | posList (x::xs) = (x > 0)::posList xs;

val posList = map (fn x => x > 0);

fun addElems []          = []
  | addElems ((x,y)::zs) = (x + y)::addElems zs;

val addElems = map op+

fun map f []      = []
  | map f (x::xs) = f x :: map f xs;

fun exists p []      = false
  | exists p (x::xs) = p x orelse exists p xs;

exists (fn x => x>=2) [1,3,1,4];

infix mem;
fun x mem ys = exists (fn y => x=y) ys;

(* (2,3.0) mem [(2, 4.0), (3, 7.0)]; *)

(2,3) mem [(2,4),(3,7)];

"abc" mem ["", "a", "ab", "abc"];

fun all p []      = true
  | all p (x::xs) = p x andalso all p xs;

all (fn x => x>=2) [1,3,1,4];

fun find p []      = NONE
  | find p (x::xs) = if p x then SOME x else find p xs;

find Char.isDigit [#"a",#"3", #"p", #"2"];

fun filter p []      = []
  | filter p (x::xs) = if p x then x :: filter p xs          
                       else filter p xs;

filter Char.isAlpha [#"1", #"p", #"F", #"-"]; 

fun partition p []      = ([],[])
  | partition p (x::xs) = 
          let val (xs1,xs2) = partition p xs 
          in if p x then (x::xs1,xs2)
             else        (xs1,x::xs2)
          end;

partition Char.isLower [#"P",#"a",#"3",#"%",#"b"];

fun foldr f b []      = b
  | foldr f b (x::xs) = f(x,foldr f b xs);

fun sumr xs = foldr op+ 0 xs;

val sumr = foldr op+ 0;

fun length xs = foldr (fn (_,y) => y+1) 0 xs;

length [4,5,6];

fun append(xs,bs) = foldr op:: bs xs;

append([1,2,3],[4,5]);

fun foldl f b []      = b
  | foldl f b (x::xs) = foldl f (f(x,b)) xs;

val suml = foldl op+ 0;

suml [1,2,3,4];

fun rev xs  = foldl op:: [] xs;

rev [1,2,3];

foldr op^ "nice" ["vacation ", "is "];

foldl op^ "nice" ["vacation ", "is "];

fun unzip s = foldr (fn ((x,y),(xs,ys)) => (x::xs,y::ys)) 
                    ([],[]) s                            ;

unzip [(1,"a"),(2,"b")];

fun revunzip s = foldl (fn ((x,y),(xs,ys)) => (x::xs,y::ys)) 
                     ([],[]) s                            ;

revunzip [(1,"a"),(2,"b")];

fun map f = foldr (fn (x,r) => (f x)::r) [];

fun existsR p = foldr (fn (x,b) => p x orelse b) false;

infix 3 o
fun f o g = fn x => f(g x);

infix 3 o
fun (f o g) x = f(g x);

datatype 'a circuit =
      Comp of 'a
    | Ser  of 'a circuit * 'a circuit
    | Par  of 'a circuit * 'a circuit

fun circRec (c,s,p) (Comp x)     = c x
  | circRec (c,s,p) (Ser(c1,c2)) = 
                 s(circRec (c,s,p) c1, circRec (c,s,p) c2)
  | circRec (c,s,p) (Par(c1,c2)) = 
                 p(circRec (c,s,p) c1, circRec (c,s,p) c2);

fun count cir = circRec (fn _ => 1, op+, op+) cir;

count (Ser(Par(Comp 0.25,Comp 1.0),Comp 1.5));

fun resistance cir 
   = circRec (fn r => r, 
              op+, 
              fn (r1,r2) => 1.0/(1.0/r1 + 1.0/r2)) cir;

resistance (Ser(Par(Comp 0.25,Comp 1.0),Comp 1.5));

