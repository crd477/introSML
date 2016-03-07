val x = ref 1; 

val y = ref 3; 

val z = ref x; 

val u = ref [1,2,3];

val w = x; 

val ref p = z;

val ref (b::bs) = u;

!x = 1 andalso !(!z) = !x;

[x,y,!z,w];

fun !(ref x) = x;

x := !x + 1; z := y;

[x,y,!z,w];

fun incr(t) = t := !t + 1;
y; 
incr(y);
y;

fun incrval(t) = (t := !t + 1 ; !t);
incrval(y);
incrval(y);

fun incr(t as ref x) = t := x + 1;

(*
fun incrval(t as ref x) = (t := x + 1 ; !t);
let
   val a    = ref [];
   fun f(x) = a := x::(!a) ;
in
   f(1);  f("A"); a
end;
*)

val a = Array.fromList [4,5,6,7];

val b = Array.array(3,"ab");

fun toList a = Array.foldr op:: [] a;
toList a;
toList b;
Array.length a;
Array.length b;
Array.sub(a, 2);
(* Array.sub(b,3); 
Array.update(a,5,9); *)

Array.update(b,1,"ccccc"); 

toList b;

fun swap(a,i,j) = let val v = Array.sub(a,i)
                  in Array.update(a, i, Array.sub(a,j)) ; 
                     Array.update(a, j, v)
                  end;

swap(b,0,1);
toList b;

fun binSearch(v, a) = 
   let fun f(first, top) = 
       if first=top then NONE
       else let val mid = (first+top) div 2
            in case Int.compare(v, Array.sub(a,mid)) of 
                   LESS    => f(first, mid)
                 | EQUAL   => SOME mid
                 | GREATER => f(mid+1, top)
            end
   in f(0, Array.length a) end;      

val a = Array.fromList [1, 3, 5, 7, 9, 11, 13, 15]; 

binSearch(0,a);
binSearch(1, a);
binSearch(9,a);
binSearch(15,a);
binSearch(16,a);

fun partition(a, v, k1, k2) =
   if k2=k1-1 then k2  (* empty section *)
   else if Array.sub(a,k2) >= v then partition(a, v, k1, k2-1)
        else (swap(a,k1,k2); partition(a,v,k1+1,k2))

val a1 = Array.fromList [9,5,1,~3,5,7,0];
partition(a1, 2, 1, 6);
toList a1;

fun qsort (a,i,j) =
   if j-i<=1 then ()
   else let val k = partition(a, Array.sub(a,i),i+1,j-1)
        in swap(a,i,k); 
           qsort(a,i,k); 
           qsort(a,k+1,j)
        end    

val b1 = Array.fromList [7, 6, 5, 19, 8, 4, 10, 1];
qsort(b1, 2,7);
toList b1;

fun quicksort a = qsort(a, 0, Array.length a);

val c1 = Array.fromList[1, 5, 2, ~3, 7,0];
quicksort c1;
toList c1;

fun itfact(n,m) = if n<>0 then itfact(n-1,n*m) else m;

fun itfact z =
    if #1(z) <> 0 then itfact(#1(z) - 1, #1(z) * #2(z))
                  else #2(z);
fun factI z =
   let val zi = ref z 
   in while #1(!zi) <> 0 do 
              zi := (#1(!zi) - 1, #1(!zi) * #2(!zi))
      ; #2(!zi)
   end;


fun factI (n,m) =
   let val ni = ref n
       val mi = ref m
   in while !ni <> 0 do (mi := !ni * !mi; ni := !ni - 1)
      ; !mi
   end;


type articleCode = int
type articleName = string
type price       = int   
type register    = (articleName * price) Array.array

val register = Array.fromList [("cheese",25), 
                               ("herring",4),
                               ("soft drink",5)
                              ];

Array.sub(register, 1);

fun makebill([], _)              = ([],0)
  | makebill((np,ac)::pur, reg) =
        let val (aname,aprice) = Array.sub(reg,ac)
            val tprice         = np*aprice
            val (billtl,sumtl) = makebill(pur,reg)
        in ((np,aname,tprice)::billtl, tprice+sumtl)
        end;

val pur = [(3,1),(1,0)];
makebill(pur, register);
Array.update(register, 1, ("herring",6) );
makebill(pur, register);
