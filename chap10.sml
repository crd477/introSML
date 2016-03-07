
type 'a set = 'a list
val empty = [];
fun singleton a = [a];
fun member(x, ys) = List.exists (fn y => x = y) ys;
fun insert(x, ys) = if member(x, ys) then ys else x::ys;
fun union(xs, ys) = List.foldl insert ys xs;
fun inter(xs, ys) = List.filter (fn x => member(x, ys)) xs;
fun diff(xs, ys)  = 
   List.filter (fn x => not (member(x, ys))) xs;
fun delete([], _)    = []
  | delete(x::xs, y) = if x=y then xs else x::delete(xs,y);  
fun subset(xs, ys) = List.all (fn x => member(x, ys)) xs;
fun equal(xs, ys) = subset(xs, ys) andalso subset(ys, xs);
fun toList xs = xs;
val card = List.length;
val filter = List.filter;
fun map f s = 
   List.foldl (fn (y,ys) => insert(f y, ys)) empty s;
val fold = List.foldl;
fun split []     = NONE
  | split(x::xs) = SOME(x,xs);

abstype ''a absset = Set of ''a list
with
   val absempty = Set [];

   fun abssingleton a = Set [a];

   fun absmember(x, Set ys) = List.exists (fn y => x=y) ys;

   fun absinsert(x, ays as Set ys) = 
           if absmember(x, ays) then ays else Set(x::ys);

   fun absdelete(Set [], _)     = Set []
     | absdelete(Set(x::xs), y) = 
            if x=y then Set xs 
            else let val Set xs' = absdelete(Set xs, y)
                 in Set(x::xs')
                 end;  

   fun abstoList(Set xs) = xs;

end;
val s1 = absinsert(1, abssingleton 2);

use "Set.sig";
use "Set.sml";


datatype country  = DK | UK | ES | SF 
                  | PRC | C | D | IS | S | N
type name         = string
type title        = string
type author       = name * country
type contribution = title * author list
type conference   = contribution Set.set;

val c1 = ("Processors", [("Lu", PRC), ("Olsen",DK)]);

val c2 = ("Scheduling", [("Suonio", SF), ("Weber",D)]);

val c3 = ("RLucid", [("Plaice",C)]);

val c4 = ("Timed Observations", [("Ortega",ES)]);

val conf =  Set.fromList [c1, c2, c3, c4];

fun nationalitiesOf(_,al) =  
   Set.fromList (List.map (fn (_,c)=> c) al);

Set.toList(nationalitiesOf c1);

val nordic = Set.fromList [IS, DK, S, N, SF];

fun haveNordic c 
    = not ( Set.equal (Set.inter(nationalitiesOf c, nordic)
                       , Set.empty));

Set.card (Set.filter haveNordic conf);

Set.toList(Set.map (fn (t,(n,_)::_) => (t,n)) conf);

fun presentations cnf = 
   case (Set.split cnf) of
       NONE                    => Set.empty
     | SOME((t,(n,_)::_),cnf') => 
             Set.insert((t,n), presentations cnf');

Set.toList(presentations conf);

Set.toList(
   Set.fold (fn (c,s) => Set.union(nationalitiesOf c, s))
            Set.empty conf);
