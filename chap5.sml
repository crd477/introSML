val xs = [2,3,2];

val ys = ["Big", "Mac"];

[("b",2),("c",3),("e",5)];
[{name="Brown", age = 25}, {name = "Cook", age = 45}];

[Math.sin, Math.cos];

[[2,3],[3],[2,3,3]];

("bce",[2,3,5]);

[2,3,2] = [2,3];

[2,3,2] = [2,3,3];

val x = 2::[3,4,5];

val y = ""::[];

val z = 2::3::[4,5];

nil = [];

fun downto1 0 = [] 
  | downto1 n = n::downto1(n-1);

downto1 4;

fun upto(i,j) = if i>j then [] else i::upto(i+1,j);

upto(~2,3);

upto(3,~2);

fun from1upto n = upto(1,n);

from1upto 4;

local 
   fun upto(i,j) = if i>j then [] else i::upto(i+1,j)
in
   fun from1upto n = upto(1,n)
end;

fun from1upto n =
   let fun upto(i,j) = if i>j then [] else i::upto(i+1,j)
   in upto(1,n)
   end;

fun from1upto n =
   let fun uptoN i = if i>n then [] else i::uptoN(i+1)
   in uptoN 1
   end;

val x::xs = [1,2,3];

val [x1,x2,x3] = [(1,true), (2,false), (3, false)];

val x1::x2::xs = [1.1, 2.2, 3.3, 4.4, 5.5];

val (y1, y2)::ys = [(1,[1]), (2, [2]), (3, [3]),(4,[4])]; 

fun suml []     = 0
  | suml(x::xs) = x + suml xs

fun altsum []          = 0
  | altsum [x]         = x
  | altsum(x1::x2::xs) = x1 - x2 + altsum xs;

altsum [2, ~1, 3];

infixr 5 @
fun      [] @ ys = ys
  | (x::xs) @ ys = x::(xs @ ys);

[1,2] @ [3,4];

[[1],[2,3]] @ [[4]];

[1] @ 2 :: [3];

1 :: [2] @ [3];

fun naive_rev []     = []
 |  naive_rev(x::xs) = naive_rev xs @ [x];

val z = [];

(5,[[]]);

1::(rev []);

(rev []): int list;

explode "abcdefg";

implode it; 

length [];

length [1,2,3];

null []; 

null [1,2,3]; 

hd [[],["a"]];

tl [[],["a"]];

List.nth ([1,2,3], 2);

List.take([0,1,2,3,4,5,6], 4);

List.drop([0,1,2,3,4,5,6], 4);

fun unzip []           = ([],[])
  | unzip((x,y)::rest) = let val (xs,ys) = unzip rest 
                         in (x::xs,y::ys) end;

infix mem 

fun x mem []      = false 
  | x mem (y::ys) = x=y orelse x mem ys; 

fun remove(_, [])    = []
  | remove(x, y::ys) = if x=y then remove(x,ys) 
                       else y::remove(x,ys);

fun removeDub []     = []
  | removeDub(x::xs) = x::removeDub(remove(x,xs)); 

removeDub [1,2,2,1,4,1,5]; 

type cd = {title: string,
           artist: string,
           company: string,
           year: int,
           songs: string list};

type cdRegister = cd list;

val cdreg = [{title="t1", artist="a1", company="c1", 
               year=93, songs=["s1","s2","s3","s4","s5"]},
             {title="t2", artist="a2", company="c2", 
               year=91, songs=["s6","s7","s8","s9"]},
             {title="t3", artist="a1", company="c2", 
               year=94,  songs=["s10","s11","s12"]}
            ];

fun titles(_, []: cdRegister)              = []
  | titles(a, {artist, title, ...}::cdreg) =
            if a=artist then title::titles(a, cdreg)
            else titles(a, cdreg);

titles("a1", cdreg); 

exception Cd

fun findArtist(_, []: cdRegister)              = raise Cd
  | findArtist(s, {artist, songs, ...}::cdreg) =  
            if s mem songs then artist 
            else findArtist(s, cdreg);


findArtist("s7", cdreg); 

(* findArtist("s88", cdreg); *)

fun remove(_, [])    = []
  | remove(x, y::ys) = if x=y then remove(x,ys) 
                       else y::remove(x,ys);
