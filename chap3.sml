infix xor;

fun false xor true  = true
  | true  xor false = true
  | _     xor _     = false;

1 > 2 xor 2 + 3 < 5;

nonfix xor;

xor(1 = 2, 3 = 4);

infix 6 vadd2   (* addition of 2-dim vectors     *)
infix 6 vsub2   (* subtraction of 2-dim vectors  *)
infix 7 tim2    (* scalar multiplication         *)
infix 6 dprod2  (* dot product for 2-dim vectors *)

fun (x1,y1) vadd2 (x2,y2) = (x1+x2,y1+y2): real*real;

fun (x1,y1) vsub2 (x2,y2) = (x1-x2,y1-y2): real*real;    

fun x tim2 (x1,y1) = (x*x1, x*y1): real*real;

fun (x1,y1) dprod2 (x2,y2) = x1*x2 + y1*y2: real;

fun (v1:real*real) vadd2 (v2:real*real) 
                  = (#1 v1 + #1 v2, #2 v1 + #2 v2);

fun norm2(x1,y1) = Math.sqrt(x1*x1+y1*y1);

val a = (1.0,~2.0);

val b = (3.0,4.0);

val c = 2.0 tim2 a vsub2 b; 

val d = c dprod2 a;         

val e = norm2 b;

infix 6 dprod4  (* dot product for 4-dim vectors *)
fun (x1,x2,x3,x4) dprod4 (y1,y2,y3,y4)
       = x1*y1 + x2*y2 + x3*y3 + x4*y4: real;

fun norm4 x = Math.sqrt(x dprod4 x);

val a = {name = "Peter", age = 20 };

#name a; 

#age a;

{age = 20, name =  "Peter"} = {name = "Peter", age = 20};

("a",true) =  {2 = true, 1 ="a"}; 

{2 = true, 1 = "a"};

val a = {name = "Peter", age = 20};

val {name = x, age = y} = a;

val {name, age} = a;  

val {name = x, ...} = a;  

val {age, ...} = a;

type person =
{age : int, birthday : int * int, name : string,
  occupation : string, sex : string};

val john = {name =  "John", age = 29, sex = "M",  
              occupation = "Teacher", birthday = (2,11)}; 

fun age(p: person) = #age p;

age john;

fun youngLady({age,sex,...}:person) = 
                     age < 25 andalso sex = "F";

type equation = real * real * real;

type solution = real * real;

exception Solve; 

(* SML NJ version *)

fun solve(a,b,c) = 
     if b*b-4.0*a*c < 0.0 orelse Real.==(a,0.0) then raise Solve 
     else ((~b+Math.sqrt(b*b-4.0*a*c))/(2.0*a),
           (~b-Math.sqrt(b*b-4.0*a*c))/(2.0*a));

solve(1.0, 0.0, 1.0); 

solve(1.0, 1.0, ~2.0);

solve(2.0, 8.0, 8.0);

fun solve(a,b,c) = 
   let val d = b*b-4.0*a*c
   in if d < 0.0 orelse Real.==(a,0.0) then raise Solve 
      else ((~b+Math.sqrt d)/(2.0*a)
           ,(~b-Math.sqrt d)/(2.0*a))
   end;   

fun solve(a,b,c) = 
      let val d = b*b-4.0*a*c
      in if d < 0.0 orelse Real.==(a,0.0) then raise Solve 
         else 
            let val d' = Math.sqrt d
                val b' = ~b
                val a' = 2.0*a
            in
               ((b'+ d')/a',(b'-d')/a')
            end

      end;   

local 
   fun disc(a,b,c) = b*b - 4.0*a*c
in
   exception Solve;

   fun hasTwoSolutions(a,b,c) = disc(a,b,c)>0.0 andalso Real.!=(a,0.0);

   fun solve(a,b,c) =
      let val d = disc(a,b,c)
      in if d < 0.0 orelse Real.==(a,0.0) then raise Solve 
         else ((~b+Math.sqrt d)/(2.0*a)
              ,(~b-Math.sqrt d)/(2.0*a))
      end
end;

hasTwoSolutions(1.0, 0.0, 1.0);

solve(1.0, 2.0, 1.0);

(* MosML Version *)


fun solve(a,b,c) = 
     if b*b-4.0*a*c < 0.0 orelse a = 0.0 then raise Solve 
     else ((~b+Math.sqrt(b*b-4.0*a*c))/(2.0*a),
           (~b-Math.sqrt(b*b-4.0*a*c))/(2.0*a));

solve(1.0, 0.0, 1.0);

solve(1.0, 1.0, ~2.0);

solve(2.0, 8.0, 8.0);

fun solve(a,b,c) = 
   let val d = b*b-4.0*a*c
   in if d < 0.0 orelse a = 0.0 then raise Solve 
      else ((~b+Math.sqrt d)/(2.0*a)
           ,(~b-Math.sqrt d)/(2.0*a))
   end;   

fun solve(a,b,c) = 
      let val d = b*b-4.0*a*c
      in if d < 0.0 orelse a = 0.0 then raise Solve 
         else 
            let val d' = Math.sqrt d
                val b' = ~b
                val a' = 2.0*a
            in
               ((b'+ d')/a',(b'-d')/a')
            end
      end;   

local 
   fun disc(a,b,c) = b*b - 4.0*a*c
in
   exception Solve;

   fun hasTwoSolutions(a,b,c) = disc(a,b,c)>0.0 andalso a<>0.0;

   fun solve(a,b,c) =
      let val d = disc(a,b,c)
      in if d < 0.0 orelse a = 0.0 then raise Solve 
         else ((~b+Math.sqrt d)/(2.0*a)
              ,(~b-Math.sqrt d)/(2.0*a))
      end
end;

hasTwoSolutions(1.0, 0.0, 1.0);

solve(1.0, 2.0, 1.0);
