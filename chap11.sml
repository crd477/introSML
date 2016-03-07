(* Examples of Chapter 11.  This version does only run under a version
   of SML with implementation of the full module system.
   In Moscow ML each signature and structure can be placed in a separate
   file and compiled. *)

structure Set =
struct
   type 'a set = 'a list;
   val empty = [];
   fun singleton a = [a];
   fun member(x, ys) = List.exists (fn y => x = y) ys;
   fun insert(x, ys) = if member(x, ys) then ys else x::ys;
   fun toList xs = xs;
end;

Set.empty;

val s1 = Set.insert(1, Set.singleton 2);

open Set;

insert(1, singleton 2);

signature Set = 
sig 
   type 'a set 
   val empty     : ''a set 
   val singleton : ''a -> ''a set
   val member    : ''a * ''a set -> bool
   val insert    : ''a * ''a set -> ''a set
   val toList    : ''a set -> ''a list
end

structure Set : Set =
struct
   type 'a set = 'a list;
   val empty = [];
   fun singleton a = [a];
   fun member(x, ys) = List.exists (fn y => x = y) ys;
   fun insert(x, ys) = if member(x, ys) then ys else x::ys;
   fun toList xs = xs;
end

val s1 = Set.singleton 2;

s1 = [2];

Set.member(1, Set.insert(3, s1));

Set.member(1, [3,2]);

structure Set :> Set =
struct
   type 'a set = 'a list;
   val empty = [];
   fun singleton a = [a];
   fun member(x, ys) = List.exists (fn y => x = y) ys;
   fun insert(x, ys) = if member(x, ys) then ys else x::ys;
   fun toList xs = xs;
end

val s1 = Set.singleton 2;

Set.toList s1;

signature T =
sig 
  type t = int

  val f : t -> t 
end

structure T :> T =
struct
   type t=int

   fun aux x = x+1
   fun f(x) = aux(x+7)
end;

signature SIG1 =
sig 
   eqtype t1
   type t2
       sharing type t1 = t2
end;

structure STR1 : SIG1 =
struct
   type t1 = int
   type t2 = int
end;

signature SIGA =
sig 
   eqtype   ta 
   type     tb
            sharing type ta = tb
end;

structure STRA : SIGA =
struct
  datatype ta = Yes | No;
  datatype tb = datatype ta
end

signature SIGI =
sig
   eqtype alpha 
   type beta = alpha list
end;


signature SetEq =
sig 
   eqtype element   
   type set
   val empty : set
   val singleton : element  -> set 
   val member: element * set -> bool
   val insert: element * set -> set
   val toList: set -> element list
end

functor SetFct(s: sig eqtype element end)
        :> SetEq where type element = s.element =
struct
   type element = s.element

   type set = s.element list

   val empty = []

   fun singleton x = [x]
   
   fun member(x, ys) = List.exists (fn y => x = y) ys;

   fun insert(x, ys) = if member(x,ys) then ys else x::ys;

   fun toList xs = xs
end 

structure intset = SetFct(type element = int);

val is1 = intset.insert(1, intset.empty);

intset.toList is1;

structure intlistset = SetFct(type element = int list);

fun eqLetters(ch1, ch2) = 
        Char.toUpper ch1 = Char.toUpper ch2;

signature EQ =
sig 
   type element 
   val eq: element * element -> bool 
end

signature SetEqPr =
sig 
   type element
   type set
   val empty : set
   val singleton: element -> set
   val insert: element * set -> set
   val member: element * set -> bool
   val union : set * set -> set
   val toList: set -> element list
end

functor SetFctPr(s: EQ) 
           :> SetEqPr where type element = s.element =
struct
   type element = s.element   

   type set = s.element list

   val empty = [];
   
   fun singleton x = [x];

   fun member(x, ys) = List.exists (fn y => s.eq(x,y)) ys

   fun insert(x, ys) = if member(x,ys) then ys else x::ys 

   fun union(xs, ys) = List.foldr insert ys xs 

   fun toList xs = xs
end 

signature CharSet = SetEqPr where type element = char;

structure charEQ1 :> CharSet = 
         SetFctPr(type element = char  val eq = op=);

val s1 = charEQ1.insert(#"a", charEQ1.singleton #"b");

charEQ1.toList s1;

structure charEQ2 :> CharSet =
       SetFctPr(type element = char  val eq = eqLetters);

val s2 = charEQ2.insert(#"A", charEQ2.singleton #"B");

signature Arith = 
sig
   type result 
   val constant    : real -> result
   val add         : result * result -> result
   val minus       : result * result -> result 
   val multiply    : result * result -> result
   val divide      : result * result -> result
   val sine        : result -> result
   val cosine      : result -> result
   val logarithm   : result -> result
   val exponential : result -> result     
end

signature Eval = 
sig
   datatype fexpr =
       Const of real                                  
     | X                                              
     | ++ of fexpr * fexpr | -- of fexpr * fexpr      
     | ** of fexpr * fexpr | // of fexpr * fexpr      
     | Sin of fexpr | Cos of fexpr                    
     | Ln of fexpr  | Exp of fexpr                    
 
   type result

   val eval: fexpr -> result -> result
end  

functor arithEval(s : Arith) 
        :> Eval where type result = s.result = 
struct 
   datatype fexpr =
       Const of real                                  
     | X                                              
     | ++ of fexpr * fexpr | -- of fexpr * fexpr      
     | ** of fexpr * fexpr | // of fexpr * fexpr      
     | Sin of fexpr | Cos of fexpr                    
     | Ln of fexpr  | Exp of fexpr                    

   type result = s.result

   fun eval exp x =
      case exp of
          Const r     => s.constant r
        | X           => x
        | ++(fe1,fe2) => s.add(eval fe1 x, eval fe2 x)
        | --(fe1,fe2) => s.minus(eval fe1 x, eval fe2 x)
        | **(fe1,fe2) => s.multiply(eval fe1 x, eval fe2 x)
        | //(fe1,fe2) => s.divide(eval fe1 x, eval fe2 x)
        | Sin fe      => s.sine(eval fe x)
        | Cos fe      => s.cosine(eval fe x)
        | Ln fe       => s.logarithm(eval fe x)
        | Exp fe      => s.exponential(eval fe x)
end

structure infixstringArith 
          :> Arith where type result = string =
struct
   type result     = string
   val constant    = Real.toString 
   val add         = fn(s1,s2) => "(" ^ s1 ^ " + " ^ s2 ^ ")"
   val minus       = fn(s1,s2) => "(" ^ s1 ^ " - " ^ s2 ^ ")"
   val multiply    = fn(s1,s2) => "(" ^ s1 ^ " * " ^ s2 ^ ")"
   val divide      = fn(s1,s2) => "(" ^ s1 ^ " / " ^ s2 ^ ")"
   val sine        = fn s => "(sin " ^ s ^ ")"
   val cosine      = fn s => "(cos " ^ s ^ ")"
   val logarithm   = fn s => "(ln " ^ s ^ ")"
   val exponential = fn s => "(exp " ^ s ^ ")"
end

structure infixstringEval = arithEval(infixstringArith);

open infixstringEval;

val f = eval (++(Const 2.9, X));

f "x";

eval (Sin(++(Const 2.9, X))) "y";

structure realArith :> Arith where type result = real =
struct
   type result     = real
   val constant    = fn x => x 
   val add         = op+ : result * result -> result
   val minus       = op- : result * result -> result
   val multiply    = op* : result * result -> result
   val divide      = op/ : result * result -> result
   val sine        = Math.sin
   val cosine      = Math.cos
   val logarithm   = Math.ln
   val exponential = Math.exp
end

structure realEval = arithEval(realArith);

open realEval;

val fe = ++(Const 2.0, **(Sin (Const 2.0), X));

val f = eval fe;

f 7.0;

f 10.0;
