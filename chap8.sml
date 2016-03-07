datatype colour = Red | Blue | Green | Yellow | Purple;

datatype cbox = Nothing                         (* 1. *)
              | Cube of real * colour * cbox;   (* 2. *)

val cb1 = Cube(0.5, Red, Nothing);

val cb2 = Cube(1.0, Green,cb1);

val cb3 = Cube(2.0,Yellow,cb2);


fun count Nothing       = 0
  | count(Cube(r,c,cb)) = 1 + count cb;

count cb2 + count cb3;

exception ChineseBox;

fun insert(r,c,cb) =
       if r <= 0.0 then raise ChineseBox
       else case cb of
               Nothing         => Cube(r,c,Nothing)
             | Cube(r1,c1,cb1) =>
                case Real.compare(r,r1) of
                  GREATER => Cube(r,c,cb)
                | EQUAL   => raise ChineseBox
                | LESS    => Cube(r1,c1,insert(r,c,cb1));

insert(2.0,Yellow,insert(1.0,Green,Nothing));

insert(1.0,Green,insert(2.0,Yellow,Nothing));


fun difflist Nothing                        = []
  | difflist(Cube(_,_,Nothing))             = []
  | difflist(Cube(r2,_,cb as Cube(r1,_,_))) = 
                              (r2-r1)::difflist(cb);

fun difflist(Cube(r2,_,cb as Cube(r1,_,_))) = 
                              (r2-r1)::difflist(cb)
  | difflist _                              = []; 

datatype cbox' = Single of real * colour
               | Multiple of real * colour * cbox';

exception ChineseBox;

fun count(Single _)         = 1
  | count(Multiple(_,_,cb)) =  1 + count cb; 

fun insert(r1,c1,cb1 as Single(r2,c2)) = 
        if r1 <= 0.0 then raise ChineseBox
        else (case Real.compare(r1,r2) of
                 LESS    => Multiple(r2,c2,Single(r1,c1))
               | EQUAL   => raise ChineseBox
               | GREATER => Multiple(r1,c1,cb1)  )
  | insert(r1,c1,cb1 as Multiple(r2,c2,cb2)) =
        if r1 <= 0.0 then raise ChineseBox
        else (case Real.compare(r1,r2) of
                 LESS    => Multiple(r2,c2,insert(r1,c1,cb2))
               | EQUAL   => raise ChineseBox
               | GREATER => Multiple(r1,c1,cb1)  );

infix 6 ++ --;
infix 7 ** //;

datatype fexpr =
    Const of real                                 (* 1 *)
  | X                                             (* 2 *)
  | ++ of fexpr * fexpr | -- of fexpr * fexpr     (* 3 *)
  | ** of fexpr * fexpr | // of fexpr * fexpr     (* 3 *)
  | Sin of fexpr | Cos of fexpr                   (* 4 *)
  | Ln of fexpr | Exp of fexpr ;                  (* 4 *)

fun D(Const _)     = Const 0.0
  | D X            = Const 1.0
  | D(fe1 ++ fe2)  = (D fe1) ++ (D fe2)
  | D(fe1 -- fe2)  = (D fe1) -- (D fe2)
  | D(fe1 ** fe2)  = (D fe1) ** fe2 ++ fe1 ** (D fe2)
  | D(fe1 // fe2)  =
         ((D fe1) ** fe2 -- fe1 ** (D fe2)) // (fe2 ** fe2)
  | D(Sin fe)      = (Cos fe) ** (D fe)
  | D(Cos fe)      = ((Const ~1.0) ** (Sin fe)) ** (D fe)
  | D(Ln fe)       = (D fe) // fe
  | D(Exp fe)      = (Exp fe) ** (D fe) ;

D(Sin (X ** X));

D(Const 3.0 ** Exp X);

fun toString(Const r)    = Real.toString r
  | toString X            = "x"
  | toString(fe1 ++ fe2) =   "(" ^ (toString fe1) 
                          ^ " + " ^ (toString fe2) ^ ")"
  | toString(fe1 -- fe2) =   "(" ^ (toString fe1) 
                          ^ " - " ^ (toString fe2) ^ ")"
  | toString(fe1 ** fe2) =   "(" ^ (toString fe1) 
                          ^ " * " ^ (toString fe2) ^ ")"
  | toString(fe1 // fe2) =   "(" ^ (toString fe1)
                          ^ " / " ^ (toString fe2) ^ ")"
  | toString(Sin fe)     = "(sin " ^ (toString fe) ^ ")"
  | toString(Cos fe)     = "(cos " ^ (toString fe) ^ ")"
  | toString(Ln fe)      = "(ln " ^ (toString fe) ^ ")"
  | toString(Exp fe)     = "(exp " ^ (toString fe) ^ ")" ;

toString(Cos (X ** X) ** (Const 1.0 ** X ++ X ** Const 1.0));

toString(X ** X ** X ++ X ** X);

fun compute(Const r,_)    = r
  | compute(X,y)          = y
  | compute(fe1 ++ fe2,y) = compute(fe1,y) + compute(fe2,y)   
  | compute(fe1 -- fe2,y) = compute(fe1,y) - compute(fe2,y)
  | compute(fe1 ** fe2,y) = compute(fe1,y) * compute(fe2,y)
  | compute(fe1 // fe2,y) = compute(fe1,y) / compute(fe2,y)
  | compute(Sin fe,y)     = Math.sin(compute(fe,y)) 
  | compute(Cos fe,y)     = Math.cos(compute(fe,y))
  | compute(Ln fe,y)      = Math.ln(compute(fe,y))
  | compute(Exp fe,y)     = Math.exp(compute(fe,y)) ;

compute(Const 3.0 ** Ln (Exp X), 9.0);

datatype ancTree = Unspec 
                 | Info of string * ancTree * ancTree; 
val at = Info("Joachim", 
            Info("Margrethe", 
               Info("Ingrid",Unspec,Unspec), 
               Info("Frederik",Unspec,Unspec)), 
            Info("Henrik",Unspec,Unspec)); 

fun pre_listof Unspec          = []                (* 1 *)
  | pre_listof (Info(n,mt,ft)) =
          n::(pre_listof mt) @ (pre_listof ft);    (* 2 *)

pre_listof at;

fun in_listof Unspec          = []                
  | in_listof (Info(n,mt,ft)) =
          (in_listof mt) @ [n] @ (in_listof ft) ;

in_listof at;

fun post_listof Unspec          = []                  
  | post_listof (Info(n,mt,ft)) =
          (post_listof mt) @ (post_listof ft) @ [n];

post_listof at;

fun maleanc Unspec          = []
  | maleanc (Info(_,mt,ft)) = 
       (maleanc mt) @ (maleanc ft) 
                    @ (case ft of 
                            Unspec      => []
                          | Info(n,_,_) => [n]) ;

maleanc at;

datatype elem = File of string 
              | Catalogue of string*contents
withtype contents = elem list;

val fs = 
   Catalogue("c1", 
             [File "a1", 
              Catalogue("c2", 
                        [File "a2", 
                         Catalogue("c3", [File "a3"])]),
              File "a4",
              Catalogue("c3", [File "a5"])]);

fun nameElems(File s)            = [s]
  | nameElems(Catalogue(s, cnt)) = s::(nameContents cnt)

and nameContents [] = []
  | nameContents (e::es) = nameElems e @ (nameContents es);


nameElems fs;

datatype Atr = ALeaf | ANode of Btr * Btr
and      Btr = BLeaf | BNode of Atr * Atr * Atr;

val t = ANode(BLeaf,BNode(ALeaf,ANode(BLeaf,BLeaf),ALeaf));

fun noALeafsInAtr ALeaf           = 1
  | noALeafsInAtr(ANode(bt1,bt2)) = 
          noALeafsInBtr bt1 + noALeafsInBtr bt2

and noALeafsInBtr BLeaf                = 0
  | noALeafsInBtr (BNode(at1,at2,at3)) = noALeafsInAtr at1 
                                       + noALeafsInAtr at2 
                                       + noALeafsInAtr at3;

noALeafsInAtr t;

datatype ('a,'b) bintree =
               Leaf of 'a
             | Node of ('a,'b) bintree * 'b * ('a,'b) bintree;

datatype 'a circuit =
      Comp of 'a
    | Ser  of 'a circuit * 'a circuit
    | Par  of 'a circuit * 'a circuit;

val cmp = Ser(Par(Comp 0.25,Comp 1.0),Comp 1.5);

fun count (Comp _)     = 1
  | count (Ser(c1,c2)) = count c1 + count c2
  | count (Par(c1,c2)) = count c1 + count c2;

count cmp;

fun resistance (Comp r) = r
  | resistance (Ser(c1,c2)) = resistance c1 + resistance c2
  | resistance (Par(c1,c2)) 
            = 1.0 / (1.0/resistance c1 + 1.0/resistance c2);

resistance cmp;

abstype qnum = MkQ of int*int  (* MkQ(a,b) with b<>0 *)
with
   exception QDiv

   fun mkQ (_,0) = raise QDiv
     | mkQ pr    = MkQ pr

   fun ++(MkQ(a,b), MkQ(c,d)) = MkQ(a*d + b*c, b*d);
   fun --(MkQ(a,b), MkQ(c,d)) = MkQ(a*d - b*c, b*d);            
   fun **(MkQ(a,b), MkQ(c,d)) = MkQ(a*c, b*d);
   fun //(MkQ(a,b), MkQ(c,d)) = **(MkQ(a,b), mkQ(d,c));

   fun ==(MkQ(a,b), MkQ(c,d)) = (a*d = b*c);

   fun gcd(0,n) = n              
     | gcd(m,n) = gcd(n mod m,m); 

   fun toString(MkQ(p,q)) = 
     let val sign = if p*q<0 then "\verb/~/" else ""
         val ap = abs p
         val aq = abs q
         val d  = gcd(ap,aq)
     in sign ^ (Int.toString(ap div d)) 
             ^ "/" ^ (Int.toString(aq div d))
     end;
end;

infix 6 ++ --  
infix 7 ** //  
infix 4 == 

val q1 = mkQ(2,5);

val q2 = mkQ(3,7);

val q3 = q1 ++ q2;

toString q3;

datatype tree = Lf | Br of tree*int*tree;

Br(Br(Br(Lf,2,Lf),7,Lf),9,Br(Br(Lf,13,Lf),21,Br(Lf,25,Lf)));


abstype stree = Lf | Br of stree * int * stree
with 
   val empty = Lf

   fun insert(i, Lf)                = Br(Lf,i,Lf)
     | insert(i, tr as Br(t1,j,t2)) =
           case Int.compare(i,j) of
                 EQUAL   => tr
               | LESS    => Br(insert(i,t1),j,t2)
               | GREATER => Br(t1,j,insert(i,t2))
          
   fun member(i, Lf)          = false  
     | member(i, Br(t1,j,t2)) =
           case Int.compare(i,j) of
                 EQUAL   => true
               | LESS    => member(i,t1)
               | GREATER => member(i,t2)

   fun toList Lf            = []
     | toList (Br(t1,j,t2)) = toList t1 @ [j] @ toList t2;
end;
val st1 = insert(2, empty);

val st2 = insert(~3, insert(7, st1));

member(4, st2);

member(7, st2);

toList st2;

