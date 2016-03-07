datatype shape = Circle of real | Square of real
               | Triangle of real*real*real;

fun area (Circle r)        = Math.pi * r * r
  | area (Square a)        = a * a
  | area (Triangle(a,b,c)) = 
             let val s = (a + b + c)/2.0 
             in Math.sqrt(s*(s-a)*(s-b)*(s-c))
             end;

fun isShape (Circle r)        = r > 0.0
  | isShape (Square a)        = a > 0.0
  | isShape (Triangle(a,b,c)) =
       a > 0.0   andalso b > 0.0   andalso c > 0.0
       andalso 
       a < b + c andalso b < c + a andalso c < a + b;

exception Shape;

fun area x = 
    if not (isShape x) then raise Shape
    else case x of
             Circle r        => Math.pi * r * r
           | Square a        => a * a
           | Triangle(a,b,c) => 
                      let val s = (a + b + c)/2.0 
                      in Math.sqrt(s*(s-a)*(s-b)*(s-c)) 
                      end;

area (Triangle(3.0,4.0,5.0));

datatype colour = Red | Blue | Green | Yellow | Purple;

fun nice_colour Red  = true
  | nice_colour Blue = true
  | nice_colour _    = false;

nice_colour Purple;

fun countLEG []       = (0,0,0)
 |  countLEG(x::rest) =
       let val (y1,y2,y3) = countLEG rest in
          case Int.compare(x,0) of
              LESS    => (y1+1,y2  ,y3  )
            | EQUAL   => (y1  ,y2+1,y3  )
            | GREATER => (y1  ,y2  ,y3+1)
       end;

countLEG [~3,0,~2,1,~3,0];

fun fact 0 = 1
  | fact n = n * fact(n-1);

fun optFact n = if n < 0 then NONE else SOME(fact n);

fun optFact n =
    case Int.compare(n,0) of
       GREATER => SOME(n * valOf(optFact(n-1)))
      |EQUAL   => SOME 1
      |LESS    => NONE;

type articleCode = string;
type articleName = string;
type price       = int;
type register    = (articleCode * (articleName*price)) list;

exception FindArticle;

fun findArticle(ac, (ac',adesc)::reg) =
          if ac=ac' then adesc else findArticle(ac,reg)  
  | findArticle _                     = raise FindArticle;

datatype articleCode = AC of string;
datatype articleName = AN of string;
datatype price       = P of int;  
datatype register    = 
        R of (articleCode * (articleName * price)) list;

fun findArticle(ac, R((ac',adesc)::reg)) =
          if ac=ac' then adesc else findArticle(ac, R reg)
  | findArticle _                        = raise FindArticle;

val reg = R [ (AC "a1", (AN "cheese", P 25)),
              (AC "a2", (AN "herring", P 4)),
              (AC "a3", (AN "soft drink", P 5)) ];

findArticle(AC "a3", reg);

exception BadArgument of int;

fun factE n = 
    case Int.compare(n,0) of
       LESS    => raise BadArgument n
      |EQUAL   => 1
      |GREATER => n * factE(n-1);

fun factStr n = 
  Int.toString(factE n) 
  handle BadArgument n => "Bad argument " ^ Int.toString n;

factStr 4;

factStr ~3;

fun safe ((x,y),(x1,y1)::xs) = 
                x <> x1          (* not same column   *)
        andalso y <> y1          (* not same row      *)
        andalso y - x <> y1 - x1 (* not on /-diagonal *)
        andalso y + x <> y1 + x1 (* not on \(\backslash\)-diagonal *)
        andalso safe((x,y),xs) 
  | safe _                   = true;

exception Conflict;

fun queens(posl,x,yl) =
    if x > 8 then posl else       (*   1 *)
    case yl of                    
       y::yl' =>                  (* 2.a *)
         if safe((x,y),posl) 
         then queens(posl@[(x,y)], x+1, [1,2,3,4,5,6,7,8])
              handle Conflict => queens(posl,x,yl')
         else queens(posl,x,yl')
     | []     => raise Conflict;  (* 2.b *)

queens([],1,[1,2,3,4,5,6,7,8]);
