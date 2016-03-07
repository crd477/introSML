use "Set.sig";
use "Set.sml";
use "Table.sig";
use "Table.sml";

type articleCode = string
type articleName = string
type noPieces    = int   
type price       = int   

type register = 
        (articleCode, articleName * price) Table.table
type purchase = (articleCode, noPieces) Table.table
type info = noPieces * articleName * price
type bill = info list * price

fun makebill(pur,reg) =
   case Table.split pur of 
       SOME (ac,np,pur') => 
             let val (aname,aprice) = Table.getval(ac,reg)
                 val tprice         = np*aprice
                 val (infs,sum)     = makebill(pur',reg)
             in ((np,aname,tprice)::infs, tprice+sum)
             end
     | NONE                 => ([],0);

fun makebill(pur,reg) = 
  let fun f(ac,np,(infs,sum)) =  
              let val (aname,aprice) = Table.getval(ac,reg)
                  val tprice         = np*aprice
              in ((np,aname,tprice)::infs, tprice+sum)
              end
  in   Table.fold f ([],0) pur 
  end;

val register = Table.update("a1", ("cheese",25),
               Table.update("a2", ("herring",4),
               Table.singleton("a3", ("soft drink",5))));

val purchase = Table.update("a2",3, Table.singleton("a1",1));

makebill(purchase,register);

val register = Table.fromList [("a1", ("cheese",25)),
                               ("a2", ("herring",4)),
                               ("a3", ("soft drink",5))];

infix 6 ++
infix 7 ** 

datatype expr = X 
              | Const of real
              | ++ of expr * expr 
              | ** of expr * expr 
              | FC of string * expr 

val sin' = fn x => FC("cos", x);

val cos' = fn x => FC("-", FC("sin", x));

val der1 = Table.fromList [("sin", sin'), ("cos", cos')]; 

fun D X _            = Const 1.0
  | D (Const _) _    = Const 0.0
  | D (fe1 ++ fe2) t = (D fe1 t) ++ (D fe2 t)
  | D (fe1 ** fe2) t = (D fe1 t) ** fe2 ++ fe1 ** (D fe2 t)
  | D (FC(f, fe)) t  = let val f' = Table.getval(f,t) 
                       in f'(fe) ** (D fe t)
                       end ;

D (FC("sin", FC("cos", X))) der1;

fun extExpr (FC(s,fe))   = Set.insert(s, extExpr fe)
  | extExpr (fe1 ++ fe2) = Set.union(extExpr fe1, extExpr fe2)
  | extExpr (fe1 ** fe2) = Set.union(extExpr fe1, extExpr fe2)
  | extExpr _            = Set.empty;

Set.toList(extExpr (FC("-", FC("sin", X))));

fun extDerivative r = extExpr (r X);

Set.toList(extDerivative cos');

fun extTable t = 
       case Table.split t of 
           NONE         => Set.empty
         | SOME(_,r,t') => 
               Set.union(extDerivative r,  extTable t');

fun extTable t = 
   Table.fold (fn (_,r,s) => Set.union(extDerivative r, s)) 
              Set.empty t;

Set.toList(extTable der1);

fun dom t = Table.fold (fn (a,_,c) =>  Set.insert(a,c)) 
                       Set.empty t;

fun cond t = Set.subset(extTable t, dom t);

cond der1;

val der2 = Table.update("-", fn x => Const ~1.0, der1);

cond der2;

fun isDiff(e,t) = Set.subset(extExpr e,dom t) andalso cond t;

isDiff(FC("sin", FC("cos", X)), der1); 

isDiff(FC("sin", FC("cos", X)), der2);
