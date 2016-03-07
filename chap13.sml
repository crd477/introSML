use "Table.sig";
use "Table.sml";


signature PartsList =
sig

type partId    = string
type quantity  = int     (* n > 0 *)
type partsList = (partId * quantity) list

type prodReg; (* The representation is hidden. *) 

exception ProdReg; 

val newReg: prodReg  (* An empty product register.~*) 

val addPart: partId * partsList * prodReg -> prodReg 
    (* Adds a new part with corresponding parts list to 
       a product register.  May raise ProdReg.~*) 

val partsList: partId * prodReg -> partsList 
    (* Finds the parts list of a part in a product register. 
       Raises ProdReg if the part is not found.~*) 

val toList: prodReg -> (partId * partsList) list 
    (* Lists a product register as a list of parts with cor- 
       responding parts list. Included for test purposes.~*) 

val partBreakDown: partId * prodReg -> partsList  
    (* Finds the parts list of basic parts of a 
       part. May raise ProdReg.~*) 

end; 


structure PartsList:> PartsList = 
struct 
   
type partId    = string 
type quantity  = int   (* n>0 *) 
type partsList = (partId * quantity) list 

type prodReg = (partId, partsList) Table.table 
   (* The entries (pk,plk) can be ordered in a sequence 
      (p1,pl1),...,(pn,pln) such that any part identifier 
      in the list pli is one of p1,...,p(i-1)  *) 

exception ProdReg 

val newReg = Table.empty: prodReg 

   (* local function  
      legalPartsList: partsList * prodReg -> bool  *) 

fun legalPartsList(pl,preg) = 
   List.all 
      (fn (pid,c) => c>0 andalso Table.isKey(pid,preg)) 
      pl 

fun addPart(pid,pl,preg) = 
   if legalPartsList(pl,preg) 
      andalso not (Table.isKey(pid,preg))
   then Table.update(pid,pl,preg) 
   else raise ProdReg 

fun partsList(pid,preg) = case Table.lookup(pid,preg) of 
       SOME pl => pl 
   | NONE => raise ProdReg 

fun toList preg = Table.toList preg 

   (* local function addPartToPartsList:  
      (partId * count) * partsList -> partsList *)
 
fun addPartToPartsList((pid,n),[])= [(pid,n)] 
  | addPartToPartsList((pid,n),(pid',n')::pl) =  
       if pid = pid' then (pid, n+n')::pl 
       else (pid',n')::addPartToPartsList((pid,n),pl) 

   (* local function 
      mergePartsLists: partsList * partsList -> partsList *) 

fun mergePartsLists(pl,pl') = foldl addPartToPartsList pl pl' 

   (* local function  
      multPartsList: int * partsList -> partsList *) 

fun multPartsList(k,pl) = map (fn (pid,n) => (pid,k*n)) pl 

fun partBreakDown(pid, preg) = 
        case Table.getval(pid,preg) of 
           [] => [(pid,1)] 
         | pl => partsListBreakDown(pl,preg) 

and partsListBreakDown([],_) = [] 
  | partsListBreakDown((pid,n)::pl,preg) =  
      let val pl' = partBreakDown(pid,preg) 
          val pl1 = multPartsList(n,pl') 
          val pl2 = partsListBreakDown(pl,preg) 
      in 
          mergePartsLists(pl1,pl2) 
      end 
           
end; 

open PartsList;

val reg1 = addPart("part1", [], newReg);

val reg2 = addPart("part2", [], reg1);

toList reg2;

val reg3 =  addPart("part3",[("part1",5),("part2",4)],reg2);

partsList("part2", reg3);

val reg4 =  addPart("part4",[("part2",3),("part3",4)],reg3);

toList reg4;

partBreakDown("part4", reg4);
