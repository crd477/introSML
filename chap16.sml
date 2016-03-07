use "Set.sig";
use "Set.sml";
use "Table.sig";
use "Table.sml";

signature Flight = 
sig 
   type person        = string 
   type reservations  = person Set.set 
   type waitingList   = person list (* without replicas *) 
   type numberOfSeats = int         (* > 0              *) 
   type flight        = string 
   type flightData    = numberOfSeats * reservations * waitingList 
                        (* (seats, rsv, wl) with  
                           ( card(rsv) <= seats and  wl = [] )  
                             or ((card(rsv) = seats) and 
                                 (p in rsv => p not in wl))  *)
 
   type flightDescr   = flight * flightData 

   datatype command = 
       CmdCreate     of flight * numberOfSeats 
     | CmdReserve    of flight * person 
     | CmdCancel     of flight * person 
     | CmdListAll 

   datatype result = 
       ResOK 
     | ResIllegalNoOfSeats 
     | ResFlightAlreadyExists 
     | ResUnknownFlight 
     | ResAlreadyReserved 
     | ResOnWaitList 
     | ResAlreadyOnWaitList 
     | ResAllList of flightDescr list 

   type register 

   val initialReg: register 

   val eval: register * command -> register * result 

end; 

structure Flight :> Flight =
struct

   type person        = string 
   type reservations  = person Set.set 
   type waitingList   = person list (* without replicas *) 
   type numberOfSeats = int         (* > 0              *) 
   type flight        = string 
   type flightData    = numberOfSeats * reservations * waitingList 
                        (* (seats, rsv, wl) with  
                           ( card(rsv) <= seats and  wl = [] )  
                             or ((card(rsv) = seats) and 
                                 (p in rsv => p not in wl))  *)
 
   type flightDescr   = flight * flightData 

   datatype command = 
       CmdCreate     of flight * numberOfSeats 
     | CmdReserve    of flight * person 
     | CmdCancel     of flight * person 
     | CmdListAll 

   datatype result = 
       ResOK 
     | ResIllegalNoOfSeats 
     | ResFlightAlreadyExists 
     | ResUnknownFlight 
     | ResAlreadyReserved 
     | ResOnWaitList 
     | ResAlreadyOnWaitList 
     | ResAllList of flightDescr list 

   type register = (flight,flightData) Table.table

   val initialReg = Table.empty: register


fun eval(reg,cmd) =  case cmd of
 
  CmdCreate(f,seats) =>                                (* 1  *)
    if seats <= 0 
    then (reg, ResIllegalNoOfSeats)                    (* 1a *)
    else 
    (case Table.insert(f,(seats,Set.empty,[]),reg) of 
        NONE      =>  (reg , ResFlightAlreadyExists)   (* 1b *)
      | SOME reg' =>  (reg', ResOK) )                  (* 1c *)

| CmdReserve(f,p) =>                                   (* 2  *)
    (case Table.lookup(f,reg) of
        NONE            => (reg, ResUnknownFlight)     (* 2a *)
      | SOME (seats,res,wl) => 
         if Set.member(p,res)
            then (reg, ResAlreadyReserved)             (* 2b *)
         else if Set.card(res) < seats then            (* 2c *)
            let val res' = Set.insert(p,res) in
               (Table.update(f,(seats,res',wl),reg), ResOK)
            end 
         else if List.exists (fn x => x = p) wl then   (* 2d *)
            (reg, ResAlreadyOnWaitList) 
         else (Table.update(f,(seats,res,wl @ [p]),reg)
               , ResOnWaitList)  )                     (* 2e *) 

| CmdCancel(f,p) =>                                    (* 3  *)
    (case Table.lookup(f,reg) of
        NONE            => (reg, ResUnknownFlight)     (* 3a *)
      | SOME (seats,res,wl) =>  
         if Set.member(p,res) then
            let val res' = Set.delete(res,p) 
                val (res'',wl'') = 
                   case wl of
                      []        => (res',[])           (* 3b *)
                    | (p'::wl') => 
                          (Set.insert(p',res'),wl')    (* 3c *)
            in (Table.update(f,(seats,res'',wl''),reg),ResOK)
            end
         else                                          (* 3d *)
            let val wl' = List.filter (fn x => x <> p) wl
            in (Table.update(f,(seats,res,wl'),reg),ResOK)
       end )
  
| CmdListAll => (reg, ResAllList(Table.toList reg))    (* 4  *)
end;

