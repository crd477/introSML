structure Table :> Table =
struct
   type ('a,'b) table = ('a * 'b) list
           (* [(a1,b1),...,(an,bn)] with ai <> aj for i <> j *)

   exception Table

   val empty   = []

   fun singleton(a,b) = [(a,b)]

   fun update(a,b,[])         = [(a,b)]
     | update(a,b,(a1,b1)::t) = if a = a1 then (a,b)::t 
                                else (a1,b1)::update(a,b,t)

   fun insert(a,b,[])         = SOME [(a,b)]
     | insert(a,b,(a1,b1)::t) = if a = a1 then NONE
                                else case insert(a,b,t) of
                                          NONE    => NONE 
                                        | SOME t1 => SOME((a1,b1)::t1)

   fun fromList []           = []
     | fromList ((a,b)::abs) = case insert(a,b,fromList(abs)) of
                                       NONE   => raise Table
                                     | SOME t => t

   fun toList t = t

   fun getval(a,[])         = raise Table
     | getval(a,(a1,b1)::t) = if a = a1 then b1 else getval(a,t)

   fun lookup(a,[])         = NONE
     | lookup(a,(a1,b1)::t) = if a = a1 then SOME b1 else lookup(a,t)

   fun isKey(a,t) = List.exists (fn (a1,_) => a1 = a) t

   fun delete(a,[])         = []
     | delete(a,(a1,b1)::t) = if a = a1 then t
                              else (a1,b1)::delete(a,t)

   fun remove(a,[])         = NONE
     | remove(a,(a1,b1)::t) = if a = a1 then SOME t
                              else case remove(a,t) of
                                       NONE   => NONE 
                                     | SOME t1 => SOME((a1,b1)::t1)
   
   fun map f t = List.map (fn (a,b) => (a, f(a,b))) t

   fun filter p t = List.filter p t

   fun exists p t = List.exists p t

   fun all p t    = List.all p t

   fun fold f e t = List.foldl (fn ((a,b),c) => f(a,b,c)) e t

   fun split []        = NONE
     | split((a,b)::t) = SOME(a,b,t)

   fun find p []         = NONE
     | find p ((a,b)::t) = if p(a,b) then SOME (a,b) else find p t
end
