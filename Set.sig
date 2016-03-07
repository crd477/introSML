signature Set = 
sig 
   type 'a set 
   val empty     : ''a set 
   val singleton : ''a               -> ''a set 
   val insert    : ''a * ''a set     -> ''a set 
   val union     : ''a set * ''a set -> ''a set 
   val inter     : ''a set * ''a set -> ''a set 
   val delete    : ''a set * ''a     -> ''a set 
   val diff      : ''a set * ''a set -> ''a set
   val fromList  : ''a list -> ''a set
   val toList    : ''a set -> ''a list 
   val member    : ''a * ''a set     -> bool
   val subset    : ''a set * ''a set -> bool
   val equal     : ''a set * ''a set -> bool 
   val card      : ''a set -> int
   val filter    : (''a -> bool) -> ''a set -> ''a set
   val exists    : (''a -> bool) -> ''a set -> bool
   val all       : (''a -> bool) -> ''a set -> bool
   val find      : (''a -> bool) -> ''a set -> ''a option
   val map       : (''a -> ''b) -> ''a set -> ''b set
   val fold      : (''a * 'b -> 'b) -> 'b -> ''a set -> 'b 
   val split     : ''a set -> (''a * ''a set) option 
end;
