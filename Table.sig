signature Table = 
sig
   type ('a,'b) table
   exception Table
   val empty    :  (''a,'b) table   
   val singleton: ''a * 'b -> (''a,'b) table 
   val update   : ''a * 'b * (''a,'b) table -> (''a,'b) table
   val insert   :  ''a * 'b * (''a,'b) table  
                                     -> (''a,'b) table option
   val delete   : ''a * (''a,'b) table -> (''a,'b) table
   val remove   : ''a * (''a,'b) table -> (''a,'b) table option
   val fromList : (''a * 'b) list -> (''a,'b) table
   val toList   : (''a,'b) table -> (''a * 'b) list
   val getval   : ''a * (''a,'b) table -> 'b
   val lookup   : ''a * (''a,'b) table -> 'b option
   val isKey    : ''a * (''a,'b) table -> bool
   val map      : (''a * 'b -> 'c) -> (''a,'b) table 
                                              -> (''a,'c) table
   val filter   : (''a * 'b -> bool) -> (''a,'b) table 
                                              -> (''a,'b) table
   val exists   : (''a * 'b -> bool) -> (''a,'b) table -> bool
   val all      : (''a * 'b -> bool) -> (''a,'b) table -> bool
   val fold     : (''a * 'b *'c -> 'c) -> 'c 
                                       ->  (''a, 'b) table -> 'c
   val split    : (''a,'b) table -> 
                  (''a * 'b * (''a,'b) table) option
   val find     : (''a * 'b -> bool) -> (''a,'b) table 
                                            -> (''a * 'b) option
end
