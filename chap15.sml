open TextIO;

fun prelude(min,max) =
   (  output(stdOut,"Think of a number between ")
    ; output(stdOut,Int.toString(min))
    ; output(stdOut," and ")
    ; output(stdOut,Int.toString(max))
    ; output(stdOut,"{\ttbsl}n")
    ; flushOut(stdOut)
   ) ;

fun question(n) =
   (  output(stdOut,"Is the number <= ")
    ; output(stdOut,Int.toString(n))
    ; output(stdOut," ? ")
    ; flushOut(stdOut) 
   ) ;

fun okmsg(n) = (  output(stdOut,"The number is ")
                ; output(stdOut,Int.toString(n))
                ; output(stdOut,"{\ttbsl}n")
                ; flushOut(stdOut)
               ) ;

datatype answer = Yes | No;

exception Quiz;

fun answer() = case inputLine(stdIn) of
                   "yes{\ttbsl}n" => Yes
                 | "no{\ttbsl}n"  => No
                 | _       => raise Quiz 

fun quiz(min,max) = ( prelude(min,max) ; A(min,max) )

and A(min,max)    = if  min >= max 
                    then okmsg(min)
                    else ( question((min+max) div 2) ; 
                           B(min, (min+max) div 2, max) ) 

and B(min,t,max)  = case answer() of 
                        Yes => A(min,t)
                      | No  => A(t+1,max) ;

fun errmsg() = (  output(stdOut, "Please answer yes or no{\ttbsl}n")
                ; flushOut(stdOut)
               )

fun inLine() = inputLine(stdIn)

fun answer() = case inLine() of
                   "yes{\ttbsl}n" => C(SOME Yes)
                 | "no{\ttbsl}n"  => C(SOME No)
                 | _       => C(NONE)

and C (SOME Yes) = Yes                    
  | C (SOME No)  = No                     
  | C NONE       = ( errmsg() ; answer() )


fun answer() = case inputLine(stdIn) of
                   "yes{\ttbsl}n" => Yes
                 | "no{\ttbsl}n"  => No
                 | _       => ( errmsg() ; answer() );  
