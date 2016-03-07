fun readIntLine(instrm) = 
    Int.fromString(TextIO.inputLine(instrm));


fun readIntLine(instrm) = 
    Int.fromString(TextIO.inputLine(instrm))
       handle Overflow => NONE ;


fun nowStr() = Date.fmt "%c" (Date.fromTimeLocal(Time.now()));

TextIO.output(TextIO.stdOut,nowStr()^"\n");

String.tokens (fn x => x = #";") "abc;de f;;;;012";

String.tokens Char.isSpace "123     monkey  1.24";

fun readIntAndReal(instr) =
 (case String.tokens Char.isSpace (TextIO.inputLine(instr))
  of
    [t1,t2] => (case (Int.fromString t1, Real.fromString t2) 
                of
                  (SOME n, SOME r) => SOME(n,r) 
                | _                => NONE     )
  | _       =>  NONE)
 handle Overflow => NONE;

fun readTwoInts(prmpt,errmsg,p) =
   let fun repeat() = 
       (TextIO.output(TextIO.stdOut,errmsg^"{\ttbsl}n") 
       ; readTwoInts(prmpt,errmsg,p))
   in
        TextIO.output(TextIO.stdOut,prmpt)
      ; TextIO.flushOut(TextIO.stdOut)
      ; case map Int.fromString 
                 (String.tokens Char.isSpace 
                     (TextIO.inputLine(TextIO.stdIn))) 
        of   [SOME x, SOME y] => if p(x,y) then (x,y) 
                                           else repeat()
           | _                => repeat()
    end; 

