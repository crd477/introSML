exception FindArticle;

fun  findArticle(ac, (ac',adesc)::reg) =
          if ac=ac' then adesc else findArticle(ac,reg)
  | findArticle _                      = raise FindArticle; 

fun makeBill([], _)             = ([],0)
  | makeBill((np,ac)::pur, reg) =
        let val (aname,aprice) = findArticle(ac,reg)
            val tprice         = np*aprice
            val (billtl,sumtl) = makeBill(pur,reg)
        in ((np,aname,tprice)::billtl, tprice+sumtl)
        end;

val register = 
   [("a1",("cheese",25)), 
    ("a2",("herring",4)),
    ("a3",("soft drink",5))
   ];

val pur = [(3,"a2"),(1,"a1")];

makeBill(pur,register);
