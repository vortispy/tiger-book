type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun max x y = if x >= y then x else y;
fun maxlist (x::xs) = max x (maxlist xs)
  | maxlist [] = 0;

fun maxargs (CompoundStm (s1, s2)) = max (maxargs s1) (maxargs s2)
  | maxargs (AssignStm (_, e)) = maxargs_exp e
  | maxargs (PrintStm es) =  max (length es) (maxlist (map maxargs_exp es))
and maxargs_exp (OpExp (e1, _, e2)) = max (maxargs_exp e1) (maxargs_exp e2)
  | maxargs_exp (EseqExp (s, e)) = max (maxargs s) (maxargs_exp e)
  | maxargs_exp _ = 0;

type table = (id * int) list

fun lookup (t, ident) =
  let val (name, value) = hd t
    val rest_t = tl t;
  in if name = ident then value else lookup (rest_t, ident)
  end

fun binopFun Plus = op +
  | binopFun Minus = op -
  | binopFun Times = op *
  | binopFun Div = op div;

fun interpStm (CompoundStm (s1, s2), t) = interpStm (s2, (interpStm (s1, t)))
  | interpStm (AssignStm (ident, expVal), t) =
    let val (ret, new_table) = interpExp (expVal, t)
    in  (ident, ret)::new_table
    end
  | interpStm ((PrintStm es), t) =
    let fun expPrint (e, t) =
      let val (n, new_table) = interpExp (e, t)
       in print (Int.toString n);
        print " ";
        new_table
      end
    in print "\n";
      List.foldl expPrint t es
    end
and interpExp ((IdExp ident), t) = (lookup (t, ident), t)
  | interpExp ((NumExp n), t) = (n, t)
  | interpExp ((OpExp (e1, bop, e2)), t) =
    let val (v1, t1) = interpExp (e1, t)
      val (v2, t2) = interpExp (e2, t1)
    in ((binopFun bop) (v1,v2), t2)
    end
  | interpExp ((EseqExp (s, e)), t) =
    let val new_table = interpStm (s, t)
    in interpExp (e, new_table)
    end;

fun interp s =
  let val t = interpStm (s, [])
  in ()
  end;
