datatype 'a inflist = NIL
                    | CONS of 'a * (unit -> 'a inflist);

exception Empty;
exception Subscript;

fun HD (CONS(a,b)) = a
  | HD NIL = raise Empty;

fun TL (CONS(a,b)) = b()
  | TL NIL = raise Empty;

fun NUL NIL = true
  | NUL _ = false;

fun NTH 0 L = HD L
  | NTH n L = NTH (n-1) (TL L);

fun TAKE (xs, 0) = []
  | TAKE (NIL, n) = raise Subscript
  | TAKE (CONS(x, xf), n) = x::TAKE(xf(), n-1);

fun FROMN n = CONS(n, fn () => FROMN (n+1));

fun FIB n m = CONS(n, fn () => FIB m (n+m));

fun STUB _ = CONS(0, fn () => STUB 0)

fun FILTER f l =
  if NUL l
  then NIL
  else if f (HD l)
       then CONS(HD l, fn() => (FILTER f (TL l)))
       else FILTER f (TL l);

fun SIFT NIL = NIL
  | SIFT l =
     let val a = HD l
     in CONS(a, fn () => SIFT(FILTER (fn x => x mod a <> 0) (TL l)))
     end;


(**********************
 *
 * FUNCTION AND INFLIST STUBS -- YOU MUST IMPLEMENT THESE
 *
 * printList and printPairList must write to the file named by f.
 * Anything printed to the terminal will not be graded.
 *
 **********************)

fun even (x : int) : bool = if x mod 2 = 0 then true else false
fun odd  (x : int) : bool = if x mod 2 <> 0 then true else false

val fibs     = FIB 0 1;
val evenFibs = FILTER  (fn x => even(x) = true) fibs;
val oddFibs  = FILTER  (fn x => odd(x) = true) fibs;

fun printGenList (f : ('a -> 'b)) (l : ('a list)) : unit =
  case l of
    [] => ()
    | h::t => (f(h); printGenList f t)

fun printList (f : string, l : int list) : unit =
  let
    val outStream = TextIO.openOut f;
  in
    case l of
      [] => TextIO.closeOut outStream
      | _ => printGenList (fn a => TextIO.output(outStream, Int.toString(a) ^ " ")) l;
            TextIO.closeOut outStream
  end;

fun printPairList (f : string, l : (int * int) list) : unit =
  let
    val outStream = TextIO.openOut f;
  in
    case l of
      [] => TextIO.closeOut outStream
      | _ => printGenList (fn (a,b) => TextIO.output(outStream, "(" ^ Int.toString(a) ^ ", " ^ Int.toString(b) ^ ") ")) l;
            TextIO.closeOut outStream
  end;

fun ZIP (infL1 : 'a inflist, infL2 : 'b inflist) : ('a * 'b) inflist =
  let
    val infList1 = infL1;
    val infList2 = infL2;
  in
    case (infList1, infList2) of
      (NIL, NIL) => NIL
      | (NIL, _) => NIL
      | (_, NIL) => NIL
      | (CONS(h1, t1), CONS(h2, t2)) => CONS((h1,h2), fn sth => ZIP(t1(),t2()))
  end;
