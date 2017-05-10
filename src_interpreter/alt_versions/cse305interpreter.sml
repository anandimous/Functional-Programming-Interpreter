fun push item (s : string list)         =  (item::s)
  fun pop ( first_item::s : string list) = (first_item, s)
fun isEmpty (s :string list)=
if s=[] then
 true
 else
  false

val v = []
val v = push "\n" v
val v = push "z" v
val v = push "\n" v
val v = push "z" v

fun is_Digit (n : string)=
let
 fun isDigit (n : string)=
let
  val intnum = valOf(Int.fromString(n))
  val realnum = valOf( Real.fromString(n))
  val realtest = Real.fromInt(intnum)

 in
   ( if Real.==(realnum,realtest) then
    true
 else
  false)
end
in
  if n = ":true:" orelse n = ":false:" orelse n = ":error:" then
   false
  else
   isDigit(n)
 end


  fun is_Push (c) = String.isSubstring "push" c;
  fun is_Pop (c) = String.isSubstring "pop" c;
  fun is_Add (c) = String.isSubstring "add" c;
  fun is_Sub (c) = String.isSubstring "sub" c;
  fun is_Swap (c) = String.isSubstring "swap" c;
  fun is_Mul (c) = String.isSubstring "mul" c;
  fun is_Div (c) = String.isSubstring "div" c;
  fun is_Rem (c) = String.isSubstring "rem" c;
  fun is_Neg (c) = String.isSubstring "neg" c;
  fun is_Quit (c) = String.isSubstring "quit" c;
  fun is_True (c) = String.isSubstring ":true:" c;
  fun is_False (c) = String.isSubstring ":false:" c;
  fun is_Error (c) = String.isSubstring ":error:" c;

fun Push1 (no : string ,v : string list)=
     let
       val v = push "\n" v
       val v = push no v

    in
       ("",v)
     end

fun Error ( v : string list) =
let
 val v = push "\n" v
  val v =push ":error:" v

 in
  ("",v)
end

fun Push (c : string, v : string list) =
let

  val b = String.size(c) + (~6)
val no = String.substring(c,5,b)


  in
   if is_Digit (no) then
     Push1 (no,v)
    else
    Error (v)
  end






fun Pop (v : string list) =
let
val a = List.nth(v,0)


fun Pop1 (v : string list) =
let
val (item,v) = pop v
val (item,v) = pop v
 in
  ("",v)
end

 in
  if (String.compare(a,"z") = EQUAL)    then
   Error (v)

   else
    Pop1(v)

end

fun Add (v : string list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Add1 (v : string list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val a1 = valOf(Int.fromString(a))
val b1 = valOf(Int.fromString(b))
val c = a1+b1
val c1 = Int.toString(c)
val v = push c1 v
in
("",v)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else if ((String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else
    Add1(v)

end

fun Sub (v : string list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Sub1 (v : string list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val a1 = valOf(Int.fromString(a))
val b1 = valOf(Int.fromString(b))
val c = b1 + (~a1)
val c1 = Int.toString(c)
val v = push c1 v
in
("",v)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else if ((String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else
    Sub1(v)

end

fun Mul (v : string list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Mul1 (v : string list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val a1 = valOf(Int.fromString(a))
val b1 = valOf(Int.fromString(b))
val c = a1*b1
val c1 = Int.toString(c)
val v = push c1 v
in
("",v)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else if ((String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else
    Mul1(v)

end



fun Div (v : string list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Div1 (v : string list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val a1 = valOf(Int.fromString(a))
val b1 = valOf(Int.fromString(b))
val c = b1 div a1
val c1 = Int.toString(c)
val v = push c1 v
in
("",v)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else if ((String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else
    Div1(v)

end

fun Rem (v : string list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Rem1 (v : string list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val a1 = valOf(Int.fromString(a))
val b1 = valOf(Int.fromString(b))
val c = b1 mod a1
val c1 = Int.toString(c)
val v = push c1 v
in
("",v)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else if ((String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)) orelse (is_Digit (a) = false) orelse (is_Digit (b) = false) then
   Error (v)
   else
    Rem1(v)

end

fun Swap (v : string list) =
let
val a = List.nth(v,0)

val b = List.nth(v,2)


fun Swap1 (v : string list) =
let
val (a,v) = pop v
val (blank,v) = pop v
val (b,v) = pop v
val v = push a v
val v = push "\n" v
val v = push b v
in
("",v)
end
 in
  if ((String.compare(a,"z") = EQUAL) andalso (String.compare(b,"z") = EQUAL))  then
   Error (v)
   else if (String.compare(a,"z") <> EQUAL) andalso (String.compare(b,"z") = EQUAL)  then
   Error (v)
   else
    Swap1(v)

end



fun Neg (v : string list) =
let
val a = List.nth(v,0)


fun Neg1 (v : string list) =
let
val (a,v) = pop v

val a1 = valOf(Int.fromString(a))

val c = (~a1)
val c1 = Int.toString(c)
val v = push c1 v
in
("",v)
end
 in
  if (String.compare(a,"z") = EQUAL)  orelse (is_Digit (a) = false)  then
   Error (v)

   else
    Neg1(v)

end


fun Quit (v : string list) =
 let
 val final = concat v
  val b = String.size(final) + (~4)
val c = String.substring(final,0,b)

 in
    (c,v)
end



fun T (c : string, v : string list) =
let
 val b = String.size(c) + (~1)
val c = String.substring(c,0,b)
 val v = push "\n" v
  val v =push c v
 in
  ("",v)
end

fun F (c : string, v : string list) =
let
 val b = String.size(c) + (~1)
val c = String.substring(c,0,b)
val v = push "\n" v
  val v =push c v
 in
  ("",v)
end

fun main (line : string, v : string list) =

   if is_Push (line) then
     Push (line,v)
    else if is_Pop line then
     Pop (v)
     else if is_Add line then
     Add (v)
     else if is_Sub line then
     Sub (v)
     else if is_Div line then
     Div (v)
     else if is_Mul line then
     Mul (v)
     else if is_Rem line then
     Rem (v)

     else if is_Quit line then
      Quit(v)
     else if is_Error line then
     Error (v)
     else if is_Neg line then
      Neg (v)
     else if line ="" then
      ("",v)
      else if is_Swap line then
      Swap(v)
     else if is_True line then
      T (line,v)
      else
       F (line,v)

      fun test a =
      let
       val b = 2*a
       in
        "b"
        end

fun hw2(inputFile : string, outputFile : string) =
let
        val inStream = TextIO.openIn inputFile
        val outStream = TextIO.openOut outputFile
        val readLine = TextIO.inputLine inStream


        fun helper(readLine : string option , v :string list) =
        case readLine of
         NONE=>(TextIO.closeIn inStream; TextIO.closeOut outStream)
         |SOME(a)=>
         (let


         val (item,v) = main(valOf(readLine),v)

         in
                                (TextIO.output(outStream,item);helper(TextIO.inputLine inStream,v))
          end)


in
        helper(readLine,v)
end

val _ = hw2("/home/anandi/Desktop/cse305interpreter/src_interpreter/in.txt","/home/anandi/Desktop/cse305interpreter/src_interpreter/out.txt")
