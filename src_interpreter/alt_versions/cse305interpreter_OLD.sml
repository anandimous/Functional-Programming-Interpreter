datatype Val = INT of int | STR of string | BOOL of bool
datatype Ops = add | sub | mul | div | neg | swap

(* name should be independently handles i.e. not in the datatype Val. To check for a name, parse character by character from a string, if format does not match, error. *)

(* push method should be seperate wherein, parse with ' ' as delimiter and push accordingly *)
fun push(input, stack) =
      case input of


fun interpret(add, INT(x)::INT(y)::xs) = INT(x+y)::xs
  | interpret(add, z, stack) = (*push error*)
  | interpret(sub, INT(x)::INT(y)::xs) = INT(x-y)::xs
  | interpret(sub, z, stack) = (*push error*)
  | interpret(mul, INT(x)::INT(y)::xs) = INT(y-x)::xs
  | interpret(mul, z, stack) = (*push error*)
  | interpret(div, INT(x)::INT(y)::xs) = INT(y-x)::xs
  | interpret(div, z, stack) = (*push error*)
  | interpret(neg, INT(x)::xs) = INT(~x)::xs
  | interpret(neg, z, stack) = (*push error*)
  | interpret(swap, x::y::xs) = y::x::xs
  | interpret(swap, z, stack) = (*push error*)

fun main(infil, outfil) =
let
    val instream = TextIO.openIn infil
    val outstream = TextIO.openIn outfil
    val readLine = TextIO.inputLine instream

    fun helper(readLine: StringOption)
