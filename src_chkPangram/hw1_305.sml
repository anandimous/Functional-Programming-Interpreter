fun comp(ch : char, sentence : char list) =
  case sentence of
        [] => false
      | h::t => if(h = ch) then true
                else comp(ch, t)

fun checkPan(alph : char list, sentence : char list) =
  case alph of
      [] => true
      | h::t => if comp(h, sentence) then checkPan(t, sentence)
              else false

(*opened file using cmd line params as func. args*)
fun hw1(infile : string, outFile : string) =
let
    val inStream = TextIO.openIn infile
    val outStream = TextIO.openOut outFile
    val readLine = TextIO.inputLine inStream
    val xix = explode "abcdefghijklmnopqrstuvwxyz"

    fun helper(readLine : string option) =
      case readLine of
            NONE => (TextIO.closeIn inStream; TextIO.closeOut outStream)
       | SOME(c) => (TextIO.output(outStream, Bool.toString(checkPan(xix, explode(c)))^"\n");
          helper(TextIO.inputLine inStream))
in
    helper(readLine)
end
