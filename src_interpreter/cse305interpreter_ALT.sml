open String;

datatype Types =  INT of int | STR of string | NAME of string | ADD of Types*Types | SUB of Types*Types | MUL of Types*Types | DIV of Types*Types | REM of Types*Types;

fun numstr xs = List.all (Char.isDigit) (explode xs)

(* NOT FULLY IMPLEMENTED *)
fun namestr xs =
  let
      val isname = explode xs
  in
      if Char.isAplha(hd (isname)) = true then true
      else
        false
  end

fun eval (INT num) = INT num
  | eval (STR str) = STR str
  | eval (ADD (e1, e2)) =
    let
        val INT n1 = eval e1
        val INT n2 = eval e2
    in
        INT (n1+n2)
    end
  | eval (MUL (e1, e2)) =
    let
        val INT n1 = eval e1
        val INT n2 = eval e2
    in
        INT (n1*n2)
    end
   | eval (SUB (e1, e2)) =
    let
    	val INT n1 = eval e1
    	val INT n2 = eval e2
    in
    	INT(n1-n2)
    end
    | eval (DIV (e1,e2)) =
    let
    	val INT n1 = eval e1
    	val INT n2 = eval e2
    in
    	INT(n1 div n2)
    end
    | eval (REM (e1,e2)) =
    let
    	val INT n1 = eval e1
    	val INT n2 = eval e2
    in
    	INT(n1 mod n2)
    end

fun readlist (infile : string) =
let
	val ins = TextIO.openIn infile
  	fun loop ins =
   		case TextIO.inputLine ins of
      			SOME line => line :: loop ins
    			| NONE      => []
in
	loop ins before TextIO.closeIn ins
end

fun process ([], stack : Types list) = stack | process (s::lines , stack : Types list) =
	if isSubstring ":error:" s then process(lines, STR(":error:")::stack)
	else if isSubstring ":true:" s then process(lines, STR(":true:")::stack)
	else if isSubstring ":false:" s then process(lines, STR(":false:")::stack)
	else if isSubstring "push" s then
		let
			val len = size s
			val y = substring(s, 5, len - 6)
		in
      if isSubstring "." y then process(lines, STR(":error:")::stack)
      else
        if numstr y = true orelse isSubstring "-" y then
            let
              val SOME x = Int.fromString(y)
            in
              process(lines, INT(x)::stack)
            end
        else
          if namestr y = true then process(lines, NAME(y)::stack)
          else
            process(lines, STR(y)::stack)
		end
	else if isSubstring "pop" s then
		let
			val len = length stack
		in
			if len = 0 then process(lines, STR(":error:")::stack)
			else
				let
					val restOfStack = tl stack
				in
					process(lines, restOfStack)
				end
		end
	else if isSubstring "add" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
			else
				let
					val x = hd (stack)
					val y = hd (tl (stack))
				in
					case (x,y) of
    						(INT x, INT y) => process(lines, eval(ADD(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if isSubstring "sub" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
			else
				let
					val y = hd (stack)
					val x = hd (tl (stack))
				in
					case (x,y) of
    						(INT x, INT y) => process(lines, eval(SUB(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if isSubstring "neg" s then
		let
			val len = length(stack)
		in
			if (len = 0) then process(lines, STR(":error:")::stack)
			else
				let
					val x = hd (stack)
				in
					case x of
    						INT x => process(lines, eval(MUL(INT(x),INT(~1)))::tl(stack))
    						| _ => process(lines, STR(":error:")::stack)
				end
		end
	else if isSubstring "mul" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
			else
				let
					val x = hd (stack)
					val y = hd (tl (stack))
				in
					case (x,y) of
    						(INT x, INT y) => process(lines, eval(MUL(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if isSubstring "div" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
			else
				let
					val y = hd (stack)
					val x = hd (tl (stack))
				in
					case (x,y) of
    						(INT x, INT y) =>
    						if (y = 0) then process(lines, STR(":error:")::stack)
    						else process(lines, eval(DIV(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if isSubstring "rem" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
			else
				let
					val y = hd (stack)
					val x = hd (tl (stack))
				in
					case (x,y) of
    						(INT x, INT y) =>
    						if (y = 0) then process(lines, STR(":error:")::stack)
    						else process(lines, eval(REM(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if isSubstring "swap" s then
		let
			val len = length(stack)
		in
			if ((len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
			else
				let
					val x = hd (stack)
					val y = hd (tl (stack))
				in
					case (x,y) of
    						(INT x, INT y) => process(lines, INT(y)::INT(x)::(tl (tl stack)))
    						| (INT x, STR y) => process(lines, STR(y)::INT(x)::(tl (tl stack)))
    						| (STR x, INT y) => process(lines, INT(y)::STR(x)::(tl (tl stack)))
                | (NAME x, INT y) => process(lines, INT(y)::NAME(x)::(tl (tl stack)))
                | (INT x, NAME y) => process(lines, NAME(y)::INT(x)::(tl (tl stack)))
                | (NAME x, STR y) => process(lines, STR(y)::NAME(x)::(tl (tl stack)))
                | (STR x, NAME y) => process(lines, NAME(y)::STR(x)::(tl (tl stack)))
                | (NAME x, NAME y) => process(lines, NAME(y)::NAME(x)::(tl (tl stack)))
    						| (STR x, STR y) => process(lines, STR(y)::STR(x)::(tl (tl stack)))
				end
		end
	else if isSubstring "quit" s then
		process([], stack)
	else stack

fun removeTilde(inp:string) =
	if isSubstring "~" inp then ("-" ^ substring(inp, 1, size(inp) - 1)) else inp

fun hw2(infile:string, outFile:string) =
	let
		val inp = readlist(infile)
		val stack = process(inp, [])
		val outStream = TextIO.openOut outFile
		fun helper([]) = (TextIO.closeOut outStream)
			| helper(a::newStack) =
			case a of
				INT a => (TextIO.output(outStream, removeTilde(Int.toString(a)) ^ "\n");
				helper(newStack))
				| STR a => (TextIO.output(outStream, a ^ "\n");
				helper(newStack))
        | NAME a => (TextIO.output(outStream, a ^ "\n");
				helper(newStack))
	in
		helper(stack)
	end

val _ = hw2("/home/anandi/Desktop/cse305interpreter/src_interpreter/in.txt","/home/anandi/Desktop/cse305interpreter/src_interpreter/out.txt")
