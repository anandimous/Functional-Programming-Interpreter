datatype Types =  INT of int | STR of string | NAME of string | BOOL of bool | ADD of Types*Types | SUB of Types*Types | MUL of Types*Types | DIV of Types*Types |
                  REM of Types*Types | AND of Types*Types | OR of Types*Types | NOT of Types | EQUAL of Types*Types | LESSTHAN of Types*Types | IF of Types*Types*Types;

fun namestr xs =
  let
      val isname = explode xs
  in
      if Char.isAlpha(hd (isname)) = true then true
      else
        false
  end

fun numstr xs = List.all (Char.isDigit) (explode xs)

fun typedef (INT num) = INT num
  | typedef (STR str) = STR str
  | typedef (NAME name) = NAME name
  | typedef (BOOL boolean) = BOOL boolean

fun compute (ADD (inp1, inp2)) =
    let
        val INT num1 = typedef inp1
        val INT num2 = typedef inp2
    in
        INT(num1+num2)
    end
   | compute (SUB (inp1, inp2)) =
    let
    	val INT num1 = typedef inp1
    	val INT num2 = typedef inp2
    in
    	INT(num1-num2)
    end
    | compute (MUL (inp1, inp2)) =
    let
      val INT num1 = typedef inp1
      val INT num2 = typedef inp2
    in
      INT(num1*num2)
    end
    | compute (DIV (inp1,inp2)) =
    let
    	val INT num1 = typedef inp1
    	val INT num2 = typedef inp2
    in
    	INT(num1 div num2)
    end
    | compute (REM (inp1,inp2)) =
    let
    	val INT num1 = typedef inp1
    	val INT num2 = typedef inp2
    in
    	INT(num1 mod num2)
    end
    | compute (AND (inp1,inp2)) =
    let
      val BOOL foo1 = typedef inp1
      val BOOL foo2 = typedef inp2
    in
      if foo1 = true andalso foo2 = true then BOOL(foo1)
      else
        BOOL(false)
    end
    | compute (OR (inp1,inp2)) =
    let
      val BOOL foo1 = typedef inp1
      val BOOL foo2 = typedef inp2
    in
      if foo1 = false andalso foo2 = false then BOOL(foo1)
      else
        BOOL(true)
    end
    | compute (NOT (inp1)) =
    let
      val BOOL foo1 = typedef inp1
    in
      BOOL(not foo1)
    end
    | compute (EQUAL (inp1,inp2)) =
    let
      val INT foo1 = typedef inp1
      val INT foo2 = typedef inp2
    in
      if foo1 = foo2 then BOOL(true)
      else
        BOOL(false)
    end
    | compute (LESSTHAN (inp1,inp2)) =
    let
      val INT foo1 = typedef inp1
      val INT foo2 = typedef inp2
    in
      if foo1 < foo2 then BOOL(true)
      else
        BOOL(false)
    end
    | compute (IF (inp1,inp2, inp3)) =
    let
      val INT foo1 = typedef inp1
      val INT foo2 = typedef inp2
      val BOOL foo3 = typedef inp3
    in
      if foo3 = true then INT(foo1)
      else
        INT(foo2)
    end

fun parseList (infile : string) =
let
	val infil = TextIO.openIn infile
  	fun list_app infil =
   		case TextIO.inputLine infil of
      			SOME line => line :: list_app infil
    			| NONE      => []
in
	list_app infil before TextIO.closeIn infil
end

fun process ([], stack : Types list) = stack | process (in_str::lines , stack : Types list) =
	if String.isSubstring ":error:" in_str then process(lines, STR(":error:")::stack)
	else if String.isSubstring ":true:" in_str then process(lines, BOOL(true)::stack)
	else if String.isSubstring ":false:" in_str then process(lines, BOOL(false)::stack)
	else if String.isSubstring "push" in_str then
		let
			val len = size in_str
			val y = substring(in_str, 5, len - 6)
      val len_y = size y
      val sub_y = substring(y, 1, len_y - 1)
		in
      if String.isSubstring "." y then process(lines, STR(":error:")::stack)
      else
        if String.isSubstring "-" y andalso numstr sub_y = false then process(lines, STR(":error:")::stack)
        else
          if numstr y = true orelse String.isSubstring "-" y then
              let
                val SOME x = Int.fromString(y)
              in
                process(lines, INT(x)::stack)
              end
          else
            if String.isPrefix "\"" y = true then process(lines, STR(y)::stack)
            else
              if namestr y = true then process(lines, NAME(y)::stack)
              else
                process(lines, STR(":error:")::stack)
		end
	else if String.isSubstring "pop" in_str then
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
	else if String.isSubstring "add" in_str then
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
    						(INT x, INT y) => process(lines, compute(ADD(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if String.isSubstring "sub" in_str then
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
    						(INT x, INT y) => process(lines, compute(SUB(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if String.isSubstring "neg" in_str then
		let
			val len = length(stack)
		in
			if (len = 0) then process(lines, STR(":error:")::stack)
			else
				let
					val x = hd (stack)
				in
					case x of
    						INT x => process(lines, compute(MUL(INT(x),INT(~1)))::tl(stack))
    						| _ => process(lines, STR(":error:")::stack)
				end
		end
	else if String.isSubstring "mul" in_str then
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
    						(INT x, INT y) => process(lines, compute(MUL(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if String.isSubstring "div" in_str then
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
    						else process(lines, compute(DIV(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
	else if String.isSubstring "rem" in_str then
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
    						else process(lines, compute(REM(INT(x),INT(y)))::(tl (tl stack)))
    						| (_, _) => process(lines, STR(":error:")::stack)
				end
		end
  else if String.isSubstring "and" in_str then
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
                (BOOL x, BOOL y) => process(lines, compute(AND(BOOL(x),BOOL(y)))::(tl (tl stack)))
                | (_, _) => process(lines, STR(":error:")::stack)
        end
    end
  else if String.isSubstring "or" in_str then
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
                (BOOL x, BOOL y) => process(lines, compute(OR(BOOL(x),BOOL(y)))::(tl (tl stack)))
                | (_, _) => process(lines, STR(":error:")::stack)
        end
    end
  else if String.isSubstring "not" in_str then
    let
      val len = length(stack)
    in
      if (len = 0) then process(lines, STR(":error:")::stack)
      else
        let
          val x = hd (stack)
        in
          case (x) of
                (BOOL x) => process(lines, compute(NOT(BOOL(x)))::(tl (tl stack)))
                | (_) => process(lines, STR(":error:")::stack)
        end
    end
  else if String.isSubstring "equal" in_str then
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
                (INT x, INT y) => process(lines, compute(EQUAL(INT(x),INT(y)))::(tl (tl stack)))
                | (_, _) => process(lines, STR(":error:")::stack)
        end
    end
  else if String.isSubstring "lessThan" in_str then
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
                (INT x, INT y) => process(lines, compute(LESSTHAN(INT(x),INT(y)))::(tl (tl stack)))
                | (_, _) => process(lines, STR(":error:")::stack)
        end
    end
  else if String.isSubstring "if" in_str then
    let
      val len = length(stack)
    in
      if ((len = 2) orelse (len = 1) orelse (len = 0)) then process(lines, STR(":error:")::stack)
      else
        let
          val x = hd (stack)
          val y = hd (tl (stack))
          val z = hd (tl (tl (stack)))
        in
          case (x,y,z) of
                (INT x, INT y, BOOL z) => process(lines, compute(IF(INT(x),INT(y),BOOL(z)))::(tl (tl (tl stack))))
                | (_, _, _) => process(lines, STR(":error:")::stack)
        end
    end
	else if String.isSubstring "swap" in_str then
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
                | (STR x, BOOL y) => process(lines, BOOL(y)::STR(x)::(tl (tl stack)))
                | (BOOL x, STR y) => process(lines, STR(y)::BOOL(x)::(tl (tl stack)))
                | (NAME x, BOOL y) => process(lines, BOOL(y)::NAME(x)::(tl (tl stack)))
                | (BOOL x, NAME y) => process(lines, NAME(y)::BOOL(x)::(tl (tl stack)))
                | (INT x, BOOL y) => process(lines, BOOL(y)::INT(x)::(tl (tl stack)))
                | (BOOL x, INT y) => process(lines, INT(y)::BOOL(x)::(tl (tl stack)))
				end
		end
  (*else if String.isSubstring "bind" in_str then*)
    (* TBD -------------------------------- TBD *)

    (* ಠ_ಠ *)

	else if String.isSubstring "quit" in_str then
		process([], stack)
	else stack

fun rmTilde(input : string) =
	if String.isSubstring "~" input then ("-" ^ substring(input, 1, size(input) - 1)) else input

fun interpreter(infil : string, outfil : string) =
	let
		val in_list = parseList(infil)
		val stack = process(in_list, [])
		val outStream = TextIO.openOut outfil
		fun helper([]) = (TextIO.closeOut outStream)
			| helper(a::oStack) =
    			case a of
    				INT a => (TextIO.output(outStream, rmTilde(Int.toString(a)) ^ "\n");
    				helper(oStack))
    				| STR a => (TextIO.output(outStream, a ^ "\n");
    				helper(oStack))
            | NAME a => (TextIO.output(outStream, a ^ "\n");
    				helper(oStack))
            | BOOL a => (TextIO.output(outStream, ":" ^ Bool.toString(a) ^ ":" ^ "\n");
    				helper(oStack))
	in
		helper(stack)
	end

val _ = interpreter("/home/anandi/Desktop/cse305interpreter/src_interpreter/in.txt","/home/anandi/Desktop/cse305interpreter/src_interpreter/out.txt");
