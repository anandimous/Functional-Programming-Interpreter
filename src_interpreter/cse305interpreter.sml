datatype Val = INT of int | STR of string | BOOL of bool (*add more types as they become apparent*)

fun interpret(Push, stack) = stack::(*sth?*)
  | interpret(Add, INT(x)::INT(y)::xs) = INT(x+y)::xs
  | interpret(Add, z) = (*push error*)
