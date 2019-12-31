(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
(* note that List.filter need two parameters, not a tuple *)
(* use val binding instead of fun binding *)
val only_capitals = List.filter (fn s => Char.isUpper(String.sub(s, 0))) 

(* 2 *)
(* note that no `=` for fn because the current element is x and the previous result is y*)
(* http://sml-family.org/Basis/list.html#SIG:LIST.foldl:VAL *)
val longest_string1 = foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" 

(* 3 *)
val longest_string2 = foldl (fn (x,y) => if String.size(x) >=String.size(y) then x else y) "" 

(* 4 *)
(* do not forget the parentheses: (x::xs) *)
(* I think my code is worse than the sample solution, I should use foldl instead of pattern_case *) 
fun longest_string_helper f sl =
    case sl of
	[] => ""
      | (x::[]) => x
      | (x::y::xs) => if f(String.size(x), String.size(y)) then longest_string_helper f (y::xs) else longest_string_helper f (x::xs)
(* val-bindings *)
val longest_string3 = longest_string_helper (fn(x, y) => x < y)
val longest_string4 = longest_string_helper (fn(x, y) => x <= y)

(* 5 *)
val longest_capitalized = longest_string1  o only_capitals

(* 6 *)
val rev_string = implode o rev o explode

(* 7 *)
(* should use pattern_case intead of if...then...else... *)
fun first_answer f al =
    case al of
	[] => raise NoAnswer
      | (x::xs) => case (f x) of
		       NONE => first_answer f xs
		     | SOME y => y 

(* 8 *)
fun all_answers f al =
    let fun helper al acc =
	    case al of
		[] => SOME(acc)
	      | (x::xs) => if isSome(f x) then helper xs (valOf(f x) @ acc) else NONE
    in
	helper al []
    end
										     
		

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* 9a *)
(* use (fn () => 1) instead of (fn _ => 1) *)
val count_wildcards =  g (fn () => 1) (fn _ => 0)

(* 9b *)
(* use (String.size()) directly instead of (fn s => String.size(s)) because it is unnecessary function wrapping*)
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* 9c *)
fun count_some_var(s, p) = g (fn () => 0) (fn x => if x=s then 1 else 0) p

(* 10 *)
fun check_pat pat =
    let fun getlist pat=
	    case pat of
		Variable s => [s]
	      | TupleP ps => foldl (fn(x, y) => getlist(x) @ y) [] ps
	      | ConstructorP(_, p) => getlist(p)
	      | _ => []

	fun judgeduplicat [] = false
	  | judgeduplicat (x::xs) = (List.exists (fn y => x=y) xs) orelse (judgeduplicat xs)
    in
	not ((judgeduplicat o getlist) pat)
    end

(* 11 *)
(* do not forget the parentheses: (ListPair.zip(v, p)) *)
fun match (_, Wildcard) = SOME []
  | match (v, Variable s) = SOME [(s,v)]
  | match (Unit, UnitP) = SOME []
  | match (Const v, ConstP p) = if v=p then SOME [] else NONE
  | match (Tuple v, TupleP p) = if length v = length p then all_answers match (ListPair.zip(v, p)) else NONE
  | match (Constructor(s1, v), ConstructorP(s2, p)) = if s1=s2 then match(v, p) else NONE
  | match _ = NONE

(* 12 *)
fun first_match v pl =
    SOME(first_answer (fn p => match(v, p)) pl)
    handle NoAnswer => NONE

								       

		 
    
