(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
infix |>
fun x |> f = f x
			       
fun only_capitals xs = List.filter (fn x => String.sub (x, 0) |> Char.isUpper) xs

fun longest_string1 xs = foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs

(* fun longest_string2 xs = xs |> List.rev |> longest_string1  *)
fun longest_string2 xs = foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs

fun longest_string_helper f xs = foldl (fn (x,y) => if f (String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
					    
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

fun rev_string s = (String.implode o rev o String.explode) s

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | h::t => case f h of
		    NONE => first_answer f t
		  | SOME v => v

fun all_answers f xs =
    let fun aux (xs', acc) =
	    case xs' of
		[] => SOME acc
	      | h::t => let val answer = f h
			in
			    case answer of
				NONE => NONE
			      | SOME v  => aux (t, acc @ v)
			end
    in
	aux (xs, [])
    end

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let fun has_duplicates (xs) =
	    case xs of
		[] => false
	      | h::t =>	if List.exists (fn x => x = h) t then true else has_duplicates t
	fun get_strings (p', acc) =
	    case p' of
		Variable x        => x::acc
	      | TupleP ps         => List.foldl (fn (p'',acc') => get_strings (p'', acc')) acc ps
	      | ConstructorP(_,p'') => get_strings (p'', acc)
	      | _                 => acc
    in
	has_duplicates (get_strings (p, [])) |> not
    end

fun match (v,p) =
    case (v, p) of
	(_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (Const v', ConstP i) => if v' = i then SOME [] else NONE
      | (v', Variable s) => SOME [(s,v')]
      | (Constructor (s1,v'), ConstructorP (s2,p')) => if s1 = s2 then match (v', p') else NONE
      | (Tuple vs, TupleP ps) => if (List.length vs = List.length ps) then all_answers match (ListPair.zip (vs, ps)) else NONE
    | _ => NONE

fun first_match v ps =
    (SOME (first_answer match (foldl (fn (x,acc) => (v,x)::acc) [] ps)) handle NoAnswer => NONE)
