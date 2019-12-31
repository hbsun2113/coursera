(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* hbsun:
1. tail-recursive
2. use case-pattern instead of if...then...else
3. now we can omit the type of fun parameter
*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
(* put your solutions for problem 1 here *)

(* Problem 1a *)
(* tail-recursive version *)
(* s: string, sl: string list *)
fun all_except_option(s, sl) =
    let fun reverse(sl, res) =
	    case sl of
		[] => res
	      | (x::xs) => reverse(xs, x::res)
	fun helper(sl, prefix) =
	    case sl of
		[] => NONE
	     |  (x::xs) => if(same_string(s, x)) then SOME(reverse(prefix, []) @ xs) else helper(xs, x::prefix)
    in
	helper(sl, [])
    end
	
(* Problem 1b *)
(* sll: string list list, s: string *)
fun get_substitutions1(sll, s) =
    case sll of
	[] => []
      | (sl::xsl) => case all_except_option(s, sl) of
			 NONE => get_substitutions1(xsl, s)
		       | SOME cur  =>  cur @ get_substitutions1(xsl, s)
		    
(* Problem 1c *)			 
fun get_substitutions2(sll, s) =
    let fun helper(sll, res) =
	    case sll of
		[] => res
	      | (sl::xsl) => case all_except_option(s, sl) of
				 NONE => helper(xsl, res)
			       | SOME cur => helper(xsl, res @ cur)	       
    in
	helper(sll, [])
    end

(* Problem 1d *)
fun similar_names(sll, {first=x, middle=y, last=z}) =
    let fun helper(sl) =
	    case sl of
		[] => []
	      | (s::xsl) => {first=s, middle=y, last=z} :: helper(xsl)
    in
	{first=x, middle=y, last=z}::helper(get_substitutions2(sll, x))
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove

(* put your solutions for problem 2 here *)

(* Problem 2a *)
(* s: suit, r: rank *)
fun card_color(s, r) =
    case s of 
	Clubs => Black
     |  Spades => Black
     |  Diamonds => Red
     |  Hearts => Red

(* Problem 2b *)
fun card_value(s, r) =
    case r of
	Ace => 11
      | Num(i) => i
      | _ => 10

(* Problem 2c *)
(* cs: card list, c: card, e *)
fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
     | (x::xs) => if x=c then xs else x::remove_card(xs, c, e)

(* Problem 2d *)
fun all_same_color(cs) =
    case cs of
	[] => true
      | _::[] => true
      | first::(second::xs) => (card_color(first)=card_color(second) andalso all_same_color(second::xs))

(* Problem 2e *)
fun sum_cards(cs) =
    let fun helper(cs, res) =
	    case cs of
		[] => res
	      | (x::xs) => helper(xs, res + card_value x)
    in
	helper(cs, 0)
    end

(* Problem 2f *)
fun score(cs, goal) =
    let val sum = sum_cards(cs)
        fun preliminary_score(sum, goal) = 
	    case (sum - goal > 0) of
		true => 3 * (sum - goal)
	      | false => goal - sum
    in
	case all_same_color(cs) of
	    true => preliminary_score(sum, goal) div 2
	  | false =>  preliminary_score(sum, goal)
    end

(* Problem 2g *)
fun officiate(cl, ml, goal) =
   let fun helper(cl, ml, goal, hl, cur) =
       case ml of
	   [] => score(hl, goal)
	 | (m::ms) => case m of
			  Discard(c) => helper(cl, ms, goal, remove_card(hl, c, IllegalMove), cur - card_value(c))
 			| Draw => case cl of
				      [] => score(hl, goal)
				    | (c::cs) => case(cur +  card_value(c) > goal) of
						     true => score(c::hl, goal)
   						   | false => helper(cs, ms, goal, c::hl, cur +  card_value(c))
   in
       helper(cl, ml, goal, [], 0)
   end
				     
    
			  
		       


