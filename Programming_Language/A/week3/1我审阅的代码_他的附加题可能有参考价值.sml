(* Matt Winter HW2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* 1 a *)
fun all_except_option (s1, ss) =
  let
    fun aux(s1, ss, acc) =
      case ss of
           [] => NONE
         | x::ss' => if same_string(x,s1)
            then SOME(acc @ ss')
            else aux(s1, ss', x::acc)
  in
    aux(s1,ss,[])
  end

(* 1 b *)
fun get_substitutions1(subs, s) =
  case subs of
       [] => []
     | x::subs' =>
         case all_except_option(s, x) of
              NONE => get_substitutions1(subs', s)
            | SOME(l) => l @ get_substitutions1(subs', s)

(* 1 c *)
fun get_substitutions2(subs, s) =
  let
    fun aux(subs, s, acc) =
      case subs of
           [] => acc
         | x::subs' => case all_except_option(s, x) of
                            NONE => aux(subs', s, acc)
                          | SOME(l) => aux(subs', s, acc @ l)
  in
    aux(subs, s, [])
  end

(* 1 d *)

fun similar_names(subs, {first=first, middle=middle, last=last}) =
  let
    fun replace_first(subs) =
      case subs of
           [] => []
         | n::subs' => {first=n, middle=middle,last=last}::replace_first(subs')
  in
    replace_first(first::get_substitutions2(subs, first))
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

(*2 a *)
fun card_color (card) =
  case card of
       (Clubs,_) => Black
     | (Spades,_) => Black
     | _ => Red

(* 2 b *)
fun card_value (card) =
  case card of
       (_,Ace) => 11
     | (_,Num(i)) => i
     | _ => 10

(* 2 c *)
(* This is the same as from problem 1a, but we use '=' instead of same_string to
 * allow comparsion of cards. Problem 1a specified that we use same_string,
 * otherwise I would have done it this way for that problem. *)
fun all_except_option_poly (s1, ss) =
  let
    fun aux(s1, ss, acc) =
      case ss of
           [] => NONE
         | x::ss' => if x=s1
            then SOME(acc @ ss')
            else aux(s1, ss', x::acc)
  in
    aux(s1,ss,[])
  end

fun remove_card(cs, c, e) =
  case all_except_option_poly(c, cs) of
       NONE => raise e
     | SOME(l) => l

(* 2 d *)
fun all_same_color(cs) =
  case cs of
       [] => true
     | _::[] => true
     |  c1::c2::cs' =>
       case (card_color(c1), card_color(c2)) of
            (Red,Red) => all_same_color(c2::cs')
          | (Black,Black) => all_same_color(c2::cs')
          | _ => false

(* 2 e *)
fun sum_cards(cs) =
  let
    fun aux(cs, acc) =
      case cs of
           [] => acc
         | c::cs' => aux(cs', acc + card_value(c))
  in
    aux(cs, 0)
  end

(* 2 f *)
fun score(held, goal) =
  let
    val sum = sum_cards(held)
    val prelim = if sum > goal then 3*(sum-goal) else goal-sum
  in
    if all_same_color(held) then prelim div 2 else prelim
  end

(* 2 g *)

fun officiate(cs, moves, goal) =
  let
    fun next_move(cs, held, moves) =
      case moves of
           [] => score(held, goal)
         | Discard(c)::moves' => next_move(cs, remove_card(held, c, IllegalMove),
           moves')
         | Draw::moves' => case cs of
                                [] => score(held, goal)
                              | c::cs' =>
                                if sum_cards(c::held) > goal
                                then score(c::held, goal)
                                else next_move(cs', c::held, moves')
  in
    next_move(cs, [], moves)
  end

(* ******** CHALLENGE PROBLEMS ******** *)

fun map(f, l) =
  case l of
        [] => []
      | v::l' => (f v)::map(f, l')

fun add_v(v, is) =
  case is of
       [] => []
     | i::is' => (i+v)::add_v(v, is')

fun sum_cards_challenge(held) =
  case held of
       [] => [0]
     | (_,Ace)::held' =>
         let
           val tail_sums = sum_cards_challenge(held')
         in
           add_v(1, tail_sums) @ add_v(11, tail_sums)
         end
     | card::held' => add_v(card_value(card), sum_cards_challenge(held'))

fun min(is) =
  case is of
        [] => raise Empty
      | [a] => a
      | a::b::is' => Int.min(a,min(b::is'))

(* 3 a *)
fun score_challenge(held, goal) =
  let
    val color_bonus = if all_same_color(held) then 2 else 1
    fun calc_score (sum) =
      if sum > goal
      then (3*(sum-goal)) div color_bonus
      else (goal-sum) div color_bonus
  in
    min(map(calc_score, sum_cards_challenge(held)))
  end

(* sum_cards_challenge returns a list of possible sums,
 * so when checking for end game trigger, just compare the min
 * possible sum against the goal. *)
fun officiate_challenge(cs, moves, goal) =
  let
    fun next_move(cs, held, moves) =
      case moves of
           [] => score_challenge(held, goal)
         | Discard(c)::moves' => next_move(cs, remove_card(held, c, IllegalMove),
           moves')
         | Draw::moves' => case cs of
                                [] => score_challenge(held, goal)
                              | c::cs' =>
                                if min(sum_cards_challenge(c::held)) > goal
                                then score_challenge(c::held, goal)
                                else next_move(cs', c::held, moves')
  in
    next_move(cs, [], moves)
  end


(* 3 b *)

fun careful_player(cs, goal) =
  let
    fun rev(ms, acc) =
      (* We construct our list of moves in reverse order, so
       * when we're done, we need to return them reversed *)
      case ms of
           [] => acc
         | m::ms' => rev(ms', m::acc)      

    fun do_discard(card, cs, held, moves) =
      (cs, remove_card(held, card, IllegalMove), Discard(card)::moves)

      (* This takes a list of possible states reached by making a move,
       * and a "do nothing else" state as an initial "best choice".
       * It picks the one with the best score, and the fewest moves. *)
    fun best_score(results, {score=s, moves=m}) =
      case results of
           [] => {score=s,moves=m}
         | {score=rs, moves=rm}::results' =>
             if rs < s orelse (rs=s andalso length rm < length m)
             then best_score(results', {score=rs, moves=rm})
             else best_score(results', {score=s, moves=m})

    fun next_move(cs, held, moves) =
      let
        val cur_score = score(held, goal)
        val cur_state = {score=cur_score, moves=moves}
      in
        (* "If a score of 0 is reached, there must be no more moves." *)
        if cur_score = 0 then cur_state else
          case cs of
               (* "you should (attempt to) draw even if no cards remain" *)
               [] => {score=cur_score, moves=Draw::moves}
             | c::cs' =>
                 if goal > 10 + sum_cards(held)
                 (* We are required to draw if "goal is more than 10 greater
                  * than the value of the held cards". Even if that's
                  * not optimal play :( *)
                 then next_move(cs', c::held, Draw::moves)

                 else if sum_cards(c::held) <= goal
                 (* Depending on the color of the cards, sometimes drawing 
                  * is not the right thing to do. It could result in a
                  * worse score. E.g.:
                  * [(Spades,Num 5), (Hearts,Num 2)], goal = 14
                  *   Draw,Draw -> score =  14 - 7    = 7
                  *   Draw      -> score = (14 - 5)/2 = 4
                  *)
                 then
                   best_score([next_move(cs', c::held, Draw::moves)],
                   cur_state)
                 else
                   (* Try discarding each held card, and see where we end up.
                    * Pick the path that results in the best score.
                    *)
                   let
                     fun try_discard(c) =
                       next_move(do_discard(c,cs,held,moves))
                   in
                     best_score(map(try_discard, held), cur_state)
                   end
      end
    val {score=s, moves=m} = next_move(cs, [], [])
  in
    rev(m, [])
  end

