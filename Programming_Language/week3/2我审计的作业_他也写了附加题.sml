(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
 fun all_except_option(tar, slist) = 
    case slist of
        [] => NONE
        | h::rst => if same_string(tar, h) 
                        then SOME rst 
                    else 
                        case all_except_option(tar, rst) of
                            NONE => NONE
                            | SOME rsp => SOME (h::rsp)

fun get_substitutions1(substitutions, tar) = 
    case substitutions of 
        [] => []
        | someList::rst => case all_except_option(tar, someList) of
                                NONE => get_substitutions1(rst, tar)
                                | SOME rsp => rsp@get_substitutions1(rst, tar)

fun get_substitutions2(substitutions, tar) = 
    let fun substitute(slists, ans) = 
        case slists of
            [] => ans
            | slist::rst => case all_except_option(tar, slist) of
                                NONE => substitute(rst, ans)
                                | SOME rsp => substitute(rst, ans@rsp)
    in
        substitute(substitutions, [])
    end

fun similar_names(substitutions, {first=fr, middle=mi, last=la}) = 
    let fun subsitute(slist) = 
        case slist of 
            [] => []
            | hs::rst => {first=hs, last=la, middle=mi}::subsitute(rst)
    in
        {first=fr, last=la,middle=mi}::subsitute(get_substitutions2(substitutions, fr))
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

fun card_color(c, _) =
    case c of
        Clubs => Black
        | Spades => Black
        | _ => Red

fun card_value(_, r) =
    case r of
        Ace => 11
        | Num(i) => i
        | _ => 10

fun remove_card(cs, c, e) = 
    case cs of 
        [] => raise e
        | hc::rst => if hc = c then rst else hc::remove_card(rst, c, e)

fun all_same_color cs = 
    case cs of
        [] => true
        | _::[] => true
        | c1::(c2::cs) => card_color(c1)=card_color(c2) andalso all_same_color(c2::cs)

fun sum_cards cs = 
    let fun sum(clist, ans) = 
        case clist of 
            [] => ans
            | c::rst => sum(rst, ans+card_value(c))
    in
        sum(cs, 0)
    end

fun score(cs, goal) = 
    let fun preScroe() = 
        let val totalScore = sum_cards(cs)
        in 
            if totalScore > goal then 3*(totalScore-goal)
            else goal-totalScore
        end
    in
        if all_same_color cs then preScroe() div 2 else preScroe()
    end

fun officiate(cardList, moveList, goal) = 
    let fun run(cardList, heldList, moveList) = 
        case moveList of
            [] => score(heldList, goal)            
            | (Discard dc)::rstMove => run(cardList, remove_card(heldList, dc, IllegalMove), rstMove)
            | Draw::rstMove => case cardList of
                                    [] => score(heldList, goal)
                                    | hc::rstList => if sum_cards(hc::heldList) > goal then score(hc::heldList, goal) else run(rstList, hc::heldList, rstMove)
    in run(cardList, [], moveList)
    end

fun score_challenge(cs, goal) = 
    let fun sumAll(cs, ans) = 
        case cs of
            [] => if ans > goal then 3*(ans-goal) else goal-ans
            | (_, Ace)::rst => let 
                            val ace11 = sumAll(rst, 11+ans)
                            val ace1 = sumAll(rst, 1+ans)
                          in Int.min(ace11, ace1)
                          end

            | (_, Num(i))::rst => sumAll(rst, i+ans)
            | _ :: rst => sumAll(rst, 10+ans)
    in
        if all_same_color cs then sumAll(cs, 0) div 2 else sumAll(cs, 0)
    end

fun officiate_challenge(cardList, moveList, goal) = 
    let fun run(cardList, heldList, moveList) = 
        case moveList of
            [] => score_challenge(heldList, goal)            
            | (Discard dc)::rstMove => run(cardList, remove_card(heldList, dc, IllegalMove), rstMove)
            | Draw::rstMove => case cardList of
                                    [] => score_challenge(heldList, goal)
                                    | hc::rstList => if sum_cards(hc::heldList) > goal then score_challenge(hc::heldList, goal) else run(rstList, hc::heldList, rstMove)
    in run(cardList, [], moveList)
    end


fun careful_player(cardList, goal) = 
    let fun chooseEqualCard (cardList, cval) =
            case cardList of
                [] => NONE
                | hd::rst => if card_value(hd) = cval then SOME hd else chooseEqualCard(rst, cval)

        fun choose(cardList, heldList) = 
            let val allValue = sum_cards heldList
            in
                if goal > 10 + allValue then
                    case cardList of
                        [] => [Draw]
                        | hc::rst=> Draw::choose(rst, hc::heldList)
                else if goal = allValue then []
                else case cardList of
                        [] => []
                        | hc::rst => 
                            let val hdValue = card_value hc
                                val expectVal = allValue + hdValue - goal
                                val possibleCard = chooseEqualCard(cardList, expectVal)
                            in
                                case possibleCard of                                    
                                    SOME target => [Discard target, Draw]
                                    | NONE => if hdValue+allValue <= goal then Draw::choose(rst, hc::heldList) 
                                                else case heldList of
                                                    [] => []
                                                    | heldHead::heldRst => (Discard heldHead)::choose(cardList, heldRst)
                            end
            end

    in
        choose(cardList, [])
    end
                                


        

        

    

        
