(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun same_card(c1 : card, c2 : card) =
    c1 = c2


(*
contains_string("mohamed", ["mohamed", "salah"]);             == true
contains_string("mohamed", ["fatma", "salah"]);               == false 
contains_string("mohamed", []);                               == false 
*)
fun contains_string(s1, ls2) =
    case ls2 of
      [] => false
      | s::ss => 
         if same_string(s1, s) then
            true
         else
            contains_string(s1, ss)

fun contains_card(c, cs) =
    case cs of
      [] => false
      | card::cards => 
         if same_card(card, c) then
            true
         else
            contains_card(c, cards)


fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append(xs',ys)

fun similar_name(ls, {first=x, middle=y, last=z} ) =
    case ls of
        [] => [{first=x, middle=y, last=z}]
        | s::s' => {first=s, middle=y, last=z}::similar_name(s', {first=x, middle=y, last=z})

(* put your solutions for problem 1 here *)



(* put your solutions for problem 2 here *)

fun all_except_option(a, b) =
   let fun aux(a, b, acc) =
        case b of
            [] => SOME(acc)
            | x::xs' => 
                case same_string(a, x) of 
                    true => aux(a, xs', acc)
                    |  _ => aux(a, xs', (acc) @ [x])
    in
      if contains_string(a, b) then
         aux(a, b, [])
      else
         NONE
   end      

(* 
use "hw2.sml"; 
all_except_option("mohamed", ["mohamed", "salah", "fatma"]);  == SOME ["salah","fatma"]
all_except_option("mohamed", ["salah", "mohamed", "fatma"]);  == SOME ["salah","fatma"]
all_except_option("mohamed", ["salah", "fatma", "mohamed"]);  == SOME ["salah","fatma"]
all_except_option("mohamed", []);                             == NONE
all_except_option("anas", ["salah", "fatma", "mohamed"]);     == NONE

*)

fun get_substitutions1(lls, s) = 
   case lls of
        [] => []
      | ls::lls' => 
        case all_except_option(s, ls) of
            NONE => get_substitutions1(lls', s)
            | SOME(lss) => lss @ get_substitutions1(lls', s);

(*
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred");  
answer: ["Fredrick","Freddie","F"] 
*)

(*
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff");
answer: ["Jeffrey","Geoff","Jeffrey"]
*)

fun get_substitutions2(lls, s) =
    let fun aux(lls, s, acc) =
        case lls of
            [] => acc
            | ls::lls' => 
                case all_except_option(s, ls) of 
                    NONE => aux(lls', s, acc)
                    | SOME(lss) => aux(lls', s, lss @ acc)

    in
        aux(lls, s, [])
    end

fun similar_names(lls, {first=x, middle=y, last=z}) = 
    let 
        val new_ls = get_substitutions2(lls, x)
    in 
        similar_name(new_ls, {first=x, middle=y, last=z})
    end

(*
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"})
answer: [{first="Fred", last="Smith", middle="W"},
        {first="Fredrick", last="Smith", middle="W"},
        {first="Freddie", last="Smith", middle="W"},
        {first="F", last="Smith", middle="W"}]
*)

fun card_color(c) = 
    case c of 
        (Spades, _) => Black
        | (Clubs, _) => Black
        | _ => Red

(*
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 
*)

fun card_value(c) = 
    case c of 
        (_, Ace) => 11
        | (_, Num n) => n
        | _ => 10

fun remove_card(cs, c, e) = 
    let fun aux(card_list, card, acc, removed: bool) =
        case removed of
            true => acc @ card_list
            | _ => 
                case card_list of
                    [] => acc
                    | x::xs' => 
                    case same_card(card, x) of 
                        true => aux(xs', card, acc, true)
                        |  _ => aux(xs', card, x::acc, false)
    in
        if contains_card(c, cs) then
            aux(cs, c, [], false)
        else
            raise e  
    end

(* 
use "hw2.sml"; 
remove_card([(Clubs, Jack), (Diamonds, Num 4), (Hearts, Queen)], (Clubs, Jack), IllegalMove);   == [(Diamonds,Num 4),(Hearts,Queen)]
remove_card([(Clubs, Jack), (Diamonds, Num 4), (Hearts, Queen)], (Spades, Jack), IllegalMove);  == IllegalMove 
remove_card([(Clubs, Jack), (Clubs, Jack), (Hearts, Queen)], (Clubs, Jack), IllegalMove);       == [(Clubs,Jack),(Hearts,Queen)] 
remove_card([], (Clubs, Jack), IllegalMove);                                                    == IllegalMove
*)

fun same_color(color, cs) =
    case cs of 
        [] => true
        | c::c' => 
            case card_color(c) = color of
                true => same_color(color, c')
                | _ => false

fun all_same_color(cs) = 
    case cs of
        [] => true
        | c::cs => same_color(card_color(c), cs)

(* 
use "hw2.sml"; 
all_same_color([(Clubs, Jack), (Spades, Num 4), (Spades, Queen)]);   == true
all_same_color([(Clubs, Jack), (Hearts, Num 4), (Spades, Queen)]);   == false 
*)

fun sum_cards(cs) = 
    let
        fun aux(cs, acc) = 
            case cs of 
                [] => acc
                | card::cards => aux(cards, acc + card_value(card))
    in
        aux(cs, 0)
    end


(* 
use "hw2.sml"; 
sum_cards([(Clubs, Jack), (Spades, Num 4), (Spades, Queen)]);   == 24
sum_cards([(Clubs, Jack), (Hearts, Num 4), (Spades, Ace)]);     == 25 
*)

fun score(cs, g) = 
        let 
            val sum = sum_cards(cs)
            val same_col = all_same_color(cs)
            val p_score = if sum > g then 3 * (sum - g) else abs(sum - g)
        in
            if same_col then 
                p_score div 2
            else
                p_scores
        end


(* 
use "hw2.sml"; 
use "hw2_test.sml"; 
score([(Clubs, Jack), (Spades, Num 4), (Spades, Queen)], 5);   == 28
score([(Clubs, Jack), (Hearts, Num 4), (Spades, Ace)], 6);     == 57
*)

fun officiate(cs, ms, g) = 
    let fun helper(card_list, held_cards, move_list) =
        case move_list of
          [] => score(held_cards, g)
        | (Discard c)::tail_move => (case held_cards of
                                      [] => raise IllegalMove
                                    | _  => helper(card_list, remove_card(held_cards, c, IllegalMove), tail_move))
        | Draw::tail_move => case card_list of
                               []      => score(held_cards, g)
                             | head::_ => if sum_cards(head::held_cards) > g
                                             then score(head::held_cards, g)
                                             else helper(remove_card(card_list, head , IllegalMove), head::held_cards, tail_move)
    in
        helper(cs, [], ms)
    end
    

