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

(* use "hw3.sml"; *)
(* use "hw3test.sml"; *)
fun only_capitals(xs) = 
	List.filter(fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string_helper f xs =
	List.foldl (fn (x, acc) => if f(String.size(x), String.size(acc)) then x else acc) "" xs

(* longest_string1 ["aa", "bb"] = "aa"; *)
fun longest_string1(xs) =
	List.foldl(fn (x, acc) => if String.size(acc) >= String.size(x) then acc else x) "" xs

(* longest_string2 ["aa", "bb"] = "bb"; *)
fun longest_string2(xs) = 
	List.foldl(fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) "" xs

(* longest_string3 ["aa", "bb"] = "aa"; *)
val longest_string3 = longest_string_helper (op >)

(* longest_string4 ["aa", "bb"] = "bb"; *)
val longest_string4 = longest_string_helper (op >=)

(* longest_capitalized ["A","bc","C"] = "A" *)
val longest_capitalized = longest_string1 o only_capitals

(* rev_string "abc" = "cba" *)
val rev_string = String.implode o List.rev o String.explode


(* use "hw3.sml"; *)
(* use "hw3test.sml"; *)

(* first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4 *)
fun first_answer f xs = 
	case xs of
		[] => raise NoAnswer
		| x::x' => 
			case f(x) of 
				NONE => first_answer f x'
				| SOME v => v


(* all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE *)
fun all_answers f xs = 
	let 
		fun aux(f, acc, s) = 
			case s of 
			[] => acc
			| s::s' => case f(s) of 
					NONE => aux(f, [], s')
					| SOME v => aux(f, v @ acc, s')
	in
		case aux(f, [], xs) of 
			[] => NONE
			| v => SOME v 
	end

(* use "hw3.sml"; *)
(* use "hw3test.sml"; *)
(* fun g f1 f2 p =
    let 
		val r = g f1 f2 
    in
		case p of
			  Wildcard          => f1 ()
			| Variable x        => f2 x
			| TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
			| ConstructorP(_,p) => r p
			| _                 => 0
    end *)

(* count_wildcards Wildcard = 1 *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* use "hw3.sml"; *)
(* use "hw3test.sml"; *)

(* count_wild_and_variable_lengths (Variable("a")) = 1 *)
val count_wild_and_variable_lengths = g (fn _ => 1)  (fn x => String.size(x)) 

(* count_some_var ("x", Variable("x")) = 1 *)
fun count_some_var(s, p) = g (fn _ => 0)  (fn x => if String.compare(s, x) = EQUAL then 1 else 0) p
(* 
(* check_pat (Variable("x")) = true *)
fun check_pat p = true

fun match (p1, p2) = NONE

fun first_match p lp = SOME [] *)
