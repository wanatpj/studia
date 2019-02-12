(* author: Agnieszka Łupińska *)

datatype Term = Const of int | Div of Term * Term

fun eval (Const i) = i
  | eval (Div (n, d)) = (eval n) div (eval d);

val answer = Div (Div (Const 1972, Const 2), Const 23)
val nothing = Div (Const 1, Const 0)

(**** Exceptions *)

type Exception = string
datatype 'a M1 = Raise of Exception | Return of 'a

fun eval2 (Const a) = Return a
  | eval2 (Div (n, d)) = case (eval2 n) of
                              Raise e => Raise e
                            | Return a =>
                                case (eval2 d) of
                                      Raise e => Raise e
                                    | Return b =>
                                      if b = 0 then Raise "divide by zero"
                                      else Return (a div b);

(**** Counting operations *)

type State = int
type 'a M2 = State -> 'a * State

(**** Output *)

type Output = string
type 'a M3 = Output * 'a

fun showterm (Const a) = "Const "^(Int.toString a)
  | showterm (Div (a, b)) = "Div ("^(showterm a)^", "^(showterm b)^")";

fun line t i = "eval("^(showterm t)^") <= "^(Int.toString i)^"\n";

fun eval4 (Const a) = (line (Const a) a, a)
  | eval4 (Div (t, u)) =
      let val (x, a) = eval4 t
          val (y, b) = eval4 u
          val res = a div b
      in (x^y^(line (Div (t, u)) res), res) end; 
(* (print o #1 o eval4) answer; *)

(* trudność modyfikacji programu - pure vs impure *)
(* zadanie - odwróć kolejność outputu *)

(**** Monads *)

(* Term -> Int  ===> Term -> M Int *)

infix 2 >>=

signature Monad = sig
  type 'a M
  val return : 'a -> 'a M
  val >>= : 'a M * ('a -> 'b M) -> 'b M
end

(* typowa konstrukcja: comp1 >>= (fn x => comp2 x)
 * analogiczna do let val x = comp1 in comp2 end
 *)

functor Eval5 (M : Monad) = struct
  open M
  fun eval (Const a) = return a
    | eval (Div (u, v)) = (eval u) >>= (fn x => (eval v) >>= (fn y => return (x div y)))
end

structure Identity : Monad = struct
  type 'a M = 'a
  fun return x = x
  fun x >>= y = y x
end

structure Exceptions : Monad = struct
  type 'a M = 'a M1
  fun return a = Return a
  fun (Return a) >>= b = b a
    | (Raise e) >>= _ = Raise e
    (*| (e as Raise _) >>= _ = e*)
end

(*structure X = Eval5(Exceptions);
 * open X
 *)

signature MonadWithError = sig
  include Monad
  val error : string -> 'a M
end

functor Eval6 (M : MonadWithError) = struct
  open M
  fun eval (Const a) = return a
    | eval (Div (u, v)) =
      (eval u) >>= (fn x =>
      (eval v) >>= (fn y =>
      if (y = 0) then error "divide by zero"
      else return (x div y)))
end

structure Exceptions2 : MonadWithError = struct
  open Exceptions
  fun error s = Raise s
end

local
  datatype 'a memo_cell = Value of 'a | Computation of unit -> 'a
  exception CellInvalid
  fun make_memo () =
  let
    val cell = ref (Computation (fn _ => raise CellInvalid))
    fun res () =
        case !cell of
          Computation h => (
            let val r = h () in
              cell := Value r;
              r
            end)
        | Value y => y
  in
    (cell, res)
  end
in
  (* fn : ((unit -> 'a) -> 'b * (unit -> 'a)) -> 'b
   * Zadaniem 'f' jest skonstruować obliczenie do spamiętania
   * oraz dowolną wartość, którą zwraca memo.
   * Argumentem funkcji 'f' jest to samo obliczenie, które
   * 'f' skonstruuje - ale w opakowanej wersji ze spamiętaniem.
   *)
  fun memo f =
  let
    val (mcell, memoized_computation) = make_memo ()
    val (result, computation) = f memoized_computation
  in
    mcell := Computation computation;
    result
  end
end

datatype 'a stream = Stream of unit -> (('a * 'a stream) option)
fun seval (Stream f) = f ();
fun sok s = Option.isSome (seval s)
fun shd s = #1 (Option.valOf (seval s))
fun stl s = #2 (Option.valOf (seval s))

(* fn : ('a stream -> 'a * 'a stream) -> 'a stream
 * smemo tworzy spamiętany strumień. Ten strumień jest
 * przekazywany do funkcji 'f' jako argument oraz
 * zwracany przez smemo. Funkcja 'f' konstruuje
 * początek strumienia - głowę oraz ogon.
 *)
fun smemo f = memo (fn the_stream =>
  let val wrapped_stream = Stream the_stream
      in (wrapped_stream, fn () => f wrapped_stream)
  end
)

fun simple_memo f = memo (fn res => (res, f))
fun memoy1 f = memo (fn res => (res, f res))
fun memoy2 f g = memo (fn r1 => memo (fn r2 => (((r1, r2), f r1 r2), g r1 r2)))
 
fun s2s s =
  let val length = String.size s
      fun from i =
        smemo (fn _ =>
          if i = length then NONE
          else SOME (String.sub (s, i), from (i+1))
        )
  in from 0 end

fun l2s l =
  smemo (fn _ =>
    if List.null l then NONE
    else SOME (List.hd l, l2s (List.tl l))
  )

fun stake 0 S = [] | stake k S = if sok S then (shd S) :: (stake (k-1) (stl S)) else []

fun smap m s = smemo (fn _ => if sok s then SOME (m (shd s), smap m (stl s)) else NONE)

fun sconcat2 s1 s2 = smemo (fn _ =>
  if sok s1 then SOME (shd s1, sconcat2 (stl s1) s2)
  else seval s2
)

fun sconcat2' s1 s2 = smemo (fn _ => if sok s1 then seval s1 else seval s2)

fun sconcatmany s = smemo (fn _ =>
  if not (sok s) then NONE
  else let val SOME (hd, tl) = seval s
       in seval (sconcat2 hd (sconcatmany tl)) end
)

fun sconst x =
    let
      fun const this = SOME(x, this)
    in
      smemo const
    end


infix 2 >>=

signature Monad = sig
  type 'a M
  val return : 'a -> 'a M
  val >>= : 'a M * ('a -> 'b M) -> 'b M
end

structure StreamMonad : Monad = struct
  type 'a M = 'a stream
  fun return x = l2s [x]
  fun x >>= y = sconcatmany (smap y x)
end;

structure ParserMonad : Monad = struct
  type 'b M = (char StreamMonad.M) -> ('b * char StreamMonad.M) StreamMonad.M
  fun return a s = StreamMonad.return (a, s);
  fun (a >>= b) inp = StreamMonad.>>= (a inp, fn (res, rest) => (b res rest));
end

fun item inp = smemo(fn _ => if not (sok inp) then NONE else seval (l2s [Option.valOf (seval inp)]))

open ParserMonad;

val p0 = (fn _ => l2s []) : 'a M;
infix 6 <+>;
fun (x <+> y) inp = sconcat2 (x inp) (y inp);

val oneortwo = (item >>= (fn x => return [x])) <+> (item >>= (fn y => item >>= (fn z  => return [y, z])));

infix 6 |>

fun parser |> predicate = parser >>= (fn res => if predicate res then return res else p0);

val letter = item |> Char.isAlpha;
val digit = item |> Char.isDigit;
fun lit l = item |> (fn x => x = l);

fun iterate m = (m >>= (fn first => (iterate m) >>= (fn tail => return (first::tail)))) <+> return [];

infix 6 <!+>;
fun (x <!+> y) inp = (sconcat2' (x inp) (y inp));

fun reiterate m = (m >>= (fn first => (reiterate m) >>= (fn tail => return (first::tail)))) <!+> return [];

fun s2s s =
  let val length = String.size s
      fun from i =
        smemo (fn _ =>
          if i = length then NONE
          else SOME (String.sub (s, i), from (i+1))
        )
  in (from 0): char StreamMonad.M end

fun code #"0" = 0 : IntInf.int
  | code #"1" = 1
  | code #"2" = 2
  | code #"3" = 3
  | code #"4" = 4
  | code #"5" = 5
  | code #"6" = 6
  | code #"7" = 7
  | code #"8" = 8
  | code #"9" = 9;

val letterOrDigit = item |> (fn x => if ((Char.isAlpha x) orelse (Char.isAlphaNum x)) then true else false);
val white = item |> (fn x => if (Char.isSpace x) then true else false);

fun asNumber [] (res: IntInf.int) = res
  | asNumber lista (res: IntInf.int) = asNumber (tl lista) ((res*(10:IntInf.int)) + (code (hd lista)));

fun number xyz = (
		((digit) >>= (fn head => (reiterate digit) >>= (fn tail => return (Number(asNumber (head::tail) (0:IntInf.int)))))) xyz)

fun isHot xyz = ((xyz = [#"l", #"e", #"t"]) orelse (xyz = [#"f", #"u", #"n"]) orelse (xyz = [#"i", #"n"]) orelse
		 (xyz = [#"v", #"a", #"l"]) orelse (xyz = [#"e", #"n", #"d"]) orelse (xyz = [#"f", #"n"]))


fun whiteChar xyz = (white >>= (fn _ => (reiterate white) >>= (fn _ => return (#" ")))) xyz
		
fun label xyz = ((letter) >>= (fn head => (reiterate letterOrDigit) >>= (fn tail => 
		if (isHot (head::tail)) then p0 else return (Label((implode (head::tail) : label)))))) xyz

and labelOrNumber xyz = (number <!+> label) xyz

and reiterateAPP head = ((cons) >>= 
			    (fn second => 
				(whiteChar >>= (fn _ => ((reiterateAPP (App(head, second))) >>= (fn res => return res))))
				<!+> 
				((reiterateAPP (App(head, second))) >>= (fn res => return res))
			    ))
			<!+>
			return head

and app xyz =  	(
		(cons >>= 
		    (fn t1 => 
			    (((whiteChar >>= (fn _ => (cons))) <!+> cons <!+> bracket) >>= 
			    (fn t2 => 
				(whiteChar >>= (fn _ => ((reiterateAPP (App(t1, t2))) >>= (fn res => return res))))
				<!+>
				(reiterateAPP (App(t1, t2))) >>= (fn res => return res)
			    ))			    
			<!+>
			return t1
		    )) 
		) xyz

and reiterateAdd head = (plus_lit >>= (fn _ => (bracket <!+> labelOrNumber <!+> letFun <!+> fnFun) >>= (fn second =>
			(reiterateAdd (App(App(Label("PLUS"), head), second))) >>= (fn res => return res))))
			<!+>
			return head

and add xyz = 	(
		((bracket <!+> labelOrNumber <!+> letFun <!+> fnFun) >>= (fn t1 => 
		    ((plus_lit) >>= (fn _ => (bracket <!+> labelOrNumber <!+> letFun <!+> fnFun) >>= (fn t2 => 
					    (reiterateAdd (App(App(Label("PLUS"), t1), t2))) >>= (fn res => return res))))
		    <!+>
		    (return t1)
		))
		) xyz

and cons xyz = 	( (* term = ((whiteChars) <!+> (app) <!+> (cons) <!+> (add) <!+> bracket <!+> (fnFun) <!+> (letFun) <!+> (number) <!+> (label) *)
		add >>= (fn t1 => 
		    ((cons_lit) >>= (fn _ => cons >>= (fn t2 => return (App(App(Label("CONS"), t1), t2)))))
		    <!+>
		    (return t1)
		)
		) xyz

and fnFun xyz = (
		(fn_lit) >>= (fn _ => (label) >>= (fn Label(etykieta) => (imp_lit) >>= (fn _ => 
		(term) >>= (fn wyrazenie => return (Abs(etykieta, wyrazenie))))))
		) xyz

and letFun xyz = (
		(let_lit) >>= (fn _ => (reiterate definition) >>= (fn def => (in_lit) >>= (fn _ => 
		(term) >>= (fn wyrazenie => (end_lit) >>= (fn _ => return (Let(def, wyrazenie)))))))
		) xyz

and reiterateAbs label = (
			((eq_lit) >>= (fn _ => (term) >>= (fn wyrazenie => return (wyrazenie))))
		<!+>
			(whiteChar >>= (fn _ => (label) >>= (fn Label(etykieta) => 
			 (reiterateAbs label) >>= (fn wyrazenie => return (Abs(etykieta, wyrazenie))))))
		<!+>
			(label >>= (fn Label(etykieta) => (reiterateAbs label) >>= (fn wyrazenie => return (Abs(etykieta, wyrazenie)))))
		)

and definition xyz = (
			((val_lit) >>= (fn _ => (label) >>= (fn Label(etykieta) => 
			 (eq_lit) >>= (fn _ => (term) >>= (fn wyrazenie => return (etykieta, wyrazenie)))))) 
			<!+>
			((fun_lit) >>= (fn _ => (label) >>= (fn Label(funName) => whiteChar >>= (fn _ =>
			 (label) >>= (fn Label(etykieta) => 
			 (reiterateAbs label) >>= (fn funLabel => return (funName, Abs(etykieta, funLabel))))))))
		) xyz

and bracket xyz = (spaceOrBracketLeft_lit >>= (fn _ => (term) >>= (fn wyrazenie => spaceOrBracketRight_lit >>= (fn _ => return wyrazenie)))) xyz

and whiteChars xyz = (whiteChar >>= (fn _ => (term) >>= (fn wyrazenie => return wyrazenie))) xyz

and term xyz = (whiteChars <!+> fnFun <!+> app <!+> letFun <!+> bracket) xyz

and noParse xyz = (return (Label("NO PARSE"))) xyz

and parseT t = debugl (let val (lt, stream) = shd ((term <!+> noParse)(s2s t)) in lt end )

and spaceOrBracketLeft_lit xyz = (
		((whiteChar) >>= (fn _ => (lit #"(") >>= (fn _ => 
		    ((whiteChar) >>= (fn _ => return (Label("("))))
		    <!+>
		    (return (Label("("))))))
		<!+>
		((lit #"(") >>= (fn _ => 
		    ((whiteChar) >>= (fn _ => return (Label("("))))
		    <!+>
		    (return (Label("(")))))
		) xyz

and spaceOrBracketRight_lit xyz = (
		((whiteChar) >>= (fn _ => (lit #")") >>= (fn _ => 
		    ((whiteChar) >>= (fn _ => return (Label(")"))))
		    <!+>
		    (return (Label(")"))))))
		<!+>
		((lit #")") >>= (fn _ => 
		    ((whiteChar) >>= (fn _ => return (Label(")"))))
		    <!+>
		    (return (Label(")")))))
		) xyz
and end_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"e") >>= (fn _ => (lit #"n") >>= (fn _ => (lit #"d") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("end"))))
		    <!+>
		    (return (Label("end"))))))))
		<!+>
		((lit #"e") >>= (fn _ => (lit #"n") >>= (fn _ => (lit #"d") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("end"))))
		    <!+>
		    (return (Label("end")))))))
		) xyz
and let_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"l") >>= (fn _ => (lit #"e") >>= (fn _ => (lit #"t") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("let"))))
		    <!+>
		    (return (Label("let"))))))))
		<!+>
		((lit #"l") >>= (fn _ => (lit #"e") >>= (fn _ => (lit #"t") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("let"))))
		    <!+>
		    (return (Label("let")))))))
		) xyz
and fun_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"f") >>= (fn _ => (lit #"u") >>= (fn _ => (lit #"n") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("fun"))))
		    <!+>
		    (return (Label("fun"))))))))
		<!+>
		((lit #"f") >>= (fn _ => (lit #"u") >>= (fn _ => (lit #"n") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("fun"))))
		    <!+>
		    (return (Label("fun")))))))
		) xyz
and val_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"v") >>= (fn _ => (lit #"a") >>= (fn _ => (lit #"l") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("val"))))
		    <!+>
		    (return (Label("val"))))))))
		<!+>
		((lit #"v") >>= (fn _ => (lit #"a") >>= (fn _ => (lit #"l") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("val"))))
		    <!+>
		    (return (Label("val")))))))
		) xyz
and fn_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"f") >>= (fn _ => (lit #"n") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("fn"))))
		    <!+>
		    (return (Label("fn")))))))
		<!+>
		((lit #"f") >>= (fn _ => (lit #"n") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("fn"))))
		    <!+>
		    (return (Label("fn"))))))
		) xyz
and in_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"i") >>= (fn _ => (lit #"n") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label ("in"))))
		    <!+>
		    (return (Label ("in")))))))
		<!+>
		((lit #"i") >>= (fn _ => (lit #"n") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label ("in"))))
		    <!+>
		    (return (Label ("in"))))))
		) xyz
and imp_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"=") >>= (fn _ => (lit #">") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("=>"))))
		    <!+>
		    (return (Label("=>")))))))
		<!+>
		((lit #"=") >>= (fn _ => (lit #">") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("=>"))))
		    <!+>
		    (return (Label("=>"))))))
		) xyz 
and plus_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"+") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("+")))) 
		    <!+>
		    (return (Label("+"))))))
		<!+>
		((lit #"+") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("+"))))
		    <!+>
		    (return (Label("+")))))
		) xyz
and eq_lit xyz = (
		(whiteChar >>= (fn _ => (lit #"=") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("="))))
		    <!+>
		    (return (Label("="))))))
		<!+>
		((lit #"=") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("="))))
		    <!+>
		    (return (Label("=")))))
		) xyz
and cons_lit xyz = (
		(whiteChar >>= (fn _ => (lit #":") >>= (fn _ => (lit #":") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("::"))))
		    <!+>
		    (return (Label("::")))))))
		<!+>
		((lit #":") >>= (fn _ => (lit #":") >>= (fn _ => 
		    (whiteChar >>= (fn _ => return (Label("::"))))
		    <!+>
		    (return (Label("::"))))))
		) xyz

val inputAndOutput = 
    let
	fun readl() =
	    case TextIO.input1 (TextIO.stdIn) of
		SOME s => (s :char)::readl()
	    |	NONE => []
    
	val out = l2s (readl())
    in
	case (seval (term out)) of
	NONE    => print "NO PARSE"
	|SOME ((t,_),_)=> print (debugl t)
end 
