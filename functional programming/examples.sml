val example1 = 
Abs
    ("a",
     Abs
       ("b",
        Abs ("c",App (App (Label "a",Label "c"),App (Label "b",Label "c")))));


val example2 = 
App
    (App
       (App
          (Abs
             ("S",
              Abs
                ("K1",Abs ("K2",App (App (Label "S",Label "K1"),Label "K2")))),
           Abs
             ("a",
              Abs
                ("b",
                 Abs
                   ("c",
                    App (App (Label "a",Label "c"),App (Label "b",Label "c")))))),
        Abs ("x",Abs ("y",Label "x"))),Abs ("x",Abs ("y",Label "x")));

val example22 =
  App
    (Abs
       ("S",Abs ("K1",Abs ("K2",App (App (Label "S",Label "K1"),Label "K2")))),
     Abs
       ("a",
        Abs
          ("b",
           Abs ("c",App (App (Label "a",Label "c"),App (Label "b",Label "c"))))));


val example3 =
App
    (App
       (Abs ("S",Abs ("K",App (App (Label "S",Label "K"),Label "K"))),
        Abs
          ("a",
           Abs
             ("b",
              Abs
                ("c",
                 App (App (Label "a",Label "c"),App (Label "b",Label "c")))))),
     Abs ("x",Abs ("y",Label "x")));

val example4 =
Let
    ([("S",
       Abs
         ("a",
          Abs
            ("b",
             Abs
               ("c",App (App (Label "a",Label "c"),App (Label "b",Label "c")))))),
      ("K",Abs ("a",Abs ("b",Label "a")))],
     App (App (Label "S",Label "K"),Label "K"));

val example5 =
Abs ("x",Abs ("y",App (App (Label "CONS",Label "x"),Label "y")));

val example6 =
Abs ("y",App (App (Label "PLUS",Label "y"),Number 1));

val example7 =
App
    (Abs
       ("I",
        Let
          ([("x",
             Let ([("y",App (Label "I",Number 7))],App (Label "I",Label "x")))],
           Label "x")),Abs ("x",Label "x"));

val example8 =
Let
    ([("foldl",
       Abs
         ("f",
          Abs
            ("z",
             Abs
               ("l1",
                App
                  (App (App (Label "IF",Number 0),Label "z"),
                   App
                     (App
                        (App (Label "foldl",Label "f"),
                         App
                           (App (Label "f",Label "z"),
                            App (Label "hd",Label "l1"))),
                      App (Label "tl",Label "l1"))))))),
      ("ones",App (App (Label "CONS",Number 1),Label "ones")),
      ("add",Abs ("x",Abs ("y",App (App (Label "PLUS",Label "x"),Label "y")))),
      ("f1",
       Abs ("i",App (App (Label "PLUS",Number 1),App (Label "f2",Label "i")))),
      ("f2",
       Abs ("i",App (App (Label "PLUS",Number 1),App (Label "f1",Label "i"))))],
     App (Label "foldl",Label "add"));
     
val example9 =
  Let
    ([("id",Abs ("x",Label "x"))],
     Abs
       ("a",
        Abs
          ("b",
           App
             (App (Abs ("u",Abs ("v",Label "u")),App (Label "id",Label "a")),
              App (Label "id",Label "b")))));

val example10 =
  Let
    ([("id",Abs ("x",Label "x"))],
     Abs
       ("a",
        Abs
          ("b",
           App
             (App (Abs ("x",Abs ("y",Label "x")),App (Label "id",Label "a")),
              App (Label "id",Label "b")))));

val example11 =
Let
    ([("f1",
       Abs ("i",App (App (Label "PLUS",Number 1),App (Label "f2",Label "i")))),
      ("f2",
       Abs ("i",App (App (Label "PLUS",Number 1),App (Label "f1",Label "i"))))],
     App (Label "f1", Number 3));

local
                infixr 6 -->
                fun a --> b= ARR (a,b)
in
                val context= [
                        ("PLUS",INT --> INT --> INT),
                        ("CONS", VAR "a" --> (LIST (VAR "a")) --> (LIST (VAR "a"))),
                        ("NIL", LIST (VAR "a")),
                        ("hd",(LIST (VAR "a")) --> VAR "a"),
                        ("IF", INT --> VAR "a" --> VAR "a" --> VAR "a"),
                        ("tl", (LIST (VAR "a")) --> (LIST (VAR "a"))),
                        ("PAIR",(VAR "a" --> VAR "b" --> VAR "a" --> VAR "b" --> INT ) )
                        ]
end

val example12 = App
                     (App
                        (App (Label "foldl",Label "f"),
                         App
                           (App (Label "f",Label "z"),
                            App (Label "hd",Label "l1"))),
                       App (Label "tl",Label "l1"));
val ex12context = ([("f", VAR "v100"), ("z", VAR "v101"), ("l1", VAR "v102"), ("foldl", VAR "v103")]@context);

val example13 = Let
    ([("foldl",
       Abs
         ("f",
          Abs
            ("z",
             Abs
               ("l1",
                App
                  (App (App (Label "IF",Number 0),Label "z"),
                   App
                     (App
                        (App (Label "foldl",Label "f"),
                         App
                           (App (Label "f",Label "z"),
                            App (Label "hd",Label "l1"))),
                      App (Label "tl",Label "l1"))))))),
      ("ones",App (App (Label "CONS",Number 1),Label "ones"))],
     Label "foldl");
val example14 =
  Let
    ([("foldl",
       Abs
         ("f",
          Abs
            ("z",
             Abs
               ("l1",
                App
                  (App (App (Label "IF",Number 0),Label "z"),
                   App
                     (App
                        (App (Label "foldl",Label "f"),
                         App
                           (App (Label "f",Label "z"),
                            App (Label "hd",Label "l1"))),
                      App (Label "tl",Label "l1"))))))),
      ("add",Abs ("x",Abs ("y",App (App (Label "PLUS",Label "x"),Label "y"))))],
     App (Label "foldl",Label "add")) : lterm

val example15 =
  Let
    ([("zip",
       Abs
         ("l1",
          Abs
            ("l2",
             App
               (App
                  (Label "CONS",
                   App
                     (App (Label "PAIR",App (Label "hd",Label "l1")),
                      App (Label "hd",Label "l2"))),
                App
                  (App (Label "zip",App (Label "tl",Label "l1")),
                   App (Label "tl",Label "l2")))))),
      ("foldl",
       Abs
         ("f",
          Abs
            ("z",
             Abs
               ("l1",
                App
                  (App (App (Label "IF",Number 0),Label "z"),
                   App
                     (App
                        (App (Label "foldl",Label "f"),
                         App
                           (App (Label "f",Label "z"),
                            App (Label "hd",Label "l1"))),
                      App (Label "tl",Label "l1"))))))),
      ("add",Abs ("x",Abs ("y",App (App (Label "PLUS",Label "x"),Label "y"))))],
     App (Label "foldl",Label "add")) : lterm
;
(*  

let
	fun isNil l = 0
	val null = isNil
in let
	fun length l = IF (isNil l) 0 (1+(length (tl l)))
  fun map f l = IF (isNil l) NIL ((f (hd l))::(map f l))
	fun foldl f acc list = IF (isNil list) acc (foldl f (f (hd list) acc) (tl list) )
	fun foldr f init list = IF (isNil list) init (f (hd list) (foldr f init (tl list)))
	fun zip l1 l2 = IF (isNil l1) NIL ((PAIR (hd l1) (hd l2))::(zip (tl l1) (tl l2)))
	fun take n l = IF (zero n) NIL (hd l)::(take (dec n) (tl l))
	fun drop n l = IF (zero n) l (drop (dec n) (tl l))

	fun rev l1 = let
		fun trev l1 acc= IF (isNil l1) acc (trev (tl l1) ((hd l1)::acc))
		in trev l1 NIL end

	fun concat l1 = let
		fun concat2 l = IF (isNil l) NIL (let
			val firstL = hd l
			val restL = tl l
			in IF (isNil firstL) (concat2 restL) ((hd firstL)::(concat2 ((tl firstL)::(restL)))) end)
 		in concat2 l1 end
	fun cat l1 l2 = concat (l1::l2::NIL)

	fun filter pred l = IF (isNil l) NIL (let
		val first = hd l
		val rest = tl l
		val ok = pred first
		val frest = filter pred rest
		in IF ok (first::frest) frest end
		)

	val ones = 1::ones

	fun inc v = 1+v+(inc2 1)
	val inc2 = inc
	val dec = inc
	fun zero x = 0

	fun add x y = x+y
	fun leq x y = 1
in  (PAIR (PAIR (PAIR (PAIR null length) (PAIR map foldl)) 
		(PAIR (PAIR zip take) (PAIR rev concat))
	) (PAIR (PAIR (PAIR cat filter) (PAIR ones inc)) 
		(PAIR (PAIR dec zero) (PAIR add leq))
	))  
end
end


	fun take n l = IF (zero n) NIL (hd l)::(take (dec n) (tl l))
	fun drop n l = IF (zero n) l (drop (dec n) (tl l))

	fun inc v = 1+v+(inc2 1)
	val inc2 = inc
	val dec = inc
	fun zero x = 0
*)

val example16 =
  Let(
    [("isNil", Abs ("l", Number 0)),
      ("null", Label "isNil")],
    Let(
      [("length", Abs ("l", App (App (App (Label "IF", App (Label "isNil", Label "l")), Number 0),
          App (App (Label "PLUS", Number 1), App (Label "length", App (Label "tl", Label "l")))))),
        ("map", Abs ("f", Abs ("l", App (App (App (Label "IF", App(Label "isNil", Label "l")), Label "NIL"),
            App (App (Label "CONS", App (Label "f", App (Label "hd", Label "l"))),
                App (App (Label "map", Label "f"), Label "l")))))),
        ("foldl", Abs ("f", Abs ("acc", Abs ("list",
            App (App (App (Label "IF", App (Label "isNil", Label "list")),
                Label "acc"), App (App (App (Label "foldl", Label "f"),
                    App (App (Label "f", App (Label "hd", Label "list")), Label "acc")),
                    App (Label "tl", Label "list"))))))),
        ("foldr", Abs ("f", Abs ("acc", Abs ("list",
            App (App (App (Label "IF", App (Label "isNil", Label "list")),
                Label "acc"), App (App (Label "f", App (Label "hd", Label "list")),
                  App (App (App (Label "foldr", Label "f"), Label "acc"), App (Label "tl", Label "list")))))))),
        ("zip", Abs ("l1", Abs ("l2", App (App (App (Label "IF", App (Label "isNil", Label "l1")), Label "NIL"),
            App
               (App
                  (Label "CONS",
                   App
                     (App (Label "PAIR",App (Label "hd",Label "l1")),
                      App (Label "hd",Label "l2"))),
                App
                  (App (Label "zip",App (Label "tl",Label "l1")),
                   App (Label "tl",Label "l2"))))))),
        ("take", Abs ("n", Abs ("l", App (App (App (Label "IF", App (Label "zero", Label "n")), Label "NIL"),
            App (App (Label "CONS", App (Label "hd", Label "l")), App (App (Label "take",
                App (Label "dec", Label "n")), App (Label "tl", Label "l"))))))),
        ("inc", Abs ("v", App (App (Label "PLUS",
            App (App (Label "PLUS", Number 0), Label "v")), App (Label "inc2", Number 1)))),
        ("inc2", Label "inc"),
        ("dec", Label "inc"),
        ("zero", Abs ("x", Number 0))],
      Label "dec"));
(*
let 
    fun isNil l = 0 
    fun concat l = IF (isNil l) NIL (let 
            val firstL = hd l 
            val restL = tl l 
            in IF (isNil firstL) (concat restL) ((hd firstL)::(concat ((tl firstL)::(restL)))) end) 
in   concat 
end 
end

val example17 =
  Let(
    [("isNil", Abs ("l", Number 0)),
      ("concat", Abs ("l", App ("", )))],
    Label "concat");*)
(*
let 
    fun id x = x; 
    fun id2 a = let 
        val b = id a; 
        val c = hd b; 
    in id b end; 
in id2 end;
*)

val example18 =
  Let (
    [("id", Abs ("x", Label "x")),
      ("id2", Abs ("a", Let (
        [("b", App (Label "id", Label "a")),
          ("c", App (Label "hd", Label "b"))],
        App (Label "id", Label "b"))))],
    Label "id2");

val example19 =
  Let (
    [("id", Abs ("x", Label "x")),
      ("id2", Abs ("a", Let (
        [("b", App (Label "id", Label "a")),
          ("c", App (Label "hd", Label "b"))],
        App (Label "id", Label "c"))))],
    Label "id2");
    
val example20 =
  Let
    ([("id",Abs ("x",Label "x"))],
     Let
       ([("x",App (Label "id",Number 0)),
         ("y",App (Label "id",App (Label "CONS",Number 0)))],Label "id"))
  : lterm


(*
let 
    fun singleton x = x::NIL 
    fun ss l = singleton l 
in singleton end 
*)

val example21 =
  Let (
    [("singleton", Abs ("x", App (App (Label "CONS", Label "x"), Label "NIL"))),
      ("ss", Abs ("l", App (Label "singleton", Label "l")))],
    Label "singleton");

val example22 =
    Let(
      [("take", Abs ("n", Abs ("l", App (App (App (Label "IF", App (Label "zero", Label "n")), Label "NIL"),
            App (App (Label "CONS", App (Label "hd", Label "l")), App (App (Label "take",
                App (Label "dec", Label "n")), App (Label "tl", Label "l"))))))),
        ("inc", Abs ("v", App (App (Label "PLUS",
            App (App (Label "PLUS", Number 0), Label "v")), App (Label "inc2", Number 1)))),
        ("inc2", Label "inc"),
        ("dec", Label "inc"),
        ("zero", Abs ("x", Number 0))],
      Label "dec");
