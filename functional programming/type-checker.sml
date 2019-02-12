datatype 'a node = Two of 'a node * 'a * 'a node |
  Three of 'a node * 'a * 'a node * 'a * 'a node |
  Empty;
datatype 'b Propagate= Good of 'b | PropagateUp of 'b;
datatype movee = LL | MM | RR;

fun st (Two (a, b, c)) = a
fun nd (Two (a, b, c)) = b
fun rd (Two (a, b, c)) = c

fun isgood (Good x) = true
  | isgood (PropagateUp x) = false

fun unwrap (Good (b)) = b
  | unwrap (PropagateUp (b)) = b

signature COMPARABLE= sig
  type t;
  val cmp: t*t -> order
end;

signature SPEC = sig
  structure Key:COMPARABLE;
  type 'vT entryT;
  type 'vT resultT;

  val extractKey: 'vT entryT -> Key.t;
  val updateE: 'vT entryT node * 'vT entryT -> 'vT entryT node Propagate;
  val lookupE: 'vT entryT option -> 'vT resultT;
end;

signature DICT= sig
  structure Key:COMPARABLE;

  type 'vt dict;

  val empty: 'vt dict;
  val insert: (Key.t * 'vt) * 'vt dict -> 'vt dict;
  val lookup: (Key.t * 'vt dict) -> 'vt option;
  val removeKey: (Key.t * 'vt dict) -> (bool * 'vt dict);
  val allItems: 'vt dict -> (Key.t * 'vt) list;
  val insertAll: ((Key.t * 'vt) list * 'vt dict) -> 'vt dict;
  val union: ('vt dict * 'vt dict) -> 'vt dict;
end;

signature OSET= sig
  structure Key:COMPARABLE;

  type oset;

  val empty: oset;
  val insert: Key.t * oset -> oset;
  val member: Key.t * oset -> bool;
  val remove: Key.t * oset -> bool * oset;
  val allItems: oset -> Key.t list;
  val insertAll: (Key.t list * oset) -> oset;
  val union: oset * oset -> oset;
end;

functor TFrame(structure Spec:SPEC)= struct
  structure Spec = Spec;
  type 'vt frame= 'vt Spec.entryT node;
  val empty = Empty
  fun lookup' (key,  Empty) = NONE
    | lookup' (key, Two (treea, a, treeb)) =
      let val c = (Spec.Key.cmp) (key, Spec.extractKey a)
      in
        if c = EQUAL then
          SOME a
        else if c = LESS then
          lookup' (key, treea)
        else
          lookup' (key, treeb)
      end
    | lookup' (key, Three (treea, a, treeb, b, treec)) =
      let val c = (Spec.Key.cmp) (key, Spec.extractKey a)
      in
        if c = EQUAL then
          SOME a
        else if c = LESS then
          lookup' (key, treea)
        else
          let val d = (Spec.Key.cmp) (key, Spec.extractKey b)
          in
            if d = EQUAL then
              SOME b
            else if d = LESS then
              lookup' (key, treeb)
            else
              lookup' (key, treec)
          end
      end
  fun lookup what = Spec.lookupE (lookup' what)
  fun insert' (key, x, Empty) = Spec.updateE (Empty, x)
    | insert' (key, x, Two (left, y, right)) =
      let val c = (Spec.Key.cmp) (key, Spec.extractKey y)
      in
        if c = EQUAL then
          Spec.updateE (Two (left, y, right), x)
        else if (c = LESS) then
          let
            val nod = insert' (key, x, left)
            val tree = unwrap nod
          in
            if (isgood nod) then
              Good ( Two (tree, y, right))
            else
              Good ( Three (st tree, nd tree, rd tree, y, right))
          end
        else
          let
            val nod = insert' (key, x, right)
            val tree = unwrap nod
          in
            if (isgood nod) then
              Good ( Two (left, y, tree))
            else
              Good ( Three (left, y, st tree, nd tree, rd tree))
          end
      end
    | insert' (key, x, Three (left, y, middle, z, right)) =
      let
        val c = (Spec.Key.cmp) (key, Spec.extractKey y)
      in
        if c = EQUAL then
          Spec.updateE (Three (left, y, middle, z, right), x)
        else if c = LESS then
          let
            val nod = insert' (key, x, left)
            val tree = unwrap nod
          in
            if (isgood nod) then
              Good ( Three (tree, y, middle, z, right))
            else
              PropagateUp ( Two (tree, y, Two (middle, z, right)))
          end
        else
          let
            val d = (Spec.Key.cmp) (key, Spec.extractKey z)
          in
            if d = EQUAL then
              Spec.updateE (Three (left, y, middle, z, right), x)
            else if d = LESS then
              let
                val nod = insert' (key, x, middle)
                val tree = unwrap nod
              in
                if (isgood nod) then
                  Good ( Three (left, y, tree, z, right))
                else
                  PropagateUp ( Two (Two (left, y, st tree), nd tree, Two (rd tree, z, right)))
              end
            else
              let
                val nod = insert' (key, x, right)
                val tree = unwrap nod
              in
                if (isgood nod) then
                  Good ( Three (left, y, middle, z, tree))
                else
                  PropagateUp ( Two (Two (left, y, middle), z, tree))
              end
          end
      end
  fun insert (x, tree) = unwrap (insert' (Spec.extractKey x, x, tree))
  fun rotate LL (Two (s, a, Two (t, b, u))) = (true, Three (s, a, t, b, u))
    | rotate RR (Two (Two (s, a, t), b, u)) = (true, Three (s, a, t, b, u))
    | rotate LL (Two (s, a, Three (t, b, u, c, v))) =
        (false, Two (Two (s, a, t), b, Two (u, c, v)))
    | rotate RR (Two (Three (s, a, t, b, u), c, v)) =
        (false, Two (Two (s, a, t), b, Two (u, c, v)))
    | rotate LL (Three (s, a, Two (t, b, u), c, v)) =
        (false, Two (Three (s, a, t, b, u), c, v))
    | rotate MM (Three (s, a, t, b, Two (u, c, v))) =
        (false, Two (s, a, Three (t, b, u, c, v)))
    | rotate RR (Three (s, a, Two (t, b, u), c, v)) =
        (false, Two (s, a, Three (t, b, u, c, v)))
    | rotate LL (Three (s, a, Three (t, b, u, c, v), d, w)) =
        (false, Three (Two (s, a, t), b, Two (u, c, v), d, w))
    | rotate MM (Three (s, a, t, b, Three (u, c, v, d, w))) =
        (false, Three (s, a, Two (t, b, u), c, Two (v, d, w)))
    | rotate RR (Three (s, a, Three (t, b, u, c, v), d, w)) =
        (false, Three (s, a, Two (t, b, u), c, Two (v, d, w)))
  fun remove_min (Two (Empty, a, Empty)) = (true, Empty, a)
    | remove_min (Three (Empty, a, Empty, b, Empty)) =
        (false, Two (Empty, b, Empty), a)
    | remove_min (Two (s, a, t)) =
      let val processed = remove_min s
      in
        if #1 processed then
          let val rotated = rotate LL (Two (#2 processed, a, t))
          in
            (#1 rotated, #2 rotated, #3 processed)
          end
        else
          (false, Two (#2 processed, a, t), #3 processed)
      end
    | remove_min (Three (s, a, t, b, u)) =
      let val processed = remove_min s
      in
        if #1 processed then
          let val rotated = rotate LL (Three (#2 processed, a, t, b, u))
          in
            (#1 rotated, #2 rotated, #3 processed)
          end
        else
          (false, Three (#2 processed, a, t, b, u), #3 processed)
      end
  fun remove' (a, Two (Empty, b, Empty)) =
    if (Spec.Key.cmp (a, Spec.extractKey b)) = EQUAL then
      (true, Empty)
    else (false, Two (Empty, b, Empty))
  | remove' (a, Three (Empty, b, Empty, c, Empty)) =
    if (Spec.Key.cmp (a, Spec.extractKey b)) = EQUAL then
      (false, Two (Empty, c, Empty))
    else if (Spec.Key.cmp (a, Spec.extractKey c)) = EQUAL then
      (false, Two (Empty, b, Empty))
    else
      (false, Three (Empty, b, Empty, c, Empty))
  | remove' (x, Two (s, a, t)) =
    let
      val c = Spec.Key.cmp (x, Spec.extractKey a)
    in
      if c = EQUAL then
        let val processed = remove_min t
        in
          if #1 processed then
            rotate RR (Two (s, #3 processed, #2 processed))
          else
            (false, Two (s, #3 processed, #2 processed))
        end
      else if c = LESS then
        let val processed = remove' (x, s)
        in
          if #1 processed then
            rotate LL (Two (#2 processed, a, t))
          else
            (false, Two (#2 processed, a, t))
        end
      else
        let val processed = remove' (x, t)
        in
          if #1 processed then
            rotate RR (Two (s, a, #2 processed))
          else
            (false, Two (s, a, #2 processed))
        end
    end
  | remove' (x, Three (s, a, t, b, u)) =
    let val c = Spec.Key.cmp (x, Spec.extractKey a)
    in
      if c = EQUAL then
        let val processed = remove_min t
        in
          if #1 processed then
            rotate MM (Three (s, #3 processed, #2 processed, b, u))
          else
            (false, Three (s, #3 processed, #2 processed, b, u))
        end
      else if c = LESS then
        let val processed = remove' (x, s)
        in
          if #1 processed then
            rotate LL (Three (#2 processed, a, t, b, u))
          else
            (false, Three (#2 processed, a, t, b, u))
        end
      else
        let val d = Spec.Key.cmp (x, Spec.extractKey b)
        in
          if d = EQUAL then
            let val processed = remove_min u
            in
              if #1 processed then
                rotate RR (Three (s, a, t, #3 processed, #2 processed))
              else
                (false, Three (s, a, t, #3 processed, #2 processed))
            end
          else if d = LESS then
            let val processed = remove' (x, t)
            in
              if #1 processed then
                rotate MM (Three (s, a, #2 processed, b, u))
              else
                (false, Three (s, a, #2 processed, b, u))
            end
          else
            let val processed = remove' (x, u)
            in
              if #1 processed then
                rotate RR (Three (s, a, t, b, #2 processed))
              else
                (false, Three (s, a, t, b, #2 processed))
            end
        end
    end
  fun remove what =
    if isSome (lookup' what) then
      (true, #2 (remove' what))
    else
      (false, #2 what)
  fun items' Empty result = result
    | items' (Two (treea, a, treeb)) result =
      let val partial = items' treea result
      in
        items' treeb (a::partial)
      end
    | items' (Three (treea, a, treeb, b, treec)) result =
      let
        val partiala = items' treea result
        val partialb = items' treeb (a::partiala)
      in
        items' treec (b::partialb)
      end
  fun items containter = rev (items' containter [])
  fun insertAll ([], container) = container
    | insertAll (a::b, container) = insertAll (b, (insert (a, container)))
  fun union' (seta, []) = seta
    | union' (seta, (a::lst)) = union' (insert (a, seta), lst)
  fun union (seta, setb) = union' (seta, (items setb))
end;

functor DSpec (structure KeyS:COMPARABLE):SPEC = struct
  structure Key = KeyS;
  type 'vT entryT = KeyS.t * 'vT;
  type 'vT resultT = 'vT option;
  fun extractKey (x, y) = x
  fun updateE (Empty, x) = PropagateUp (Two (Empty, x, Empty))
    | updateE (Two (treea, y, treeb), x) = Good (Two (treea, x, treeb))
    | updateE (Three (treea, a, treeb, b, treec), (x, y)) =
      if (Key.cmp (x, extractKey a)) = EQUAL then
        Good (Three(treea, (x, y), treeb, b, treec))
      else
        Good (Three(treea, a, treeb, (x, y), treec))
  fun lookupE NONE = NONE
    | lookupE (SOME (x, y)) = SOME y
end

functor SSpec (structure KeyS:COMPARABLE):SPEC = struct
  structure Key = KeyS;
  type 'vT entryT = KeyS.t;
  type 'vT resultT = bool;
  fun extractKey x = x
  fun updateE (Empty, x) = PropagateUp (Two (Empty, x, Empty))
    | updateE (Two (treea, y, treeb), x) = Good (Two (treea, x, treeb))
    | updateE (Three (treea, a, treeb, b, treec), x) =
      if (Key.cmp (x, a)) = EQUAL then
        Good (Three(treea, x, treeb, b, treec))
      else
        Good (Three(treea, a, treeb, x, treec))
  fun lookupE NONE = false
    | lookupE (SOME x) = true
end;

functor TDict(structure KeyS:COMPARABLE):>DICT where type Key.t=KeyS.t = struct
  structure Spec:SPEC=DSpec(structure KeyS=KeyS);
  structure Frame= TFrame(structure Spec= Spec);

  structure Key:COMPARABLE=KeyS;
  type 'vt dict= 'vt Frame.frame;

  val empty= Frame.empty;
  val insert= Frame.insert;
  val lookup= Frame.lookup;
  val removeKey= Frame.remove;
  val allItems= Frame.items;
  val insertAll= Frame.insertAll;
  val union= Frame.union;
end;

functor TSet(structure KeyS:COMPARABLE):>OSET where type Key.t=KeyS.t = struct
  structure Spec:SPEC=SSpec(structure KeyS=KeyS);
  structure Frame= TFrame(structure Spec= Spec);

  structure Key:COMPARABLE=KeyS;
  type oset= unit Frame.frame;

  val empty= Frame.empty;
  val insert= Frame.insert;
  val member= Frame.lookup;
  val remove= Frame.remove;
  val allItems= Frame.items
  val insertAll= Frame.insertAll;
  val union= Frame.union;
end;

(*
structure mystrcomp: COMPARABLE= struct
        type t= string;
        val cmp = String.compare;
end;

structure TD= TDict(structure KeyS=mystrcomp);
structure TS= TSet(structure KeyS=mystrcomp);
*)

infix 1 >>=
signature MONAD =
sig
  type 'a m
  val return : 'a -> 'a m
  val >>= : 'a m * ('a -> 'b m) -> 'b m
end;

structure OptionM : MONAD =
struct
  type 'a m = 'a option
  val return = SOME
  fun x >>= k = Option.mapPartial k x
end;
open OptionM

structure mystrcomp: COMPARABLE= struct
        type t= string;
        val cmp = String.compare;
end;

structure TD= TDict(structure KeyS=mystrcomp);
structure TS= TSet(structure KeyS=mystrcomp);

fun set_difference set_ [] = set_
  | set_difference set_ (a::b) = set_difference (#2 (TS.remove (a, set_))) b

fun map_remove_keys map_ [] = map_
  | map_remove_keys map_ (a::b) = map_remove_keys (#2 (TD.removeKey (a, map_))) b

fun map_drop mapa [] = mapa
  | map_drop mapa (a::b) = map_drop (#2 (TD.removeKey (a, mapa))) b

type label = string
datatype lterm = Number of (IntInf.int)
                        |Label of label
                        |App of lterm*lterm
                        |Abs of label*lterm
                        |Let of ((label*lterm) list) * lterm

type label = string
datatype TTerm = VAR of label
                | ARR of TTerm * TTerm
                | LIST of TTerm
                | INT
                | FOREACH of label list * TTerm

fun debugl (Number x) = IntInf.toString x
  | debugl (Label label) = label
  | debugl (App (lterma, ltermb)) =
      concat ["App (", (debugl lterma), ", ", (debugl ltermb), ")"]
  | debugl (Abs (label, lterma)) =
      concat ["Abs (", label, ", ", (debugl lterma), ")"]
  | debugl (Let ([], lterm)) = concat [" in (", (debugl lterm), ")"]
  | debugl (Let ((label, ltermi)::b, lterm)) = concat ["Let ",
      label,
      " = ",
      (debugl ltermi),
      " ; ",
      (debugl (Let (b, lterm)))]


fun debugt INT = "INT"
  | debugt (VAR label) = label
  | debugt (ARR (tterma, ttermb)) =
      concat ["ARR (", (debugt tterma), ", ", (debugt ttermb), ")"]
  | debugt (LIST tterm) = concat ["{", (debugt tterm), "} list"]
  | debugt (FOREACH ([], tterm)) = concat [": ", debugt tterm]
  | debugt (FOREACH (label::b, tterm)) =
      concat ["FOREACH ", label, " ", debugt (FOREACH (b, tterm))]

fun debugc' [] = "]"
  | debugc' ((a, c)::b) = concat [a, " => ", debugt c, "\n", debugc' b]
fun debugc context = debugc' (TD.allItems context)

type name = char list
datatype term = Fun of name * term list | Var of name
type substitution = (name * term) list (* triangular form !!! *)

fun gettypevar counter = VAR (String.concat ["v", Int.toString counter])

fun termtotterm (Var name) = VAR (implode name)
  | termtotterm (Fun ([#"A"], [terma, termb])) =
      ARR (termtotterm terma, termtotterm termb)
  | termtotterm (Fun ([#"L"], [terma])) = LIST (termtotterm terma)
  | termtotterm (Fun ([#"I"], [])) = INT

fun termstotterms' [] result = result
  | termstotterms' ((a,b)::c) result =
      termstotterms' c (TD.insert ((implode a, termtotterm b), result))
fun termstotterms arr = termstotterms' arr TD.empty

fun ttermtoterm (VAR name) = Var (explode name)
  | ttermtoterm (ARR (terma, termb)) =
      (Fun ([#"A"], [ttermtoterm terma, ttermtoterm termb]))
  | ttermtoterm (LIST (terma)) = (Fun ([#"L"], [ttermtoterm terma]))
  | ttermtoterm INT = (Fun ([#"I"], []))

fun free_variables' (VAR label) set = TS.insert (label, set)
  | free_variables' (ARR (tterma, ttermb)) set =
    let val set' = free_variables' tterma set
    in
      free_variables' ttermb set'
    end
  | free_variables' (LIST tterm) set = free_variables' tterm set
  | free_variables' INT set = set
  | free_variables' (FOREACH (labels, tterm)) set =
      set_difference (free_variables' tterm set) labels (* nasty *)

fun free_variables tterm = free_variables' tterm TS.empty

fun free_variables_context' [] result = result
  | free_variables_context' (a::b) result =
      free_variables_context' b (TS.union (result, (free_variables a)))
fun free_variables_context context =
    free_variables_context'
        (map (fn (x, y) => y) (TD.allItems context))
        TS.empty

fun unfold substitution (VAR (label)) =
    let val found = TD.lookup (label, substitution)
    in
      if isSome found then
        unfold substitution (valOf found)
      else
        VAR (label)
    end
  | unfold substitution (ARR (tterma, ttermb)) =
      ARR (unfold substitution tterma, unfold substitution ttermb)
  | unfold substitution (LIST tterm) = LIST (unfold substitution tterm)
  | unfold substitution INT = INT
  | unfold substitution (FOREACH (lst, tterm)) =
      FOREACH (lst, unfold (map_drop substitution lst) tterm)

fun apply subst (VAR label) =
    let val el = TD.lookup (label, subst)
    in
      if el = NONE then
        VAR label
      else
        valOf el
    end
  | apply subst (ARR (tterma, ttermb)) =
      ARR (apply subst tterma, apply subst ttermb)
  | apply subst (LIST tterm) = LIST (apply subst tterm)
  | apply subst INT = INT
  | apply subst (FOREACH (labels, tterm)) =
      FOREACH (labels, apply (map_drop subst labels) tterm)

fun apply_to_context' subst [] result = result
  | apply_to_context' subst ((a, b)::c) result =
      apply_to_context' subst c (TD.insert ((a, unfold subst b), result))
fun apply_to_context subst context =
    apply_to_context' subst (TD.allItems context) TD.empty

fun compose_substitution substb substa =
  let
    val mymap = fn (x, y) => (x, apply substb y)
    val mapped = map mymap (TD.allItems substa)
  in
    TD.insertAll (mapped, substb)
  end

fun generalize tterm context =
  let
    val ttermvarsset = free_variables tterm
    val contextvars = TS.allItems (free_variables_context context)
  in
    FOREACH (TS.allItems (set_difference ttermvarsset contextvars), tterm)
  end

fun instantiate' (FOREACH ([], tterm)) counter subst =
    (unfold subst tterm, counter)
  | instantiate' (FOREACH (a::b, tterm)) counter subst =
    instantiate' (FOREACH (b, tterm))
        (counter + 1)
        (TD.insert ((a, gettypevar counter), subst))

fun instantiate (FOREACH a) counter =
    instantiate' (FOREACH a) counter TD.empty
  | instantiate ttetm counter = (ttetm, counter)

fun ensure_type_in_context [] context counter = (context, counter)
  | ensure_type_in_context ((a, b) :: c) context counter =
      if isSome (TD.lookup (a, context)) then
        (context, counter)
      else
        ensure_type_in_context
            c
            (TD.insert ((a, gettypevar counter), context))
            (counter + 1)

signature UNIFIER = sig
  val unify: term -> term -> substitution option
  val lunify : (term list * term list) option -> substitution option
end

functor TYPECHECK(U:UNIFIER) = struct
  fun typecheck' (Number x) context counter = (SOME INT, TD.empty, counter)
    | typecheck' (Label label) context counter =
      let val processed = Option.map
          (fn x => instantiate x counter)
          (TD.lookup (label, context))
      in
        if isSome processed then
          (SOME (#1 (valOf processed)),
              TD.empty,
              #2 (valOf processed))
        else
          (NONE, TD.empty, 0)
      end
    | typecheck' (Abs (label, term)) context counter =
      let
        val origintype = gettypevar counter
        val processed = typecheck' term
            (TD.insert ((label, origintype), context))
            (counter + 1)
        val newtype = unfold (#2 processed) origintype
      in
        if isSome (#1 processed) then
          (*(print "ltype\n";
          print ((debugl (Abs (label, term)))^ "\n");
          print "ttype\n";
          print ((debugt (ARR (newtype, valOf (#1 processed))))^ "\n");*)
          (SOME (ARR (newtype,
                  valOf (#1 processed))),
              #2 processed,
              #3 processed)
        else
          (*(print "ltype\n";
          print ((debugl (Abs (label, term)))^ "\n");
          print "ttype\nNONE1\n";*)
          (NONE, TD.empty, 0)
      end
    | typecheck' (App (x, y)) context counter =
      let
        val tvar = gettypevar counter
        val xprocessed = typecheck' x context (counter + 1)
      in
        if isSome (#1 xprocessed) then
          let val yprocessed = typecheck'
              y
              (apply_to_context (#2 xprocessed) context)
              (#3 xprocessed)
          in
            if isSome (#1 yprocessed) then
              let
                val substopt = U.unify
                    (ttermtoterm (ARR (valOf (#1 yprocessed), tvar)))
                    (ttermtoterm (unfold (#2 yprocessed) (valOf (#1 xprocessed))))
              in
                if isSome substopt then
                  let val unifiedsubs = termstotterms (valOf (substopt))
                  in
                    (*print "ltype1\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype1\n";
                    print ((debugt (unfold unifiedsubs tvar))^ "\n");*)
                    (SOME (unfold unifiedsubs tvar),
                        compose_substitution unifiedsubs
                            (compose_substitution (#2 yprocessed) (#2 xprocessed)),
                        #3 yprocessed)
                  end
                else
                    (*(print "COUND NOT UNIFY\n";
                    print ((debugt (ARR (valOf (#1 yprocessed), tvar)))^ "\n");
                    print "AND\n";
                    print ((debugt (unfold (#2 yprocessed) (valOf (#1 xprocessed))))^ "\n");
                    print "ltype\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype\nNONE2\n";*)
                  (NONE, (compose_substitution (#2 yprocessed) (#2 xprocessed)), 0)
              end
            else
                    (*(print "ltype2\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype2\nNONE3\n";*)
              (NONE, TD.empty, 0)
          end
        else
                    (*(print "ltype3\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype3\nNONE4\n";*)
          (NONE, TD.empty, 0)
      end
    | typecheck' (Let (defs, lterm)) context counter =
        let
          val number = List.length defs
          val defnumbers = List.rev (List.foldl
              (fn (def, lst) =>
                if List.null lst then
                  [(def, counter)]
                else
                  let val (a, b)::c = lst in (def, b + 1)::lst end)
              []
              defs)
          val labels = List.map (fn (x, y) => x) defs
          val defvars = List.map (fn (x, y) => (x, gettypevar y)) defnumbers
          val labelvars = List.map (fn ((label, lterm), var) => (label, var)) defvars
          val ltermvaras = List.map (fn ((label, lterm), var) => (lterm, var)) defvars
          val newcontext = TD.insertAll (labelvars, context)
          fun process [] context' counter' subst' = return (context', counter', subst')
            | process ((lterm, var)::rmd) context' counter' subst' =
                let
                  val (tterm, subst2, counter2) = typecheck' lterm context' counter'
                  val subst3 = tterm
                      >>= (fn x =>
                        ((U.unify (ttermtoterm x) (ttermtoterm (unfold (compose_substitution subst2 subst') var)))
                            >>= (fn y => return (termstotterms y))))
                in
                    (*print "ltype2\n";
                    print ((debugl lterm)^ "\n");
                    print "ttype2\n";
                    print ((debugt (valOf tterm))^ "\n");*)
                  if isSome subst3 then
                    let
                      val s = valOf subst3
                      val part_composed_subst = compose_substitution s subst2
                      val composed_subst = compose_substitution part_composed_subst subst'
                      val new_context = apply_to_context part_composed_subst context'
                    in
                      (*print "subst'\n";
                      print ((debugc subst')^ "\n");
                      print "subst2\n";
                      print ((debugc subst2)^ "\n");
                      print "subst3\n";
                      print ((debugc s)^ "\n");
                      print "part_composed_subst\n";
                      print ((debugc part_composed_subst)^ "\n");
                      print "composed_subst\n";
                      print ((debugc composed_subst)^ "\n");
                      print "new_context\n";
                      print ((debugc new_context)^ "\n");*)
                      process rmd new_context counter2 composed_subst
                    end
                  else
                    NONE
                end
          val processed = process ltermvaras newcontext (counter + number) TD.empty
        in
          if isSome processed then
            let
              val (context', counter', subst') = valOf processed
              val togeneralize = List.map (fn x => (x, Option.valOf (TD.lookup (x, context')))) labels
              val context'' = map_remove_keys context' labels
              val generalized = List.map (fn (x, y) => (x, generalize y context'')) togeneralize
              val (ittype, isubst, icounter) = typecheck' lterm (TD.insertAll (generalized, context'')) counter'
            in
              (ittype, compose_substitution isubst subst', icounter)
            end
          else
            (NONE, TD.empty, 0)
        end
  fun typecheck'' term [] generalized_context =
      typecheck' term generalized_context 0
    | typecheck'' term ((a, b)::c) generalized_context =
      typecheck''
          term
          c
          (TD.insert ((a, generalize b TD.empty), generalized_context))
 (*fun typecheck term context =
      let val processed = (typecheck'' term context TD.empty)
      in
        if isSome (#1 processed) then
          (#1 processed, TD.allItems (#2 processed), #3 processed)
        else
          (NONE, TD.allItems (#2 processed), #3 processed)
      end*)
  fun typecheck term context =
      #1 (typecheck'' term context TD.empty)
end
