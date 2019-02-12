use "2-3trees-functors.sml";
use "option-monad.sml";
open OptionM

structure mystrcomp: COMPARABLE= struct
        type t= string;
        val cmp = String.compare;
end;

structure TD= TDict(structure KeyS=mystrcomp);
structure TS= TSet(structure KeyS=mystrcomp);

fun set_difference set_ [] = set_
  | set_difference set_ (a::b) = set_difference (#2 (TS.remove (a, set_))) b

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
  fun typecheck' (Number x) context counter = return (INT, TD.empty, counter)
    | typecheck' (Label label) context counter =
        (TD.lookup (label, context)) >>=
            (fn x => return (instantiate x counter)) >>=
            (fn (typ, counter) => return (typ, TD.empty, counter))
      end
    | typecheck' (Abs (label, term)) context counter =
        (return (gettypevar counter))
            >>= (fn origintype => (typecheck' term
                (TD.insert ((label, origintype), context))
                (counter + 1))
                >>= (fn (itype, isubst, icounter) =>
                    return (
                        ARR (unfold (#2 processed) origintype, itype),
                        isubst,
                        icounter)))
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
                    print "ltype\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype\n";
                    print ((debugt (unfold unifiedsubs tvar))^ "\n");
                    (SOME (unfold unifiedsubs tvar),
                        compose_substitution unifiedsubs
                            (compose_substitution (#2 yprocessed) (#2 xprocessed)),
                        #3 yprocessed)
                  end
                else
                    (print "COUND NOT UNIFY\n";
                    print ((debugt (ARR (valOf (#1 yprocessed), tvar)))^ "\n");
                    print "AND\n";
                    print ((debugt (unfold (#2 yprocessed) (valOf (#1 xprocessed))))^ "\n");
                    print "ltype\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype\nNONE2\n";
                  (NONE, (compose_substitution (#2 yprocessed) (#2 xprocessed)), 0))
              end
            else
                    (print "ltype\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype\nNONE3\n";
              (NONE, TD.empty, 0))
          end
        else
                    (print "ltype\n";
                    print ((debugl (App (x, y)))^ "\n");
                    print "ttype\nNONE4\n";
          (NONE, TD.empty, 0))
      end
    | typecheck' (Let ((label, lterm)::c, ltermb)) context counter =
        let
          val context_counter =
              ensure_type_in_context ((label, lterm)::c) context counter
          val tvar = valOf (TD.lookup (label, #1 context_counter))
          val processed = typecheck'
              lterm
              (#1 context_counter)
              (#2 context_counter)
        in
          if isSome (#1 processed) then
            let
              val (SOME tterm, subst, counter') = processed
              val unified = U.unify
                  (ttermtoterm (unfold subst tvar))
                  (ttermtoterm tterm)
            in
              if isSome unified then
                let
                  val unifiedsubs = termstotterms (valOf (unified))
                  val subst' = compose_substitution
                      unifiedsubs
                      subst
                  val generalizedtterm =
                      generalize (unfold unifiedsubs tterm) (context)
                  val newcontext =
                    (print "INSIDE ltype\n";
                    print ((debugl lterm)^ "\n");
                    print "INSIDE ttype\n";
                    print ((debugt generalizedtterm)^ "\n");
                    print "ON CONTEXT:\n";
                    print ((debugc (apply_to_context subst' context))^ "\n");
                    print "PREV CONTEXT:\n";
                    print ((debugc (#1 context_counter))^ "\n");
                    apply_to_context
                      subst'
                      (TD.insert ((label, generalizedtterm), (#1 context_counter))))
                  val processedb = typecheck' (Let (c, ltermb)) newcontext counter'
                in
                  if isSome (#1 processedb) then
                    (print "ltype\n";
                    print ((debugl (Let ((label, lterm)::c, ltermb)))^ "\n");
                    print "ttype\n";
                    print ((debugt (valOf (#1 processedb)))^ "\n");
                    (#1 processedb,
                        compose_substitution subst' (#2 processedb),
                        #3 processedb))
                  else
                    (print "ltype\n";
                    print ((debugl (Let ((label, lterm)::c, ltermb)))^ "\n");
                    print "ttype\nNONE7\n";
                    (NONE, #2 processedb, 7))
                end
              else
                    (print "COUND NOT UNIFY\n";
                    print ((debugt (unfold subst tvar))^ "\n");
                    print "AND\n";
                    print ((debugt tterm)^ "\n");
                    print "ltype\n";
                    print ((debugl (Let ((label, lterm)::c, ltermb)))^ "\n");
                    print "ttype\nNONE6\n";
                (NONE, subst, 6))
            end
          else
                    (
                    print "ltype\n";
                    print ((debugl (Let ((label, lterm)::c, ltermb)))^ "\n");
                    print "ttype\nNONE5\n";
            (NONE, TD.empty, 5))
        end
    | typecheck' (Let ([], lterm)) context counter =
        typecheck' lterm context counter
  (*fun typecheck term context =
      let val processed = (typecheck' term (TD.insertAll (context, TD.empty)) 0)
      in
        if isSome (#1 processed) then
          (#1 processed, TD.allItems (#2 processed), #3 processed)
        else
          (NONE, TD.allItems (#2 processed), #3 processed)
      end*)
  fun typecheck term context =
      #1 (typecheck' term (TD.insertAll (context, TD.empty)) 0)
end

(*
some of code is based on https://github.com/wh5a/Algorithm-W-Step-By-Step/blob/master/AlgorithmW.pdf
*)
