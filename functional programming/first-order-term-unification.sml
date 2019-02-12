use "2-3trees-functors.sml";
use "option-monad.sml";
open OptionM

(*type name = char list
datatype term = Fun of name * term list | Var of name*)

structure mystrcomp: COMPARABLE= struct
        type t= name;
        val cmp = List.collate Char.compare;
end;

structure TD= TDict(structure KeyS=mystrcomp);

type substitution = (name * term) list

fun dfs_step nei color [] result = result
  | dfs_step nei color (v::remaining) result =
    if Array.sub (color, v) = 2 then
      dfs_step nei color remaining result
    else
      if Array.sub (color, v) = 1 then 
        NONE
      else
        let
          val save = Array.update (color, v, 1)
          val part_result = dfs_step nei color (Vector.sub (nei, v)) result
          val save' = Array.update (color, v, 2)
          val part_result' = part_result >>= (fn rslt => return (v::rslt))
        in
          dfs_step nei color remaining part_result'
        end

fun dfs_all v nei color i result =
  if v = i then
    result
  else
    let val processed = dfs_step nei color [i] result
    in
      dfs_all v nei color (i + 1) processed
    end

fun topological_sort g =
  let
    val v = length g
    val nei = Vector.fromList g
    val color = Array.array(v, 0)
  in
    dfs_all v nei color 0 (return [])
  end

fun inverse_permutation' [] arr counter = arr
  | inverse_permutation' (a::b) arr counter = (
      Array.update (arr, a, counter);
      inverse_permutation' b arr (counter + 1))
fun inverse_permutation lst = Array.vector
    (inverse_permutation' lst (Array.array (length lst, 0)) 0)

fun index_elements [] indexing_fn arr = ()
  | index_elements (a::b) indexing_fn arr = (
      Array.update (arr, indexing_fn a, a);
      index_elements b indexing_fn arr)

fun loop_processing func [] (a::b) result = NONE
  | loop_processing func (a::b) [] result = NONE
  | loop_processing func [] [] result = SOME result
  | loop_processing func (a::arem) (b::brem) result =
    let val uni = func a b result
    in
      if isSome uni then
        loop_processing func arem brem (valOf uni)
      else
        NONE
    end

fun update func at newval result =
  let val old = TD.lookup (at, result)
  in
    if isSome old then
      func (valOf old) newval result
    else
      SOME (TD.insert ((at, newval), result))
  end

fun try_unify (Var a) (Var b) result =
    if EQUAL = (List.collate Char.compare (a, b)) then
      SOME result
    else
      update try_unify a (Var b) result
  | try_unify (Var a) (Fun (name, term)) result =
      update try_unify a (Fun (name, term)) result
  | try_unify (Fun (name, term)) (Var a) result =
      update try_unify a (Fun (name, term)) result
  | try_unify (Fun (namea, terma)) (Fun (nameb, termb)) result =
      if EQUAL = (List.collate Char.compare (namea, nameb)) then
        loop_processing try_unify terma termb result
      else
        NONE

fun extract_names' (Var name) result = name::result
  | extract_names' (Fun (name, [])) result = result
  | extract_names' (Fun (name, (a::b))) result =
      extract_names' (Fun (name, b)) (extract_names' a result)
fun extract_names term = extract_names' term []

fun get_all_words' [] result = result
  | get_all_words' ((a, b)::c) result =
      let val (fst, snd) = result
      in
        get_all_words' c (a::fst, b@snd)
      end
fun get_all_words desc =
  let val (fst, snd) = get_all_words' desc ([], [])
  in
    fst@snd
  end

fun assign_numbers [] start result = (result, start)
  | assign_numbers (a::b) start result =
      if isSome (TD.lookup (a, result)) then
        assign_numbers b start result
      else
        assign_numbers b (start + 1) (TD.insert ((a, start), result))

fun insertall [] arr = ()
  | insertall ((a,b)::c) arr =
    let val save = Array.update (arr, a, b)
    in
      insertall c arr
    end

fun build_graph desc =
    let
      val conv_desc = map (fn (x, y) => (x, extract_names y)) desc
      val all_words = get_all_words conv_desc
      val (nametonumber, unique_count) = assign_numbers all_words 0 TD.empty
      val mapped = map
          (fn (x, y) => (valOf (TD.lookup (x, nametonumber)),
              map (fn z => valOf (TD.lookup (z, nametonumber))) y))
          conv_desc
      val arr = Array.array (unique_count, [])
      val save = insertall mapped arr
    in
      (arr, nametonumber)
    end

fun arrayToList arr = Array.foldr (op ::) [] arr

(*fun print_int_list lst = print (concat ["[", concat (map (fn x => concat [Int.toString x, ","]) lst), "]\n"])*)

fun unify a b =
    let val uni = try_unify a b TD.empty
    in
      if isSome uni then
        let
          val allitems = TD.allItems (valOf uni)
          val (graph, translation) = build_graph (allitems)
          val toposort = topological_sort (arrayToList (graph))
          val unisize = length allitems
          val indexed = Array.array (unisize, ([], Var []))
        in
          toposort >>= (fn ts => return (List.filter (fn x => x < unisize) ts))
                   >>= (fn ts => return (inverse_permutation ts))
                   >>= (fn positions => (index_elements
                           allitems
                           (fn (x, y) => (Vector.sub(positions,
                               valOf (TD.lookup (x, translation)))))
                           indexed;
                       return (arrayToList indexed)))
        end
      else
        NONE
    end

(*
  val t1 = Fun ([#"f"],[Var [#"x"],Fun ([#"g"],[Var [#"x"],Var [#"y"]])]) : term
  val t2 = Fun ([#"f"],[Fun ([#"h"],[Var [#"y"]]),Var [#"z"]]) : term
  val t3 = Fun ([#"f"],[Fun ([#"h"],[Var [#"z"]]),Var [#"z"]]) : term
  - unify t1 t2;
  val it =
    SOME
  [([#"z"],Fun ([#"g"],[Var [#"x"],Var [#"y"]])),
   ([#"x"],Fun ([#"h"],[Var [#"y"]]))] : (name * term) list option
  - unify t1 t3;
  val it = NONE : (name * term) list option
*)
