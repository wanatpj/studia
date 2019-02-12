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
