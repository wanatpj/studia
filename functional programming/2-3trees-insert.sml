datatype 'a node = Two of 'a node * 'a * 'a node |
                 Three of 'a node * 'a * 'a node * 'a * 'a node |
                 Empty;

fun st (Two (a, b, c)) = a
fun nd (Two (a, b, c)) = b
fun rd (Two (a, b, c)) = c

fun insert' cmp (x, Empty) = (true, Two (Empty, x, Empty))
  | insert' cmp (x, Two (left, y, right)) =
    let val c = cmp (x, y)
    in
      if (c < 0) then
        let
          val nod = insert' cmp (x, left)
          val tree = #2 nod
        in
          if (#1 nod) then
            (false, Three (st tree, nd tree, rd tree, y, right))
          else
            (false, Two (tree, y, right))
        end
      else
        let
          val nod = insert' cmp (x, right)
          val tree = #2 nod
        in
          if (#1 nod) then
            (false, Three (left, y, st tree, nd tree, rd tree))
          else
            (false, Two (left, y, tree))
        end
    end
  | insert' cmp (x, Three (left, y, middle, z, right)) =
    if (cmp (x, y) <= 0) then
      let
        val nod = insert' cmp (x, left)
        val tree = #2 nod
      in
        if (#1 nod) then
          (true, Two (tree, y, Two (middle, z, right)))
        else
          (false, Three (tree, y, middle, z, right))
      end
    else if cmp (x, z) <= 0 then
      let
        val nod = insert' cmp (x, middle)
        val tree = #2 nod
      in
        if (#1 nod) then
          (true, Two (Two (left, y, st tree), nd tree, Two (rd tree, z, right)))
        else
          (false, Three (left, y, tree, z, right))
      end
    else
      let
        val nod = insert' cmp (x, right)
        val tree = #2 nod
      in
        if (#1 nod) then
          (true, Two (Two (left, y, middle), z, tree))
        else
          (false, Three (left, y, middle, z, tree))
      end

fun insert cmp what = #2 (insert' cmp what)


