
datatype movee = LL | MM | RR;

fun search cmp (x, Empty) = false |
  search cmp (x, Two (left, y, right)) =
    let val c = cmp (x, y)
    in
      if (c < 0) then
        search cmp (x, left)
      else if (c > 0) then
        search cmp (x, right)
      else true
    end |
  search cmp (x, Three (left, y, middle, z, right)) =
    let val c = cmp (x, y)
    in
      if (c < 0) then
        search cmp (x, left)
      else if (c > 0) then
        let val d = cmp (x, z)
        in
          if (d < 0) then
            search cmp (x, middle)
          else if (d > 0) then
            search cmp (x, right)
          else true
        end
      else true
    end

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

fun remove' cmp (a, Two (Empty, b, Empty)) =
    if (cmp (a, b)) = 0 then
      (true, Empty)
    else (false, Two (Empty, b, Empty))
  | remove' cmp (a, Three (Empty, b, Empty, c, Empty)) =
    if (cmp (a, b)) = 0 then
      (false, Two (Empty, c, Empty))
    else if (cmp (a, c)) = 0 then
      (false, Two (Empty, b, Empty))
    else
      (false, Three (Empty, b, Empty, c, Empty))
  | remove' cmp (x, Two (s, a, t)) =
    let
      val c = cmp (x, a)
    in
      if c = 0 then
        let val processed = remove_min t
        in
          if #1 processed then
            rotate RR (Two (s, #3 processed, #2 processed))
          else
            (false, Two (s, #3 processed, #2 processed))
        end
      else if c < 0 then
        let val processed = remove' cmp (x, s)
        in
          if #1 processed then
            rotate LL (Two (#2 processed, a, t))
          else
            (false, Two (#2 processed, a, t))
        end
      else
        let val processed = remove' cmp (x, t)
        in
          if #1 processed then
            rotate RR (Two (s, a, #2 processed))
          else
            (false, Two (s, a, #2 processed))
        end
    end
  | remove' cmp (x, Three (s, a, t, b, u)) =
    let val c = cmp (x, a)
    in
      if c = 0 then
        let val processed = remove_min t
        in
          if #1 processed then
            rotate MM (Three (s, #3 processed, #2 processed, b, u))
          else
            (false, Three (s, #3 processed, #2 processed, b, u))
        end
      else if c < 0 then
        let val processed = remove' cmp (x, s)
        in
          if #1 processed then
            rotate LL (Three (#2 processed, a, t, b, u))
          else
            (false, Three (#2 processed, a, t, b, u))
        end
      else
        let val d = cmp (x, b)
        in
          if d = 0 then
            let val processed = remove_min u
            in
              if #1 processed then
                rotate RR (Three (s, a, t, #3 processed, #2 processed))
              else
                (false, Three (s, a, t, #3 processed, #2 processed))
            end
          else if d < 0 then
            let val processed = remove' cmp (x, t)
            in
              if #1 processed then
                rotate MM (Three (s, a, #2 processed, b, u))
              else
                (false, Three (s, a, #2 processed, b, u))
            end
          else
            let val processed = remove' cmp (x, u)
            in
              if #1 processed then
                rotate RR (Three (s, a, t, b, #2 processed))
              else
                (false, Three (s, a, t, b, #2 processed))
            end
        end
    end

fun remove cmp what =
  if search cmp what then
    let val processed = remove' cmp what
    in
      (true, #2 processed)
    end
  else
    (false, #2 what)
