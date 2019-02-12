fun dfs_step nei color [] result = result
  | dfs_step nei color (v::remaining) result =
    if Array.sub (color, v) then
      dfs_step nei color remaining result
    else
      let
        val save = Array.update (color, v, true)
        val part_result = dfs_step nei color (Vector.sub (nei, v)) (v :: result)
      in
        dfs_step nei color remaining part_result
      end

fun dfs_all v nei color i result =
  if v = i then
    result
  else
    let val part_result = dfs_step nei color [i] result
    in
      dfs_all v nei color (i + 1) part_result
    end

fun dfs g =
  let
    val v = length g
    val nei = Vector.fromList g
    val color = Array.array(v, false)
  in
    rev (dfs_all v nei color 0 [])
  end
