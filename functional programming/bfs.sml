datatype 'a Tree = Leaf | Branch of 'a Tree * 'a * 'a Tree

fun prebf Leaf ldep = ldep
  | prebf (Branch (treea, a, treeb)) [] =
    1 :: (prebf treeb (prebf treea []))
  | prebf (Branch (treea, a, treeb)) (same::deeper) =
    (same + 1) :: (prebf treeb (prebf treea deeper))

fun bfnum' Leaf ldep dep acc = (Leaf, ldep)
  | bfnum' (Branch (treea, a, treeb)) [] (dep::rem) acc =
    let
      val processeda = bfnum' treea [] rem (acc + dep)
      val processedb = bfnum' treeb (#2 processeda) rem (acc + dep)
    in
      (Branch (#1 processeda, (a, acc), #1 processedb), 1 :: (#2 processedb))
    end
  | bfnum' (Branch (treea, a, treeb)) (same::deeper) (dep::rem) acc =
    let
      val processeda = bfnum' treea deeper rem (acc + dep)
      val processedb = bfnum' treeb (#2 processeda) rem (acc + dep)
    in
      (Branch (#1 processeda, (a, same + acc), #1 processedb), (same + 1) :: (#2 processedb))
    end

fun bfnum tree = #1 (bfnum' tree [] (prebf tree []) 0)
