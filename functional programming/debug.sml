typecheck' (Let ((label, lterm)::c, ltermb)) context counter =
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
                    apply_to_context
                      subst'
                      (TD.insert ((label, generalizedtterm), (#1 context_counter)))
                  val processedb = typecheck' (Let (c, ltermb)) newcontext counter'
                in
                  if isSome (#1 processedb) then
                    (#1 processedb,
                        compose_substitution subst' (#2 processedb),
                        #3 processedb)
                  else
                    (NONE, #2 processedb, 7)
                end
              else
                (NONE, subst, 6)
            end
          else
            (NONE, TD.empty, 5)
        end
        
        
        typecheck' (Let (defs, lterm)) context counter =
        let
          val vars = List.map (fn (x, y) => x) defs
          val number = List.length vars
          val vardummytypes = List.rev (List.foldl
              (fn (var, lst) =>
                if List.null lst then
                  [(var, counter)]
                else
                  let val (a, b)::c = lst in (var, b + 1)::lst end)
              []
              vars)
          val vartypes = List.map (fn (x, y) => (x, gettypevar y)) vardummytypes
          val newcontext = TD.insertAll (vartypes, context)
          val 
        in
          typecheck' lterm some_new_context (counter + number)
        end
        
val xxx = let
  fun id x = x
  fun id2 a =
    let
      val b = id a
      val c = hd b
    in id b end
in id2 end;
