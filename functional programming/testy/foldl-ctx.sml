
type label = string
datatype TTerm = VAR of label | ARR of TTerm * TTerm | LIST of TTerm | INT 

local 
		infixr 6 -->
		fun a --> b= ARR (a,b)
in 
		val context= [
			("PLUS",(INT --> (INT --> INT)) ),
			("CONS", (VAR "a" --> (LIST (VAR "a")) --> (LIST (VAR "a")) ) ),
			("NIL", (LIST (VAR "a"))),
			("hd",((LIST (VAR "a")) --> VAR "a" ) ),
			("tl",((LIST (VAR "a")) --> (LIST (VAR "a") )) ),
			("PAIR",(VAR "a" --> VAR "b" --> VAR "a" --> VAR "b" --> INT ) ),
			("IF",(INT --> VAR "b" --> VAR "b" --> VAR "b" ))
			]
end

local 
	structure LDICT= struct
		val empty:int * ((label*int) list) = (0,[])
		fun lookup_insert (n,[]) (k:string) = (n, (n+1, [(k,n)]))
			|lookup_insert (n,(d as (k,v)::xs)) el = if (el=k) then (v,(n,d))
				else if (el< k) then (n, (n+1,(el,n)::d)) else let
					val (v',(n',d')) = lookup_insert (n,xs) el
				in (v', (n',(k,v)::d'))  end
	end

	fun relabel t= let
		val rdict = ref LDICT.empty
		fun relabel' (VAR label) =let
			val (v,d') = LDICT.lookup_insert (!rdict) label
			val () = rdict:= d'
			val name = "v"^(Int.toString v)			
			in (VAR name) end
			|relabel' (ARR (t1,t2))= ARR (relabel' t1, relabel' t2)
			|relabel' (LIST t)= LIST (relabel' t)
			|relabel' INT = INT
	
		in relabel' t end
	
	fun printTTerm INT = "int"
		|printTTerm (VAR name) = name
		|printTTerm  (ARR (t1,t2)) = "("^(printTTerm t1)^")->"^(printTTerm t2)
		|printTTerm  (LIST t) = "["^(printTTerm t)^"]"
in
fun tterm2str t = let
	val t' = relabel t
	in printTTerm t' end
end

