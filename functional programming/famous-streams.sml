fun fibadd i [] = [i]
  | fibadd i (x::y) =
  if i + 1 < x then
    i::(x::y)
  else
    fibadd (i + 2) y;
fun fibonacci' (x::y) =
  if x = 0 then
    smemo (fn _ => (1, fibonacci' (fibadd 1 y)))
  else
    smemo (fn _ => (0, fibonacci' (fibadd 0 (x::y))));
val fibonacci = smemo (fn _ => (0, fibonacci' [0]));
fun rollkolakoski ((x, 0)::y) =
    let
      val rx::ry = rollkolakoski y
    in
      (3 - x, (#1 rx) - 1)::(rx::ry)
    end
  | rollkolakoski ((x, 1)::y) = (x, 0)::y
  | rollkolakoski [] = [(2, 0)];
fun kolakoski' lst = smemo (fn _ => (
    let val x::y = rollkolakoski lst
    in
      (#1 x, kolakoski' (x::y))
    end
));
val kolakoski = smemo (fn _ => (1, smemo (fn _ => (2, kolakoski' []))));
fun thuemorsefn n =
  if n = 0 then
    0
  else if (n mod 2) = 1 then
    1 - thuemorsefn (n div 2)
  else
    thuemorsefn (n div 2);
fun thuemorse' n = smemo (fn _ => (thuemorsefn n, thuemorse' (n + 1)));
val thuemorse = thuemorse' 0;
