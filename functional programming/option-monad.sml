infixr 1 >>=
signature MONAD =
sig
  type 'a m
  val return : 'a -> 'a m
  val >>= : 'a m * ('a -> 'b m) -> 'b m
end;

structure OptionM : MONAD =
struct
  type 'a m = 'a option
  val return = SOME
  fun x >>= k = Option.mapPartial k x
end;
