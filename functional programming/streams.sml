fun sconst x = smemo (fn stream => (x, stream));
fun snth 0 stream = shd stream
  | snth n stream = snth (n-1) (stl stream);
fun stake 0 stream = []
  | stake n stream = (shd stream)::(stake (n-1) (stl stream));
fun smap f stream = smemo (fn _ => (f (shd stream), smap f (stl stream)));
fun smap1 f stream = smemo (fn _ => (f (shd stream), stl stream));
fun snat s z = smemo (fn _ => (z, snat s (s z)));
fun stab' f z = smemo (fn _ => (f(z), stab' f (z + 1)));
fun stab f = stab' f 0;
fun szip streama streamb = smemo (fn _ => ((shd streama, shd streamb), szip (stl streama) (stl streamb)));
fun szipwith f streama streamb = smemo (fn _ => (f (shd streama, shd streamb), szipwith f (stl streama) (stl streamb)));
fun sfoldl' f start stream = smemo (fn _ =>
    let val fst = f (start, shd stream)
    in
      (fst, sfoldl' f fst (stl stream))
    end);
fun sfoldl f start stream = smemo (fn _ => (start, sfoldl' f start stream));
fun srev stream = stl (sfoldl (fn (x, y) => y::x) [] stream);
fun sfilter pred stream = smemo (fn _ => (
  if pred (shd stream) then
    (shd stream, sfilter pred (stl stream))
  else
    seval (sfilter pred (stl stream))
));
fun stakewhile pred stream =
  if pred (shd stream) then
    (shd stream)::(stakewhile pred (stl stream))
  else
    [];
fun srepeat' ls fxpt = smemo(fn _ => (
    case ls of
        [] => seval fxpt
      | x::y => (x, srepeat' y fxpt)
));
fun srepeat lst = smemo(fn fixedpoint => seval (srepeat' lst fixedpoint));
fun spairs stream = smemo(fn _ =>
    ((shd stream, shd (stl stream)), spairs (stl (stl stream))));

fun sntail 0 stream = stream
  | sntail n stream = smemo (fn _ => seval (sntail (n-1) (stl stream)));
fun sjump n stream = smemo (fn _ => (seval stream, sjump n (sntail n stream)));
fun ssplitn' 0 stream = []
  | ssplitn' n stream = (smap (fn x => (#1 x)) stream)
      ::(ssplitn' (n - 1) (smap (fn x => seval (#2 x)) stream));
fun ssplitn n stream = ssplitn' n (sjump n stream);

fun sinterleave' (x::y) revlst = smemo (fn _ =>
    (shd x, sinterleave' y ((stl x)::revlst)))
  | sinterleave' [] revlst = smemo(fn _ => seval (sinterleave' (rev revlst) []));

fun sinterleave streamlist = sinterleave' streamlist [];
