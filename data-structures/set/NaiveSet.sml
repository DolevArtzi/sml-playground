(* naive set implementation using lists
    - highly inefficient, but can serve as a refsol for other set
      implementations 
    - can ignore polyequal warning (see SET.sig: equality types)
    - implements map and reduce currently
    - author: @DolevArtzi
*)
structure NaiveSet :> SET =
struct
  datatype 'a set = NvSt of 'a list
  exception Range
  fun unwrap (NvSt l) = l
  fun pack l = (NvSt l)

  fun member' _ (NvSt []) x = false
    | member' f (NvSt (y::ys)) x = f x y orelse member' f (pack ys) x
  fun member s x = member' (fn x => fn y => x = y) s x

  fun toSet [] = NvSt []
    | toSet (x::xs) = let
      val w = toSet xs
    in
      if member w x then w else pack(x::(unwrap(toSet xs)))
    end
  val emp = NvSt []
  fun empty () = emp

  fun singleton x = pack (x::[])

  
  fun add s x = case member s x of
                  true => s
                | _ => pack(x::(unwrap s))

  fun remove' (s as (NvSt [])) _ = (false,s)
    | remove' (s as (NvSt (y::ys))) x = let
      val ys_set = pack ys
    in
      if x = y then (true, pack ys)
      else let
        val (b,s') = remove' ys_set x
      in
        (b,pack(y::(unwrap s')))        
      end
    end

  fun snd (a,b) = b
  val remove = fn s => snd o (remove' s)

  fun Zn 0 = emp
    | Zn n = case n >= 0 of
               true => add (Zn (n-1)) n
             | false => raise Range
  val add' = fn n => fn S => add S n
  val Zn' = (add' 0) o Zn

  fun union (S as (NvSt l)) (S' as (NvSt l')) = toSet (List.revAppend(l, l'))
  
  fun difference X (NvSt []) = X
    | difference X (NvSt (y::ys)) = difference (remove X y) (pack ys)

  fun intersect x y = difference (difference (union x y) (difference x y)) (difference y x)

  fun setEq' _ (NvSt []) = true
    | setEq' S (NvSt (y::ys)) = member S y andalso setEq' S (pack ys)

  fun len S = List.length(unwrap S)

  fun setEq X Y = if len X = len Y then setEq' X Y else false

  val f = Int.toString
  fun toString' _ [] = ""
    | toString' f (x::xs) = let
      val z = (case xs of [] => "" | _ => ", ")
    in
      f x ^ z ^ (toString' f xs)
    end

  val enclose = (fn z => "{" ^ z ^ "}")
  val toString = fn f => enclose o (toString' f) o unwrap
  val f = enclose o (toString' (Int.toString)) o unwrap
  val f' = enclose o (toString' (Fn.id)) o unwrap
  val p = toString
  val % = toSet

  fun naiveReduce f (NvSt []) b = b
    | naiveReduce f (NvSt (x::xs))b = f x (naiveReduce f (pack xs) b)

  val reduce = naiveReduce
  val reduce1toN = fn n => fn f => fn b => reduce f (Zn n) b

  val rec map' = fn f => (fn [] => []
                       | (x::xs) => f x :: (map' f xs))
  val map = fn f => toSet o ((map' f) o unwrap)
end

