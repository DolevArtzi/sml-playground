signature SET = sig
  type ''a set
  exception Range
  val emp : ''a set
  val empty : unit -> ''a set
  val singleton : ''a -> ''a set
  val Zn : int -> int set (* [1...n] *)
  val Zn' : int -> int set (* [0...n] *)


  val add : ''a set -> ''a -> ''a set
  val remove : ''a set -> ''a -> ''a set
  (* remove' tells you whether the val was removed *)
  val remove' : ''a set -> ''a -> bool * ''a set

  val toSet : ''a list -> ''a set
  val % : ''a list -> ''a set (* alias for toSet *)
  val unwrap : ''a set -> ''a list


  val union : ''a set -> ''a set -> ''a set
  val intersect : ''a set -> ''a set -> ''a set
  val difference : ''a set -> ''a set -> ''a set

  val member : ''a set -> ''a -> bool
  val setEq : ''a set -> ''a set -> bool
  (* reduce1toN n f b ==> reduce f (Zn n) b *)
  val reduce : (''a -> ''a -> ''a) -> ''a set -> ''a -> ''a
  val reduce1toN : int -> (int -> int -> int) -> int -> int
  (* obviously, map preserves uniqueness (aka not very useful) *)
  val map : (''a -> ''b) -> ''a set -> ''b set
  val filter : (''a -> bool) -> ''a set -> ''a set

  val toString : (''a -> string) -> ''a set -> string
  val f : int set -> string
  val f' : string set -> string
  val p : (''a -> string) -> ''a set -> string
  val len : ''a set -> int
end