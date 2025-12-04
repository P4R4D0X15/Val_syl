(** Signature des modules des types énumérables. *)
module type Enumerable = sig
  type t
  (** Le type à énumérer. *)

  val values : t Seq.t
  (** La séquence contenant les valeurs du type t *)

  val mini : t option
  (** La valeur minimale du type énuméré, si elle existe *)

  val maxi : t option
  (** La valeur maximale du type énuméré, si elle existe *)

  val safe_succ : t -> t option
  (** Renvoie le successeur d'une valeur de type t, s'il existe *)
end

(** Module des entiers énumérables *)
(* module EnumerableInt : Enumerable with type t = int = struct
     type t = int

     let values = failwith "à faire"
     let mini = failwith "à faire"
     let maxi = failwith "à faire"
     let safe_succ (_ : t) = failwith "à faire"
   end *)

(** from_succ succ v_min : calcule une séquence constituée des éléments obtenus en appliquant d'une façon répétée la fonction succ
  depuis la valeur v_min, jusqu'à obtenir None *)
let from_succ (_ : 't -> 't option) (_ : 't) : 't Seq.t = failwith "à faire"

(** Module des caractères énumérables *)
(* module EnumerableChar : Enumerable with type t = char = struct
     type t = char

     let values = failwith "à faire"
     let mini = failwith "à faire"
     let maxi = failwith "à faire"
     let safe_succ (_ : t) = failwith "à faire"
   end *)

(** Signature des modules des types énumérables basés une sous-partie des entiers. *)
module type EnumerableFromInt = sig
  include Enumerable

  val from_int : int -> t option
  val to_int : t -> int
end

(** enum_from_int n : renvoie un module de type EnumerableInt représentant les entiers de 0 à n *)
let enum_from_int (n : int) =
  (module struct
    type t = int

    let values =
      if n < 0 then Seq.empty
      else from_succ (fun x -> if x >= n then None else Some (x + 1)) 0

    let mini = if n >= 0 then Some 0 else None
    let maxi = if n >= 0 then Some n else None
    let safe_succ x = if x = n then None else Some (x + 1)
    let from_int x = if x <= n then Some x else None
    let to_int x = x
  end : EnumerableFromInt)

(** enum_between_int n1 n2 : renvoie un module de type EnumerableInt représentant les entiers entre n1 et n2 *)
let enum_between_int (_ : int) (_ : int) =
  (module struct
    type t = int

    let values = failwith "à faire"
    let mini = failwith "à faire"
    let maxi = failwith "à faire"
    let safe_succ _ = failwith "à faire"
    let from_int _ = failwith "à faire"
    let to_int _ = failwith "à faire"
  end : EnumerableFromInt)
