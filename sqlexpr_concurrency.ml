
module type THREAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val fail : exn -> 'a t
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val finalize : (unit -> 'a t) -> (unit -> 'a t) -> 'a t
end

module Id =
struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
  let fail = raise

  let catch f g = try f () with e -> g e

  let finalize f g =
    match try `Ok (f ()) with e -> `Exn e with
        `Ok x -> g (); x
      | `Exn e -> g (); raise e
end
