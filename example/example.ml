module type S = sig
  type t

  val zero : t
  val succ : t -> t
end

module type Cnt = sig
  type t

  val _incr : unit -> t
end

module Cnt (V : S) : Cnt with type t = V.t = struct
  type t = V.t

  let p = ref V.zero

  let _incr () =
    p := V.succ !p;
    !p
  ;;

  let%test _ = V.succ V.zero > V.zero
end

module _ = Cnt (Int)

let%test_module _ = (module Cnt (Int))
let%test_module "description" = (module Cnt (Int))

include struct
  (* A pience from Base *)
  let groupi l ~break =
    (* We allocate shared position and list references so we can make the inner loop use
       [[@tail_mod_cons]], and still return back information about position and where in the
       list we left off. *)
    let pos = ref 0 in
    let l = ref l in
    (* As a result of using local references, our inner loop does not need arguments. *)
    let[@tail_mod_cons] rec take_group () =
      match !l with
      | ([] | [ _ ]) as group ->
        l := [];
        group
      | x :: (y :: _ as tl) ->
        pos := !pos + 1;
        l := tl;
        if break !pos x y then [ x ] else x :: take_group ()
    in
    (* Our outer loop does not need arguments, either. *)
    let[@tail_mod_cons] rec groups () =
      if [] = !l
      then []
      else (
        let group = take_group () in
        group :: groups ())
    in
    (groups () [@nontail])
  ;;

  let group l ~break = (groupi l ~break:(fun _ x y -> break x y) [@nontail])
end

let%test_module _ =
  (module struct
    open List

    let%test _ = List.map (fun _ -> assert false) [] = []
    let mis = [ 'M'; 'i'; 's'; 's'; 'i'; 's'; 's'; 'i'; 'p'; 'p'; 'i' ]

    let equal_letters =
      [ [ 'M' ]
      ; [ 'i' ]
      ; [ 's'; 's' ]
      ; [ 'i' ]
      ; [ 's'; 's' ]
      ; [ 'i' ]
      ; [ 'p'; 'p' ]
      ; [ 'i' ]
      ]
    ;;

    let single_letters = [ [ 'M'; 'i'; 's'; 's'; 'i'; 's'; 's'; 'i'; 'p'; 'p'; 'i' ] ]

    let every_three =
      [ [ 'M'; 'i'; 's' ]; [ 's'; 'i'; 's' ]; [ 's'; 'i'; 'p' ]; [ 'p'; 'i' ] ]
    ;;

    let%test _ = group ~break:( <> ) mis = equal_letters
    let%test _ = group ~break:(fun _ _ -> false) mis = single_letters
    let%test _ = groupi ~break:(fun i _ _ -> i mod 3 = 0) mis = every_three

    let%test "slow, but takes no cpu time" =
      Unix.sleepf 0.25;
      true
    ;;
  end)
;;
