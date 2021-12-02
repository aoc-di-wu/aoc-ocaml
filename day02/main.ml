open Core

type command =
    | Forward of int
    | Up      of int
    | Down    of int

let command_of_string str =
    let to_command direction n =
        match direction with
            | "forward" -> Forward n
            | "up"      -> Up      n
            | "down"    -> Down    n
            | _         -> raise   (Failure direction)
    in Scanf.sscanf str "%s %d" to_command

(** Reads the input and converts every line to an int. *)
let input =
    In_channel.read_lines "./in/02.dat"
    |> List.map ~f:command_of_string

(** Applies the given instructions to the position. *)
(*
| x: horizontal position
| d: depth
- Forward n: increases the horizontal position by n units.
- Down    n: increases the depth by n units.
- Up      n: decreases the depth by n units.
*)
let course (x, d) = function
  | Forward n -> (x + n, d    )
  | Up      n -> (x    , d - n)
  | Down    n -> (x    , d + n)

let part1 = let x, y = List.fold ~init:(0, 0) ~f:course input in x * y

(** Applies the given instructions to the position. *)
(*
| x: horizontal position
| d: depth
| a: aim
- Forward n does two things:
    - It increases your horizontal position by n units.
    - It increases your depth by your aim multiplied by n.
- Down    n: increases your aim by n units.
- Up      n: decreases your aim by n units.
*)
let aim (x, d, a) = function
  | Forward n -> (x + n, d + (a * n), a    )
  | Up      n -> (x    , d          , a - n)
  | Down    n -> (x    , d          , a + n)

let part2 = let x, y, _ = List.fold ~init:(0, 0, 0) ~f:aim input in x * y

let () =
    Printf.printf "Part 01: %d\n" part1;
    Printf.printf "Part 02: %d\n" part2;