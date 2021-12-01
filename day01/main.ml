open Core

(** Reads the input and converts every line to an int. *)
let input =
    In_channel.read_lines "./in/01.dat"
    |> List.map ~f:Int.of_string

(** Int.is_positive is needed to skip the first entry. *)
let increased a b =
    (b, if Int.is_positive a && b > a then 1 else 0)

(** Simple sum of a list. *)
let sum = List.fold ~init:0 ~f:Int.( + )

let part1 i = i
    |> List.folding_map ~init:0 ~f:increased
    |> sum

(** Creates a n-measurement sliding window. *)
let rec sliding_window n = function
    | [] -> []
    | _ :: l_ as l ->
        if List.length l >= n then
            List.take l n :: sliding_window n l_
        else []

let part2 = input
    |> sliding_window 3
    |> List.map ~f:sum
    |> part1

let () =
    Printf.printf "Part 01: %d\n" (part1 input);
    Printf.printf "Part 02: %d\n" part2;
