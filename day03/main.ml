open Core

let to_bits str =
    List.map ~f:(fun c -> if Char.equal c '0' then 0 else 1)
    @@ String.to_list str

let input =
    In_channel.read_lines "./in/03.dat"
    |> List.map ~f:to_bits

let bits_to_int =
    List.fold ~init:0 ~f:(fun c b -> Int.bit_or (Int.shift_left c 1) b)

let counts = input
    |> List.transpose_exn
    |> List.map ~f:(
        List.fold ~init:(0, 0) ~f:(
            fun (c0, c1) c ->
                if c = 0 
                then (c0 + 1, c1)
                else (c0, c1 + 1)
        )
    )

let count1 rate = counts
    |> List.map ~f:(fun (x, y) -> if rate(x, y) then 0 else 1)
    |> bits_to_int

let gamma   (x, y) = x > y
let epsilon (x, y) = x < y

let part1 = count1(gamma) * count1(epsilon)

let rec filter l n rating =
    let f xs n rating =
        let cs    = List.map ~f:(fun l -> List.nth_exn l n) xs in
        let zeros = List.count ~f:(Int.equal 0) cs in
        let rate  = rating zeros (List.length xs) in
        List.filter ~f:(fun l -> Int.equal rate (List.nth_exn l n)) xs
    in match l with
    | []    -> raise (Failure "?")
    | [ x ] -> x
    | _     -> filter (f l n rating) (n + 1) rating

let count2 rate =
    filter input 0 (fun c l -> if rate(c, l) then 0 else 1)
    |> bits_to_int

let o2  (c, l) = c >  l / 2
let co2 (c, l) = c <= l / 2

let part2 = count2(o2) * count2(co2)

let () =
    Printf.printf "Part 01: %d\n" part1;
    Printf.printf "Part 02: %d\n" part2;
