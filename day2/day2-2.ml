open Str

let rec run op_codes pos =
    let at p = List.nth op_codes p in
    let d_at p = at (at p) in
    let apply v = List.mapi (fun i x -> if i = (at (pos + 3)) then v else x) op_codes in
    let op_add () = apply ((d_at (pos + 1)) + (d_at (pos + 2))) in
    let op_mul () = apply ((d_at (pos + 1)) * (d_at (pos + 2))) in
    match at pos with
    | 1 -> run (op_add ()) (pos + 4)
    | 2 -> run (op_mul ()) (pos + 4)
    | 99 -> op_codes
    | _ -> []
;;

let main () =
    let expected_out = 19690720 in
    let pairs l = List.map (fun x -> List.map (fun y -> x, y) l) l |> List.flatten in
    let nums = Stream.npeek 100 (Stream.from (fun x -> Some x)) in
    let line = input_line stdin in
    let program n v = List.map int_of_string (String.split_on_char ',' line) |> List.mapi (fun i x -> if i = 1 then n else if i = 2 then v else x) in
    let (x, y) = List.find (fun (x, y) -> run (program x y) 0 |> List.hd = expected_out) (pairs nums) in
    Printf.printf "(%d, %d)\n" x y;
    Printf.printf "%d" (100 * x  + y)
;;

main()
