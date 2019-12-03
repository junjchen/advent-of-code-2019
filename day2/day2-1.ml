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
    let line = input_line stdin in
    let program = List.map int_of_string (String.split_on_char ',' line) in
    let out = run program 0 in
    Printf.printf "%d\n" (List.nth out 0);
;;

main()
