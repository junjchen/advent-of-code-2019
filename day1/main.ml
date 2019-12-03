let stream_fold f stream init =
    let result = ref init in
    Stream.iter (fun x -> result := f x !result) stream;
    !result
;;

let line_stream_of_channel channel =
    Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None);;


let main () =
    (* Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2. *)
    let lines = line_stream_of_channel stdin in
    let cal_fuel mass = int_of_float (floor (mass /. 3.0)) - 2 in
    let cumulate_fuel mass total_fuel = total_fuel + cal_fuel (float_of_string mass) in
    Printf.printf "%d" (stream_fold cumulate_fuel lines 0)
;;

main()
