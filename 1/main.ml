open! Core

let rec read_all () =
  match In_channel.input_line In_channel.stdin with
  | None -> ""
  | Some v -> v :: [ read_all () ] |> String.concat ~sep:"\n"
;;

let line_to_two_numbers s =
  let s = String.to_list s in
  match s with
  | [] -> None
  | s ->
    let take_num s =
      let take_num_aux =
        List.fold ~init:(false, [], []) ~f:(fun (halted, a, b) c ->
          match halted with
          | true -> halted, a, c :: b
          | false -> if Char.is_digit c then false, c :: a, b else true, a, b)
      in
      let _, a, b = take_num_aux s in
      a, b
    in
    let rec drop_spaces s =
      match s with
      | [] -> []
      | ' ' :: s -> drop_spaces s
      | _ :: _ -> s
    in
    let n1, s = take_num s in
    let s = drop_spaces s in
    let n2, _ = take_num s in
    (List.rev n1, n2)
    |> Tuple2.map ~f:String.of_char_list
    |> Tuple2.map ~f:Int.of_string
    |> Option.return
;;

type t = (int * int) list [@@deriving sexp_of]

let part1 () =
  let s = read_all () in
  let ns = String.split ~on:'\n' s |> List.filter_map ~f:line_to_two_numbers in
  let n1s, n2s = List.unzip ns in
  let n1s, n2s = Tuple2.map ~f:(List.sort ~compare:Int.compare) (n1s, n2s) in
  let rs = List.map2_exn n1s n2s ~f:(fun a b -> Int.abs (a - b)) in
  let result = List.sum (module Int) rs ~f:Fn.id in
  print_endline (Int.to_string result)
;;

let part2 () =
  let s = read_all () in
  let ns = String.split ~on:'\n' s |> List.filter_map ~f:line_to_two_numbers in
  let n1s, n2s = List.unzip ns in
  let n2s_count =
    List.fold n2s ~init:Int.Map.empty ~f:(fun count n2 ->
      Map.change count n2 ~f:(function
        | None -> Some 1
        | Some v -> Some (v + 1)))
  in
  let total_score =
    List.fold n1s ~init:0 ~f:(fun sum n1 ->
      let similarity_score =
        match Map.find n2s_count n1 with
        | None -> 0
        | Some n2 -> n1 * n2
      in
      sum + similarity_score)
  in
  print_endline (Int.to_string total_score)
;;

let cmd () =
  let basic_command f =
    Command.basic
      ~summary:""
      (let%map_open.Command () = return () in
       f)
  in
  Command.group ~summary:"" [ "part1", basic_command part1; "part2", basic_command part2 ]
;;

let () = Command_unix.run (cmd ())
