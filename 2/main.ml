open! Core

let rec read_all () =
  match In_channel.input_line In_channel.stdin with
  | None -> ""
  | Some v -> v :: [ read_all () ] |> String.concat ~sep:"\n"
;;

let line_to_many_numbers s =
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
      List.rev a, List.rev b
    in
    let rec drop_spaces s =
      match s with
      | [] -> []
      | ' ' :: s -> drop_spaces s
      | _ :: _ -> s
    in
    let rec aux s =
      let n, s = take_num s in
      let s = drop_spaces s in
      match s with
      | [] -> [ n ]
      | _ :: _ -> n :: aux s
    in
    aux s |> Option.return
;;

type t = int list list [@@deriving sexp_of]

let part1 () =
  let s = read_all () in
  let ns = String.split ~on:'\n' s |> List.filter_map ~f:line_to_many_numbers in
  let ns =
    List.map ns ~f:(List.map ~f:(fun v -> v |> String.of_char_list |> Int.of_string))
  in
  let is_good ns =
    let is_increasing =
      match ns with
      | n1 :: n2 :: _ -> n2 > n1
      | _ -> raise_s [%message "idk what to do in this case"]
    in
    let rec aux ns =
      match ns with
      | [] -> true
      | n1 :: n2 :: ns ->
        let diff = abs (n1 - n2) in
        Bool.equal (n2 > n1) is_increasing && diff > 0 && diff < 4 && aux (n2 :: ns)
      | _ :: _ -> true
    in
    aux ns
  in
  let ngood = List.count ns ~f:is_good in
  print_endline (Int.to_string ngood)
;;

let subsets_of_minus_one_element xs =
  let rec aux flag = function
    | [] -> [ [] ]
    | x :: xs ->
      List.concat
        [ (if flag then aux false xs else [])
        ; aux flag xs |> List.map ~f:(fun vs -> x :: vs)
        ]
  in
  aux true xs
;;

let part2 () =
  let s = read_all () in
  let ns = String.split ~on:'\n' s |> List.filter_map ~f:line_to_many_numbers in
  let ns =
    List.map ns ~f:(List.map ~f:(fun v -> v |> String.of_char_list |> Int.of_string))
  in
  let is_good ns =
    let is_increasing =
      match ns with
      | n1 :: n2 :: _ -> n2 > n1
      | _ -> raise_s [%message "idk what to do in this case"]
    in
    let rec aux ns =
      match ns with
      | [] -> true
      | n1 :: n2 :: ns ->
        let diff = abs (n1 - n2) in
        Bool.equal (n2 > n1) is_increasing && diff > 0 && diff < 4 && aux (n2 :: ns)
      | _ :: _ -> true
    in
    let result = aux ns in
    result
  in
  let ngood =
    List.count ns ~f:(fun ns -> subsets_of_minus_one_element ns |> List.exists ~f:is_good)
  in
  print_endline (Int.to_string ngood)
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
