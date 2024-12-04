open! Core
open! Async
open Deferred.Or_error.Let_syntax

let rec read_all () =
  match In_channel.input_line In_channel.stdin with
  | None -> ""
  | Some v -> v :: [ read_all () ] |> String.concat ~sep:"\n"
;;

let parse_restricted_int cs =
  let rec aux n cs =
    let%bind.Option _ =
      match n with
      | 1 | 2 | 3 -> Some ()
      | _ -> None
    in
    match cs with
    | c :: cs when Char.is_digit c ->
      (match aux (n - 1) cs with
       | Some (subcall, cs) -> Some (c :: subcall, cs)
       | None -> Some ([ c ], cs))
    | _ -> None
  in
  let%map.Option num, cs = aux 3 cs in
  let num = num |> String.of_char_list |> Int.of_string in
  num, cs
;;

let parse_valid_prefix cs =
  print_s [%message "testing" ([%here] : Source_code_position.t) (cs : char list)];
  match cs with
  | 'm' :: 'u' :: 'l' :: '(' :: cs ->
    print_s [%message "testing" ([%here] : Source_code_position.t) (cs : char list)];
    let%bind.Option num1, cs = parse_restricted_int cs in
    print_s [%message "testing" ([%here] : Source_code_position.t) (cs : char list)];
    (match cs with
     | ',' :: cs ->
       print_s [%message "testing" ([%here] : Source_code_position.t) (cs : char list)];
       let%bind.Option num2, cs = parse_restricted_int cs in
       (match cs with
        | ')' :: cs ->
          print_s [%message "testing" ([%here] : Source_code_position.t) (cs : char list)];
          Some (num1, num2)
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

let rec all_suffixes ~writer cs =
  print_s [%message "testing" ([%here] : Source_code_position.t)];
  let%bind () = Pipe.write writer cs |> Deferred.map ~f:Or_error.return in
  match cs with
  | [] -> return ()
  | _ :: cs -> all_suffixes ~writer cs
;;

let part1 () =
  let reader, writer = Pipe.create () in
  let writing_job =
    let%map () = read_all () |> String.to_list |> all_suffixes ~writer in
    Pipe.close writer
  in
  print_s [%message "testing" ([%here] : Core.Source_code_position.t)];
  let reading_job =
    let i = ref 0 in
    Deferred.repeat_until_finished 0 (fun n ->
      print_s [%message ([%here] : Source_code_position.t) (!i : int)];
      i := !i + 1;
      match%map.Deferred Pipe.read reader with
      | `Eof -> `Finished n
      | `Ok suffix ->
        (match parse_valid_prefix suffix with
         | None -> `Repeat n
         | Some (a, b) -> `Repeat (n + (a * b))))
  in
  let%map () = writing_job
  and ans = Deferred.map ~f:Or_error.return reading_job in
  print_endline (Int.to_string ans)
;;

let part2 () = return ()

let cmd () =
  let basic_command f =
    Command.async_or_error
      ~summary:""
      (let%map_open.Command () = return () in
       f)
  in
  Command.group ~summary:"" [ "part1", basic_command part1; "part2", basic_command part2 ]
;;

let () = Command_unix.run (cmd ())
