open Util
module CharSet = Set.Make (Char)
module Signals = CharSet

module Digit = struct
  open List

  type t = { signals : Signals.t; solved_digit : int option }

  let intersect_signals a b = Signals.inter a.signals b.signals

  let is_signal_subset a b = Signals.subset a.signals b.signals

  let count_signals d = CharSet.cardinal d.signals

  let make signals =
    {
      signals =
        fold_left (fun sigset c -> Signals.add c sigset) Signals.empty signals;
      solved_digit = None;
    }

  let solve_digit n d = { d with solved_digit = Some n }

  let value d =
    match d.solved_digit with
    | None -> failwith "digit not solved"
    | Some x -> x

  let compare a b = Int.compare (value a) (value b)

  let to_tens_decimal tens t = CCInt.pow 10 tens * value t
end

module Solver = struct
  open List

  let assign_1_4_7_8 d =
    let open Digit in
    match count_signals d with
    | 2 -> solve_digit 1 d
    | 3 -> solve_digit 7 d
    | 4 -> solve_digit 4 d
    | 7 -> solve_digit 8 d
    | _ -> d

  let replace n digit =
    map (fun d -> if d = digit then Digit.solve_digit n d else d)

  let get_digit digits v =
    find
      (fun (d : Digit.t) ->
        CCOption.(
          is_some d.solved_digit && value ~default:(-1) d.solved_digit = v))
      digits

  let get_digits fn digits = filter fn digits

  let get_digits_with_n_signals ?(unsolved = true) n digits =
    let fn d =
      Digit.count_signals d = n
      && if unsolved then Option.is_none d.solved_digit else true
    in
    get_digits fn digits

  let assign_6 digits =
    let one = get_digit digits 1 in
    let six_sigs = get_digits_with_n_signals 6 digits in
    let six =
      find
        (fun d ->
          let sigs = Digit.intersect_signals d one in
          let card = Signals.cardinal sigs in
          card = 1)
        six_sigs
    in
    replace 6 six digits

  let assign_9 digits =
    let nines =
      get_digits_with_n_signals 6 digits
      |> filter (fun d' ->
             let open Digit in
             is_signal_subset (get_digit digits 4) d'
             && is_signal_subset (get_digit digits 7) d')
    in
    if length nines > 1 then failwith ">1 9 found";
    replace 9 (hd nines) digits

  let assign_3 digits =
    let one = get_digit digits 1 in
    let five_sigs = get_digits_with_n_signals 5 digits in
    let threes = filter (fun d' -> Digit.is_signal_subset one d') five_sigs in
    if length threes > 1 then failwith ">1 3 found";
    replace 3 (hd threes) digits

  let assign_5 digits =
    let six = get_digit digits 6 in
    let five_sigs = get_digits_with_n_signals 5 digits in
    let fives = filter (fun d' -> Digit.is_signal_subset d' six) five_sigs in
    if length fives > 1 then failwith ">1 5 found";
    replace 5 (hd fives) digits

  let assign_2 digits =
    let five_sigs = get_digits_with_n_signals 5 digits in
    if length five_sigs > 1 then failwith ">1 2 found";
    replace 2 (hd five_sigs) digits

  let assign_0 digits =
    let six_sigs = get_digits_with_n_signals 6 digits in
    if length six_sigs > 1 then failwith ">1 0 found";
    replace 0 (hd six_sigs) digits

  let assert_digits_assigned digits =
    iter
      (fun (d : Digit.t) ->
        if Option.is_none d.solved_digit then failwith "digits unsolved" else ())
      digits;
    digits

  let as_char_set = CCString.fold (fun s c -> CharSet.add c s) CharSet.empty

  let decode encoded solved_digits =
    let to_solved enc =
      let enc_set = as_char_set enc in
      let check_signals_match (d : Digit.t) = CharSet.equal enc_set d.signals in
      find check_signals_match solved_digits
    in
    map to_solved encoded

  let decode_line digits encoded =
    map assign_1_4_7_8 digits |> assign_6 |> assign_9 |> assign_5 |> assign_3
    |> assign_2 |> assign_0 |> assert_digits_assigned |> sort Digit.compare
    |> decode encoded

  let parse lines =
    let parse_line l =
      let on_scan d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 n1 n2 n3 n4 =
        ( [ d1; d2; d3; d4; d5; d6; d7; d8; d9; d10 ] |> map CCString.to_list,
          [ n1; n2; n3; n4 ] )
      in
      Scanf.sscanf l "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s" on_scan
    in
    map parse_line lines

  let to_solved_digits (encoded_digits, encoded_values) =
    let digits = map Digit.make encoded_digits in
    decode_line digits encoded_values

  let sum = fold_left ( + ) 0

  let count_occurrences filter_ints digits =
    let ints = map Digit.value digits in
    let contain_filter_int d = exists (Int.equal d) filter_ints in
    filter contain_filter_int ints |> length

  let digits_to_decimal = rev >> CCList.mapi Digit.to_tens_decimal >> sum

  let solve_1 filename =
    Util.Input.parse_lines filename parse
    |> map to_solved_digits
    |> map (count_occurrences [ 1; 4; 7; 8 ])
    |> fold_left ( + ) 0 |> print_int

  let solve_2 filename =
    Util.Input.parse_lines filename parse
    |> map to_solved_digits |> map digits_to_decimal |> fold_left ( + ) 0
    |> print_int
end
