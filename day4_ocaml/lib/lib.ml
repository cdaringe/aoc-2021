open Util

module Board = struct
  type t = (int * bool) array array [@@deriving show]

  let find_num (b : t) (target : int) =
    let open CCArray in
    let find_cell r =
      find_map_i (fun i (v, _) -> if v = target then Some i else None) r
    in
    find_map_i
      (fun y row ->
        let cell_idx = find_cell row in
        match cell_idx with Some x -> Some (x, y) | _ -> None)
      b

  let score board num =
    match find_num board num with
    | None -> ()
    | Some (col, row) ->
        let open CCArray in
        let row = get board row in
        ignore @@ set row col (num, true)

  let fold f ini (b : t) =
    let open Array in
    fold_left (fun acc r -> fold_left f acc r) ini b

  let sum_misses (b : t) =
    let f acc (v, hit) = acc + if hit then 0 else v in
    fold f 0 b

  let transpose b =
    let open CCArray in
    mapi
      (fun y row -> mapi (fun x _ -> get b x |> fun row' -> get row' y) row)
      b

  let check_win (b : t) =
    let open CCArray in
    let is_winning_arr = for_all snd in
    exists is_winning_arr b || exists is_winning_arr (transpose b)

  module Parser = struct
    open Util.InputParse

    let from_list (board_lists : int list list list) : t list =
      let to_score_row nums =
        Array.(of_list nums |> map (fun v -> (v, false)))
      in
      let to_score_board board_list =
        List.map to_score_row board_list |> Array.of_list
      in
      List.map to_score_board board_lists

    let chunk_boards_lines boards_lines =
      let open CCList in
      let rec inner lines =
        let board_lines, rest = take_drop 5 lines in
        let raw_board = map line_to_ints board_lines in
        raw_board :: (if has_length rest then inner rest else [])
      in
      inner boards_lines

    let of_lines lines : int list * t list =
      let num_str, boards_lines =
        match List.filter_map get_non_empty_str_opt lines with
        | nums :: boards_lines -> (nums, boards_lines)
        | _ -> failwith "invalid input"
      in
      ( num_str |> String.split_on_char ',' |> strings_to_ints,
        from_list @@ chunk_boards_lines boards_lines )
  end
end

module Solver = struct
  let solve filename =
    let open CCList in
    let lines = Util.InputParse.read_lines filename in
    let nums, boards = Board.Parser.of_lines lines in
    let winner_opt =
      find_mapi
        (fun i num ->
          print_endline [%string "trying num %{num#Int} @ index %{i#Int}"];
          let score_board b = Board.score b num in
          let with_last_num b = (b, num) in
          iter score_board boards;
          Option.map with_last_num @@ find_opt Board.check_win boards)
        nums
    in
    let board, last_num = CCOption.get_exn_or "no winner" winner_opt in
    let miss_sum = Board.sum_misses board in
    print_endline @@ string_of_int last_num;
    print_endline @@ string_of_int (miss_sum * last_num)
end
