module Solver = struct
  let p2_cost a b =
    let n = CCInt.abs (a - b) in
    n * (n + 1) / 2

  let parse lines =
    List.hd lines |> String.split_on_char ',' |> List.map int_of_string
    |> List.sort Int.compare

  let solve_1 filename =
    Util.Input.parse_lines filename parse |> fun l ->
    (l, List.(nth l (length l / 2))) |> fun (l, target_position) ->
    List.fold_left
      (fun total_fuel pos ->
        total_fuel
        +
        if pos > target_position then pos - target_position
        else target_position - pos)
      0 l
    |> print_int

  let solve_2 filename =
    Util.Input.parse_lines filename parse |> fun crab_positions ->
    let open CCList in
    let min_target = fold_left CCInt.min CCInt.max_int crab_positions in
    let max_target = fold_left CCInt.max 0 crab_positions in
    let possible_targets = range min_target max_target in
    let cost_for_target target =
      let target_cost = p2_cost target in
      map target_cost crab_positions |> fold_left ( + ) 0
    in
    map cost_for_target possible_targets
    |> CCInt.(List.fold_left min max_int)
    |> print_int
end
