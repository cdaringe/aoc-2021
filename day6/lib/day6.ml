module Fishies = struct
  type t = int array [@@deriving show, eq, ord]

  let tick (t : t) =
    [| t.(1); t.(2); t.(3); t.(4); t.(5); t.(6); t.(7) + t.(0); t.(8); t.(0) |]

  let count = Array.fold_left ( + ) 0

  let make ints : t =
    let t' = Array.make 9 0 in
    let inc_idx v =
      let last = Array.get t' v in
      Array.set t' v (last + 1)
    in
    List.iter inc_idx ints;
    t'
end

module Solver = struct
  let parse lines =
    let rec parse_line ?(ints = []) (s : string) =
      match s with
      | "" -> ints
      | _ -> (
          let f (i : int) rest = parse_line ~ints:(i :: ints) rest in
          try Scanf.sscanf s "%d,%s" f
          with End_of_file -> int_of_string s :: ints)
    in
    List.(map parse_line lines |> rev |> hd |> Fishies.make)

  let solve filename num_days =
    let days = CCList.range 1 num_days in
    let init_fish = Util.Input.parse_lines filename parse in
    let tick fish _ = Fishies.tick fish in
    List.fold_left tick init_fish days |> Fishies.count |> print_int
end
