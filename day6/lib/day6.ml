module Fishies = struct
  type t = int array [@@deriving show, eq, ord]

  let make ints : t =
    let t' = Array.make 8 0 in
    List.iter
      (fun v ->
        let idx = v - 1 in
        let last = Array.get t' idx in
        Array.set t' idx (last + 1))
      ints;
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
    List.map parse_line lines |> List.rev |> List.hd |> Array.of_list

  let solve filename =
    Util.InputParse.parse_lines filename parse |> fun x ->
    print_endline @@ Fishies.show  x
end
