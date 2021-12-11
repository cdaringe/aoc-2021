module Point = struct
  type t = int * int [@@deriving show, eq, ord]

  let make x y : t = (x, y)
end

module Line = struct
  type t = Point.t * Point.t

  let interpolate_points ?(filter_non_rectilinear = false) (end_points : t list)
      : Point.t list =
    let unit_dir a b = if a > b then -1 else 1 in
    let interpolate (p1, p2) =
      let (x1, y1), (x2, y2) = (p1, p2) in
      let is_rectilinear, x'', y'' =
        if x1 = x2 then (true, 0, unit_dir y1 y2)
        else if y1 = y2 then (true, unit_dir x1 x2, 0)
        else if filter_non_rectilinear then (false, 0, 0)
        else (false, unit_dir x1 x2, unit_dir y1 y2)
      in
      let rec fill points =
        let ((x', y') as hd) = List.hd points in
        if hd = (x2, y2) then points
        else fill @@ ((x' + x'', y' + y'') :: points)
      in
      if filter_non_rectilinear then
        if is_rectilinear then fill [ (x1, y1) ] else []
      else fill [ (x1, y1) ]
    in
    List.(flatten @@ map interpolate end_points)
end

module Terrain = struct
  module Vents = Map.Make (Point)

  let plot points =
    let count_vent vents point =
      Vents.update point
        (function None -> Some 1 | Some x -> Some (x + 1))
        vents
    in
    List.fold_left count_vent Vents.empty points

  let count_if fn t =
    let f k v acc = if fn k v then acc + 1 else acc in
    Vents.fold f t 0
end

module Solver = struct
  let parse lines =
    let parse_line (s : string) =
      let f x1 y1 x2 y2 = Point.(make x1 y1, make x2 y2) in
      Scanf.sscanf s "%d,%d -> %d,%d" f
    in
    List.map parse_line lines

  let solve filename =
    Util.InputParse.parse_lines filename parse
    |> Line.interpolate_points ~filter_non_rectilinear:true
    |> Terrain.plot
    |> Terrain.count_if (fun _ num_vents -> num_vents > 1)
    |> print_int

  let solve2 filename =
    Util.InputParse.parse_lines filename parse
    |> Line.interpolate_points |> Terrain.plot
    |> Terrain.count_if (fun _ num_vents -> num_vents > 1)
    |> print_int
end
