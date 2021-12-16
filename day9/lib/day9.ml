open Util
module IntMap = Map.Make (Int)

module Grid = struct
  open CCArray
  open CDOpt.Syntax

  type cell = (*basin*)
    | B of int | (*9*) N | (*unknown*) U of int

  type t = cell array array

  let u x = U x

  let of_list list = List.map of_list list |> of_list

  let get x y t =
    let* row = get_safe t y in
    get_safe row x

  let set x y v t =
    let row = Array.get t y in
    Array.set row x v

  let up x y = get x (y + 1)

  let down x y = get x (y - 1)

  let left x y = get (x - 1) y

  let right x y = get (x + 1) y

  let probes = [ up; down; left; right ]

  let is_opt_gt ~default than = function Some i -> i > than | _ -> default

  let is_lowest_neighbor x y t =
    let current = Option.get (get x y t) in
    let check_lower fn = fn x y t |> is_opt_gt ~default:true current in
    List.for_all check_lower probes

  let foldi fn init =
    foldi
      (fun acc y row -> foldi (fun acc' x cell -> fn acc' x y cell) acc row)
      init

  let iteri fn t =
    iteri (fun y row -> iteri (fun x cell -> fn (x + y) x y cell t) row) t

  let rec update_neighbors id x y (t : t) =
    List.iter
      (fun (dx, dy) ->
        let x' = x + dx in
        let y' = y + dy in
        match get x' y' t with
        | None -> ()
        | Some c -> (
            match c with
            | U v ->
                if v == 9 then set x' y' N t
                else (
                  set x' y' (B id) t;
                  update_neighbors id x' y' t)
            | B _ | N -> ()))
      [ (1, 0); (0, 1); (-1, 0); (0, -1) ]

  let get_basin_counts t =
    let fold_row s rw =
      fold
        (fun s' cell ->
          match cell with
          | U _ | N -> s'
          | B id ->
              IntMap.update id
                (function None -> Some 1 | Some i -> Some (i + 1))
                s')
        s rw
    in
    fold fold_row IntMap.empty t

  let basinify_cell ~id _ x y _cell t =
    match get x y t |> Option.get with
    | U v ->
        if v == 9 then set x y N t
        else (
          set x y (B !id) t;
          update_neighbors !id x y t;
          id := !id + 1)
    | _ -> ()

  let basinify t =
    let id = ref 0 in
    iteri (basinify_cell ~id) t
end

module Solver = struct
  open List

  let parse =
    map (String.to_seq >> of_seq >> map (fun c -> Grid.u (int_of_char c - 48)))
    >> Grid.of_list

  let concat_on_low grid lows x y cell =
    match Grid.is_lowest_neighbor x y grid with
    | true -> cell :: lows
    | _ -> lows

  (* let solve_1 filename =
     Util.Input.parse_lines filename parse |> fun grid ->
     Grid.foldi (concat_on_low grid) [] grid
     |> map (( + ) 1)
     |> fold_left ( + ) 0 |> print_int *)

  let solve_2 filename =
    Util.Input.parse_lines filename parse |> fun grid ->
    Grid.basinify grid;
    grid |> Grid.get_basin_counts |> IntMap.to_seq |> Seq.map snd |> List.of_seq
    |> List.sort Int.compare |> List.rev |> CCList.take 3
    |> List.fold_left (fun acc v -> (if v = 0 then 1 else v) * acc) 1
    |> print_int
end
