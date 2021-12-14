open Util

module Grid = struct
  open CCArray
  open CDOpt.Syntax

  type t = int array array

  let of_list list = List.map of_list list |> of_list

  let get x y t =
    let* row = get_safe t y in
    get_safe row x

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

  let print t =
    let row_len = length (Array.get t 0) in
    ignore
    @@ foldi
         (fun _ x _y v ->
           print_string @@ [%string "%{v#Int},"];
           if x == row_len - 1 then print_newline ();
           0)
         0 t
end

module Solver = struct
  open List

  let parse =
    map (String.to_seq >> of_seq >> map (fun c -> int_of_char c - 48))
    >> Grid.of_list

  let concat_on_low grid lows x y cell =
    match Grid.is_lowest_neighbor x y grid with
    | true -> cell :: lows
    | _ -> lows

  let solve_1 filename =
    Util.Input.parse_lines filename parse |> fun grid ->
    Grid.foldi (concat_on_low grid) [] grid
    |> map (( + ) 1)
    |> fold_left ( + ) 0 |> print_int
end
