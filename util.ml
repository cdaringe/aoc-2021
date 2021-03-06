let ( >> ) f g x = f x |> g

module CDOpt = struct
  module Syntax = struct
    let ( let* ) o f = match o with None -> None | Some x -> f x
  end
end

module Input = struct
  let split_whitespace = Str.(split (regexp "[ \t]+"))

  let strings_to_ints = CCList.(map int_of_string)

  let line_to_ints = split_whitespace >> strings_to_ints

  let read_lines filename =
    let f = open_in filename in
    let rec loop () =
      try
        let next = input_line f in
        next :: loop ()
      with End_of_file ->
        close_in f;
        []
    in
    loop ()

  let parse_lines filename parser = parser @@ read_lines filename
end

let has_length l = List.length l > 0

let get_non_empty_str_opt s = if String.length s > 0 then Some s else None
