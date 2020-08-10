(* Frontend for the "wmctrl" utility *)
open Base
open Stdio
open Dmlenu

module Wmctrl = struct
  let windows =
    let aux () =
      let ic = Unix.open_process_in "wmctrl -l" in
      let rec loop acc =
        try
          let line = In_channel.input_line_exn ic in
          let (id, rest) = String.lsplit2_exn ~on:' ' line in
          let rest = String.lstrip rest in
          let (_, rest) = String.lsplit2_exn ~on:' ' rest in
          let rest = String.lstrip rest in
          let (_, title) = String.lsplit2_exn ~on:' ' rest in
          loop ((title,id,"")::acc)
        with | End_of_file -> acc
             | Not_found_s(_) -> loop acc
      in
      let r = loop [] in
      ignore (Unix.close_process_in ic);
      r
    in
    Source.from_list_lazy (Lazy.from_fun aux)
end

let get_windows prompt =
  Matching.(set_match_query_fun @@ fuzzy_match ~case:false) ;
  let compl = Engine.singleton Wmctrl.windows in
  match App.(run ~prompt
               ~font:"Monoid 16"
               ~topbar:true
               ~layout:(State.MultiLine 20)
               ~colors:Ui.Colors.default
               compl) with
  | None -> Caml.exit 0
  | Some ws -> ws

let () =
  let wn = get_windows "Switch to: " in
  Stdio.print_endline wn;
  Unix.execvp "wmctrl" [|"wmctrl" ; "-i" ; "-a" ; wn|]
