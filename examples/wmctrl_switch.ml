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
  let () = Matching.(set_match_query_fun (subset ~case:false)) in
  let () = Ordering.(set_reorder_matched_fun prefixes_first) in
  let colors = Ui.Colors.({
    focus_foreground = Draw.Color.of_string_exn "#eaeae8";
    focus_background = Draw.Color.of_string_exn "#443936";
    normal_foreground = Draw.Color.of_string_exn "#eaeae8";
    normal_background = Draw.Color.of_string_exn "#3a201a";
    match_foreground = Draw.Color.of_string_exn "#ee9711";
    window_background = Draw.Color.of_string_exn "#3a201a";
  }) in
  let compl = Engine.singleton Wmctrl.windows in
  match App.(run ~prompt
               ~font:"Source Code Pro 17"
               ~topbar:true
               ~border:5
               ~layout:(State.MultiLine 25)
               ~colors:colors
               compl) with
  | None -> Caml.exit 0
  | Some ws -> ws

let () =
  let wn = get_windows "Switch to: " in
  Stdio.print_endline wn;
  Unix.execvp "wmctrl" [|"wmctrl" ; "-i" ; "-a" ; wn|]
