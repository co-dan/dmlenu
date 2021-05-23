open Base
open Stdio
open Dmlenu

module type EXTRASOURCES = sig
  val union : Source.t -> Source.t -> Source.t
  (** Two sources at the same time, wooo *)
end

module ExtraSources : EXTRASOURCES = struct
  open Source
  let union (S s1) (S s2) =
    S { delay = s1.delay && s2.delay;
        default_state = (s1.default_state, s2.default_state) ;
        compute = fun (st1,st2) qry ->
                  let (st1',cand1) = s1.compute st1 qry in
                  let (st2',cand2) = s2.compute st2 qry in
                  ((st1',st2'), cand1 @ cand2)
      }
end

module MyExec = struct
  let binaries = Source.update_candidates (fun c -> {c with doc = "RUN"}) (Lazy.force Source.binaries)
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
          loop ((title,id,"SWITCH")::acc)
        with | End_of_file -> acc
             | Not_found_s(_) -> loop acc
      in
      let r = loop [] in
      ignore (Unix.close_process_in ic);
      r
    in
    Source.from_list_lazy (Lazy.from_fun aux)

 (* ExtraSources.union
  *              
  *              Wmctrl.windows *)
  (* let all = Lazy.force Source.binaries *)
end

let get_inp prompt =
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
  let compl = Engine.({ empty with sources = [  MyExec.binaries ; MyExec.windows ]}) in
  match App.(run ~prompt
               ~font:"Noto Sans Mono 20"
               ~topbar:true
               ~border:5
               ~layout:(State.MultiLine 25)
               ~colors:colors
               compl) with
  | None -> Caml.exit 0
  | Some ws -> ws

let () =
  let wn = get_inp "run:" in
  Stdio.print_endline wn
