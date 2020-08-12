(** Main entry point of the library *)

type app_state = {
  colors: Ui.Colors.t; (** The set of colors to use *)
  font: string;
  prompt: string; (** The prompt to the user *)
  topbar: bool;  (** Shall dmlenu sit on the bottom or on the top of the screen? *)
  hook: (app_state -> app_state); (** Hook called whenever a token is added *)
  state: State.t; (** The state of the current engine *)
  ui_state: Ui.state; (** Data related to the drawing backend *)
}
(** The current state of the application *)

val run_list : 
  ?topbar: bool
  -> ?separator: string
  -> ?border: int
  -> ?colors: Ui.Colors.t
  -> ?font: string
  -> ?layout:State.layout
  -> ?prompt: string
  -> ?hook : (app_state -> app_state)
  -> Engine.t
  -> string list
(** Run a program and outputs the read tokens. You can override the
    default values for the parameter using optional parameters. The
    default values are
    - topbar: [true]
    - separator: [" "]
    - colors: [Ui.Colors.default]
    - font: ["DejaVu Sans Mono 9"]
    - layout: [State.SingleLine]
    - prompt: [""]
    - border: [0]
    - hook: the identity function
*)

val run : 
  ?topbar: bool
  -> ?separator: string
  -> ?border: int
  -> ?colors: Ui.Colors.t
  -> ?font: string
  -> ?layout:State.layout
  -> ?prompt: string
  -> ?hook : (app_state -> app_state)
  -> Engine.t
  -> string option
(* Same as {!run_list} but concatenates the tokens using the separator.
   If no token were read, returns [None] *)
