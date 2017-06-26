open Lwt.Infix

let src = Logs.Src.create "dhcp-client/config"
module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Act: Mirage_flow_lwt.S)
    (Eng: Mirage_flow_lwt.S) =
struct

  open Sdk.Conf

  let start act eng =
    Lwt_switch.with_switch @@ fun switch ->
    let routes = [
      ["ip"]     , [`Write];
      ["mac"]    , [`Read ];
      ["gateway"], [`Write];
    ] in
    Server.KV.v () >>= fun db ->
    let service = Server.service ~switch ~routes db in
    let listen m f = Server.listen ~switch service m f in
    listen (module Act) act;
    listen (module Eng) eng;
    let t, _ = Lwt.task () in
    t

end

(************ LinuxKit specific devices *****************)

open Sdk.Mirage

module Main = Make(Flow.FIFO)(Flow.FIFO)

let run () act eng =
  Lwt_main.run (
    Flow.FIFO.connect act >>= fun act ->
    Flow.FIFO.connect eng >>= fun eng ->
    Main.start act eng
  )

(* CLI *)

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  let pp_header ppf x =
    Fmt.pf ppf "%5d: %a " (Unix.getpid ()) Logs_fmt.pp_header x
  in
  Logs.set_reporter (Logs_fmt.reporter ~pp_header ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())


let act =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Named pipe used to communicate with the DHCP \
                                client engine" ["network"]
  in
  Arg.(value & opt string "/var/run/dhcp-client/conf-eng" doc)

let eng =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Named pipe used for communicate with the DHCP \
                                actuator." ["network"]
  in
  Arg.(value & opt string "/var/run/dhcp-client/conf-act" doc)

let run =
  Term.(const run $ setup_log $ act $ eng),
  Term.info "dhcp-client-config" ~version:"0.0"

let () = match Term.eval run with
  | `Error _ -> exit 1
  | `Ok () |`Help |`Version -> exit 0
