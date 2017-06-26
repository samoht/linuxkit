(** [dhcp-network] proxy network packets between a raw interface
    (using BPF filters) and a unix domain socket. *)

open Lwt.Infix

let src = Logs.Src.create "network" ~doc:"DHCP client network proxy"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (Net: Mirage_flow_lwt.S) (Socket: Mirage_flow_lwt.S) = struct

  let start net socket =
    let net = Mirage_flow_lwt.create (module Net) net "net" in
    let socket = Mirage_flow_lwt.create (module Socket) socket "socket" in
    Mirage_flow_lwt.proxy ~verbose:true net socket

end

(************ LinuxKit specific devices *****************)

module Main = Make (Sdk.Mirage.Flow.Rawlink)(Sdk.Mirage.Flow.FIFO)
external dhcp_filter: unit -> string = "bpf_filter"


let run () ethif path =
  let filter = dhcp_filter () in
  Lwt_main.run (
    Sdk.Mirage.Flow.Rawlink.connect ~filter ethif >>= fun net ->
    Sdk.Mirage.Flow.FIFO.connect path >>= fun path ->
    Main.start net path
  )

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

let ethif =
  let doc =
    Arg.info ~docv:"NAME" ~doc:"The interface to listen too." ["ethif"]
  in
  Arg.(value & opt string "eth0" & doc)

let net =
  let doc =
    Arg.info ~docv:"PATH"
      ~doc:"The named pipe to forward the network traffic to." ["net"]
  in
  Arg.(value & opt string "/var/run/dhcp-client/net" & doc)

let run =
  Term.(const run $ setup_log $ ethif $ net),
  Term.info "dhcp-client" ~version:"%%VERSION%%"

let () = match Term.eval run with
  | `Error _ -> exit 1
  | `Ok () |`Help |`Version -> exit 0
