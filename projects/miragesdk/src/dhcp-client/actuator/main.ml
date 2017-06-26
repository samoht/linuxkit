open Lwt.Infix
open Astring

let src = Logs.Src.create "dhcp-client/actuator"
module Log = (val Logs.src_log src : Logs.LOG)

let failf fmt = Fmt.kstrf Lwt.fail_with fmt

module Sys (F: Mirage_flow_lwt.S) = struct

  (* System handlers *)

  module Client = Sdk.Conf.Client(F)

  let with_ip str f =
    match Ipaddr.V4.of_string (String.trim str) with
    | Some ip ->
      Log.info (fun l -> l "SET IP to %a" Ipaddr.V4.pp_hum ip);
      f ip
    | None ->
      Log.err (fun l -> l "%s is not a valid IP" str);
      Lwt.return_unit

  let ip t =
    Client.find t ["ethif"] >>= function
    | None       -> failf "Missing /ethif"
    | Some ethif ->
      Client.watch t ["ip"]
        (fun ip -> with_ip ip (fun ip -> Sdk.Net.set_ip ethif ip))

  let gateway t =
    Client.watch t ["gateway"]
      (fun gw -> with_ip gw (fun gw -> Sdk.Net.set_gateway gw))

  let handlers = [ ip; gateway ]

  let watch db =
    Lwt_list.map_p (fun f -> f db) handlers >>= fun _ ->
    let t, _ = Lwt.task () in
    t

  let set_ethif t ethif =
    Client.set t ["ethif"] ethif

  let set_mac t =
    Client.find t ["ethif"] >>= function
    | None       -> failf "Missing /ethif"
    | Some ethif ->
      Sdk.Net.mac ethif >>= fun mac ->
      let mac = Macaddr.to_string mac ^ "\n" in
      Client.set t ["mac"] mac

end

module Make (F: Mirage_flow_lwt.S) = struct

  module S = Sys(F)

  let start flow =
    Lwt_switch.with_switch @@ fun switch ->
    S.Client.connect ~switch flow >>= fun conf ->
    S.set_ethif conf "eth0" >>= fun () ->
    S.set_mac conf >>= fun () ->
    S.watch conf

end

(************ LinuxKit specific devices *****************)

open Sdk.Mirage
module Main = Make(Flow.FIFO)

let run () path = Lwt_main.run (Flow.FIFO.connect path >>= Main.start)

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

let conf =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Named pipe to connect to the config store."
      ["path"]
  in
  Arg.(value & opt string "/var/run/dhcp-client/conf-act" & doc)

let run =
  Term.(const run $ setup_log $ conf),
  Term.info "dhcp-client-actuator" ~version:"0.0"

let () = match Term.eval run with
  | `Error _ -> exit 1
  | `Ok () |`Help |`Version -> exit 0
