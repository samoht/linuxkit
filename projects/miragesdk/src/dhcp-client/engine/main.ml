open Astring
open Lwt.Infix

let src = Logs.Src.create "dhcp-client/engine"
module Log = (val Logs.src_log src : Logs.LOG)

let failf fmt = Fmt.kstrf Lwt.fail_with fmt

type t = {
  address: Ipaddr.V4.t;
  gateway: Ipaddr.V4.t option;
  domain: string option;
  search: string option;
  nameservers: Ipaddr.V4.t list;
}

(* FIXME: we (still) lose lots of info here *)
let of_lease (t: Dhcp_wire.pkt) =
  let gateway = match Dhcp_wire.collect_routers t.Dhcp_wire.options with
  | [] -> None
  | n::_ -> Some n
  in
  { address = t.Dhcp_wire.yiaddr;
    gateway;
    domain = Dhcp_wire.find_domain_name t.Dhcp_wire.options;
    search = Dhcp_wire.find_domain_search t.Dhcp_wire.options;
    nameservers = Dhcp_wire.collect_dns_servers t.Dhcp_wire.options }

let pp ppf t =
  Fmt.pf ppf "\n\
              address    : %a\n\
              domain     : %a\n\
              search     : %a\n\
              nameservers: %a\n"
    Ipaddr.V4.pp_hum t.address
    Fmt.(option ~none:(unit "--") string) t.domain
    Fmt.(option ~none:(unit "--") string) t.search
    Fmt.(list ~sep:(unit " ") Ipaddr.V4.pp_hum) t.nameservers

let parse_option_code str =
  match Dhcp_wire.string_to_option_code str with
  | Some x -> Ok x
  | None   -> Error (Fmt.strf "%s is not a valid DHCP option code" str)

let default_options =
  let open Dhcp_wire in
  [
    RAPID_COMMIT;
    DOMAIN_NAME;
    DOMAIN_SEARCH;
    HOSTNAME;
    CLASSLESS_STATIC_ROUTE;
    NTP_SERVERS;
    INTERFACE_MTU;
  ]

module Conf (F: Mirage_flow_lwt.S) = struct

  module Client = Sdk.Conf.Client(F)

  let connect = Client.connect

  let set_ip conf k ip =
    let str = Ipaddr.V4.to_string ip ^ "\n" in
    Client.set conf k str

  let set_ip_opt conf k = function
    | None    -> Lwt.return_unit
    | Some ip -> set_ip conf k ip

  let get_mac conf =
    Client.find conf ["mac"] >|= function
    | None   -> None
    | Some s -> Macaddr.of_string (String.trim s)

  let get_dhcp_code conf =
    Client.find conf ["dhcp-options"] >|= function
    | None   -> default_options
    | Some s ->
      let lines = String.cuts ~sep:"\n" s in
      List.fold_left (fun acc c -> match parse_option_code c with
          | Ok x    -> x :: acc
          | Error e ->
            Log.err (fun l -> l "error: %s" e);
            acc
        ) [] lines

end

module Make
    (Time: Mirage_time_lwt.S)
    (Net: Mirage_net_lwt.S)
    (Flow: Mirage_flow_lwt.S) =
struct

  module Dhcp_client = Dhcp_client_lwt.Make(Time)(Net)
  module Conf = Conf (Flow)

  let start () net conf =
    Lwt_switch.with_switch @@ fun switch ->
    Conf.Client.connect ~switch conf >>= fun conf ->
    Conf.get_dhcp_code conf >>= fun requests ->
    Dhcp_client.connect ~requests net >>= fun stream ->
    Lwt_stream.last_new stream >>= fun result ->
    let result = of_lease result in
    Log.info (fun l -> l "found lease: %a" pp result);
    Conf.set_ip conf ["ip"] result.address >>= fun () ->
    Conf.set_ip_opt conf ["gateway"] result.gateway

end

(************ LinuxKit specific devices *****************)

open Sdk.Mirage

module Main = Make(Time)(Net.FIFO)(Flow.FIFO)
module C = Conf(Flow.FIFO)

let run () net conf =
  Lwt_main.run (
    Flow.FIFO.connect conf >>= fun conf ->
    Lwt_switch.with_switch @@ fun switch ->
    C.connect ~switch conf >>= fun c ->
    (* FIXME: it's a bit weird to read the mac address here *)
    C.get_mac c >>= function
    | None     -> failf "No mac address!"
    | Some mac ->
      Net.FIFO.connect ~mac net >>= fun net ->
      Main.start () net conf
  )

open Cmdliner

let conf =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Named pipe used to communicate with the DHCP \
                                client configurator." ["network"]
  in
  Arg.(value & opt string "/var/run/dhcp-client/conf-enf" doc)

let net =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Named pipe used to manage network traffic"
      ["network"]
  in
  Arg.(value & opt string "/var/run/dhcp-client/net" doc)

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

let run =
  Term.(const run $ setup_log $ net $ conf),
  Term.info "dhcp-client-engine" ~version:"%%VERSION%%"

let () = match Term.eval run with
  | `Error _ -> exit 1
  | `Ok () |`Help |`Version -> exit 0
