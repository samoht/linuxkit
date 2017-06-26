open Lwt.Infix

let src = Logs.Src.create "sdk/mirage"
module Log = (val Logs.src_log src : Logs.LOG)

module Time = struct
  type +'a io = 'a Lwt.t
  let sleep_ns x = Lwt_unix.sleep (Int64.to_float x /. 1_000_000_000.)
end

module Flow = struct

  module FIFO = struct

    include Mirage_flow_unix.Fd

    let mkfifo path =
      if not (Sys.file_exists path) then
        Lwt.catch (fun () ->
            Lwt_unix.mkfifo path 0o644
          ) (function
            | Unix.Unix_error(Unix.EEXIST, _, _) -> Lwt.return_unit
            | e -> Lwt.fail e)
      else
        Lwt.return_unit

    let connect path =
      Log.debug (fun l -> l "opening FIFO: %s\n%!" path);
      mkfifo path >>= fun () ->
      Lwt_unix.openfile path [Lwt_unix.O_RDWR] 0o644

  end

  module Socket = struct

    include Mirage_flow_unix.Fd

    let connect path =
      let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
      Lwt_unix.connect fd (Lwt_unix.ADDR_UNIX path) >|= fun () ->
      fd

  end

  module Rawlink = struct

    include Mirage_flow_rawlink

    let connect ~filter ethif =
      Log.debug (fun l -> l "bringing up %s" ethif);
      (try Tuntap.set_up_and_running ethif
       with e -> Log.err (fun l -> l "rawlink: %a" Fmt.exn e));
      Lwt_rawlink.open_link ~filter ethif
      |> Lwt.return

  end

end

module Net = struct

  module FIFO = struct

    module Net = Mirage_net_flow.Make(Flow.FIFO)

    include Net

    let connect ?mac path =
      Flow.FIFO.connect path >>= fun net ->
      Net.connect ?mac net

  end

end
