open Astring
open Lwt.Infix

let random_string n =
  Bytes.init n (fun _ -> char_of_int (Random.int 255))

(* workaround https://github.com/mirage/alcotest/issues/88 *)
exception Check_error of string

let check_raises msg f =
  Lwt.catch (fun () ->
      f () >>= fun () ->
      Lwt.fail (Check_error msg)
    ) (function
      | Check_error e -> Alcotest.fail e
      | _             -> Lwt.return_unit
    )

let escape = String.Ascii.escape

module IO = Sdk.Mirage.Flow.FIFO

let write fd strs =
  Lwt_list.iter_s (fun str ->
      IO.write fd (Cstruct.of_string str) >>= function
      | Ok ()   -> Lwt.return_unit
      | Error e -> Fmt.kstrf Lwt.fail_with "write: %a" IO.pp_write_error e
    ) strs

let read fd =
  IO.read fd >>= function
  | Ok (`Data x) -> Lwt.return (Cstruct.to_string x)
  | Ok `Eof      -> Lwt.fail_with "read: EOF"
  | Error e      -> Fmt.kstrf Lwt.fail_with "read: %a" IO.pp_error e

let test_fifo path () =
  IO.connect path >>= fun c ->
  IO.connect path >>= fun s ->
  let test strs =
    let escape_strs = String.concat ~sep:"" @@ List.map escape strs in
    (* socket pairs are bi-directional *)
    (* c -> s works *)
    write c strs >>= fun () ->
    read s       >>= fun buf ->
    Alcotest.(check string) (path ^ " c -> s") escape_strs (escape buf);
    (* s -> c works *)
    write s strs >>= fun () ->
    read c       >>= fun buf ->
    Alcotest.(check string) (path ^ " s -> c") escape_strs (escape buf);
    Lwt.return_unit
  in
  test [random_string 1] >>= fun () ->
  test [random_string 1; random_string 1; random_string 10] >>= fun () ->
  test [random_string 100] >>= fun () ->
  (* note: if size(writes) > 8192 then the next writes will block (as
     we are using SOCK_STREAM *)
  let n = 8182 / 4 in
  test [
    random_string n;
    random_string n;
    random_string n;
    random_string n;
  ] >>= fun () ->

  Lwt.return_unit

let failf fmt = Fmt.kstrf Alcotest.fail fmt

(* read ops *)

module Client = Sdk.Conf.Client(IO)
module Server = Sdk.Conf.Server

let pp_path = Fmt.(Dump.list string)

let read_should_err t k =
  Lwt.catch (fun () ->
      Client.find t k >|= function
      | None   -> failf "read(%a) -> got: none, expected: err" pp_path k
      | Some v -> failf "read(%a) -> got: found:%S, expected: err" pp_path k v
    ) (fun _ -> Lwt.return ())

let read_should_none t k =
  Lwt.catch (fun () ->
      Client.find t k >|= function
      | None   -> ()
      | Some v -> failf "read(%a) -> got: found:%S, expected none" pp_path k v
    ) (fun e ->
      failf "read(%a) -> got: error:%a, expected none" pp_path k Fmt.exn e)

let read_should_work t k v =
  Lwt.catch (fun () ->
      Client.find t k >|= function
      | None    -> failf "read(%a) -> got: none, expected ok" pp_path k
      | Some v' ->
        if v <> v' then
          failf "read(%a) -> got: ok:%S, expected: ok:%S" pp_path k v' v
    ) (fun e ->
      failf "read(%a) -> got: error:%a, expected ok" pp_path k Fmt.exn e)

(* write ops *)

let write_should_err t k v =
  Lwt.catch
    (fun () -> Client.set t k v >|= fun () -> failf "write(%a) -> ok" pp_path k)
    (fun _  -> Lwt.return ())

let write_should_work t k v =
  Lwt.catch
    (fun () -> Client.set t k v)
    (fun e  -> failf "write(%a) -> error: %a" pp_path k Fmt.exn e)

(* del ops *)

let delete_should_err t k =
  Lwt.catch
    (fun () -> Client.delete t k >|= fun () -> failf "del(%a) -> ok" pp_path k)
    (fun _  -> Lwt.return ())

let delete_should_work t k =
  Lwt.catch
    (fun () -> Client.delete t k)
    (fun e  -> failf "write(%a) -> error: %a" pp_path k Fmt.exn e)

let test_ctl path () =
  Lwt_switch.with_switch @@ fun switch ->
  IO.connect path >>= fun s ->
  IO.connect path >>= fun c ->
  let k1 = ["foo"; "bar"] in
  let k2 = ["a"] in
  let k3 = ["b"; "c"] in
  let k4 = ["xxxxxx"] in
  let all = [`Read; `Write; `Delete] in
  let routes = [k1,all; k2,all; k3,all ] in
  Server.KV.v () >>= fun kv ->
  let _server =
    let service = Server.service ~switch ~routes kv in
    Server.listen ~switch service (module IO) s
  in
  Client.connect ~switch c >>= fun t ->
  let allowed k v =
    delete_should_work t k  >>= fun () ->
    read_should_none t k    >>= fun () ->
    write_should_work t k v >>= fun () ->
    read_should_work t k v  >>= fun () ->
    Server.KV.get kv k      >|= fun v' ->
    Alcotest.(check string) "in the db" v v'
  in
  let disallowed k v =
    read_should_err t k    >>= fun () ->
    write_should_err t k v >>= fun () ->
    delete_should_err t k
  in
  allowed k1 ""                           >>= fun () ->
  allowed k2 "xxx"                        >>= fun () ->
  allowed k3 (random_string (255 * 1024)) >>= fun () ->
  disallowed k4 "" >>= fun () ->
  Lwt.return_unit

let run f () =
  try Lwt_main.run (f ())
  with e ->
    Fmt.epr "ERROR: %a" Fmt.exn e;
    raise e

let test_stderr () = ()

let fifo = "/tmp/sdk-fifo"

let test = [
  "FIFO flows", `Quick, run (test_fifo fifo);
  "conf"      , `Quick, run (test_ctl fifo);
]

let reporter ?(prefix="") () =
  let pad n x =
    if String.length x > n then x
    else x ^ String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ())

let () = Alcotest.run "sdk" [
    "init", test;
  ]
