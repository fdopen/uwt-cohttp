open Lwt.Infix
open OUnit
open Cohttp
open Cohttp_uwt

type 'a io = 'a Lwt.t
type body = Cohttp_lwt.Body.t

type spec = Request.t -> body -> (Response.t * body) io

type async_test = unit -> unit Lwt.t

let const = Cohttp_test.const

let response_sequence = Cohttp_test.response_sequence Lwt.fail_with

let temp_server ?port spec callback =
  let port = match port with
    | None -> Cohttp_test.next_port ()
    | Some p -> p in
  let server = Server.make ~callback:(fun _ req body -> spec req body) () in
  let uri = Uri.of_string ("http://127.0.0.1:" ^ (string_of_int port)) in
  let server_failed, server_failed_wake = Lwt.task () in
  let server = Lwt.catch
                 (fun () -> Server.create ~mode:(`TCP (`Port port)) server)
                 (function
                   | Lwt.Canceled -> Lwt.return_unit
                   | x -> Lwt.wakeup_exn server_failed_wake x; Lwt.fail x)
  in
  Lwt.pick [ callback uri; server_failed ] >|= fun res ->
  Lwt.cancel server;
  res

let test_server_s ?port ?(name="Cohttp Server Test") spec f =
  temp_server ?port spec begin fun uri ->
    Logs_lwt.info (fun f ->
        f "Test %s running on %s" name (Uri.to_string uri)) |> Lwt.ignore_result;
    let tests = f uri in
    let results =
      tests
      |> Lwt_list.map_s (fun (name, test) ->
        Logs_lwt.info (fun f -> f "Running %s" name ) |> Lwt.ignore_result;
        let res = Lwt.try_bind test
                    (fun () -> Lwt.return `Ok)
                    (fun exn -> Lwt.return (`Exn exn)) in
        res >|= (fun res -> (name, res))) in
    results >|= (fun results ->
      let ounit_tests =
        results
        |> List.map (fun (name, res) ->
          name >:: fun () ->
            match res with
            | `Ok -> ()
            | `Exn x -> raise x) in
      name >::: ounit_tests)
  end

let run_async_tests test = test >|= OUnit.run_test_tt_main
