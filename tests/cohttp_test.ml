open Cohttp

module type S = sig
  type 'a io
  type body

  type spec = Request.t -> body -> (Response.t * body) io
  type async_test = unit -> unit io

  val const : (Response.t * body) io -> spec
  val response_sequence : spec list -> spec
  val temp_server : ?port:int -> spec -> (Uri.t -> 'a io) -> 'a io
  val test_server_s : ?port:int -> ?name:string -> spec
    -> (Uri.t -> (string * async_test) list) -> OUnit.test io
  val run_async_tests : OUnit.test io -> OUnit.test_results io
end

let rs = Random.State.make_self_init ()
let ports_used = Hashtbl.create 10

let rec next_port () =
  let n = Random.State.int rs (24576 - 4096) + 4096 in
  if Hashtbl.mem ports_used n then
    next_port ()
  else
    let () = Hashtbl.replace ports_used n () in
    n

let response_sequence fail responses =
  let xs = ref responses in
  fun req body ->
    match !xs with
    | x::xs' ->
      xs := xs';
      x req body
    | [] -> fail "response_sequence: Server exhausted responses"

let const resp _ _ = resp
