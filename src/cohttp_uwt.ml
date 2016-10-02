(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)


module Request = struct
  include Cohttp.Request
  include (Make(Cohttp_uwt_io)
           : module type of Make(Cohttp_uwt_io) with type t := t)
end

module Response = struct
  include Cohttp.Response
  include (Make(Cohttp_uwt_io)
           : module type of Make(Cohttp_uwt_io) with type t := t)
end

module Client = struct
  include
    Cohttp_lwt.Make_client
      (Cohttp_uwt_io)(Cohttp_uwt_net)
  let custom_ctx = Cohttp_uwt_net.init
end

module Server_core = Cohttp_lwt.Make_server(Cohttp_uwt_io)
module Server = struct
  include Server_core
  open Lwt

  let blank_uri = Uri.of_string ""

  let resolve_file ~docroot ~uri =
    (* This normalises the Uri and strips out .. characters *)
    let frag = Uri.path (Uri.resolve "" blank_uri uri) in
    Filename.concat docroot frag

  let buffer_size = 16384

  exception Isnt_a_file
  let respond_file ?headers ~fname () =
    Lwt.catch (fun () ->
      (* Check this isnt a directory first *)
      (Uwt.Fs.stat fname >>= fun s ->
      if Uwt.Fs.(s.st_kind <> S_REG) then fail Isnt_a_file
      else return_unit) >>= fun () ->
      let buffer = Uwt_bytes.create buffer_size in
      Uwt_io.open_file ~buffer ~mode:Uwt_io.input fname >>= fun ic ->
      Uwt_io.length ic >>= fun len ->
      let encoding = Cohttp.Transfer.Fixed len in
      let stream = Lwt_stream.from (fun () ->
        Lwt.catch (fun () ->
          Uwt_io.read ~count:buffer_size ic >|= function
          | "" -> None
          | buf -> Some buf)
          (fun exn ->
             Uwt_log.ign_debug ~exn ("Error resolving file " ^ fname);
             return_none)
        )
      in
      Lwt_stream.on_terminate stream (fun () ->
          Lwt.ignore_result (Uwt_io.close ic));
      let body = Cohttp_lwt_body.of_stream stream in
      let mime_type = Magic_mime.lookup fname in
      let headers = Cohttp.Header.add_opt_unless_exists headers "content-type" mime_type in
      let res = Cohttp.Response.make ~status:`OK ~encoding ~headers () in
      return (res, body)
    ) (function
      | Unix.Unix_error(Unix.ENOENT,_,_) | Isnt_a_file ->
        respond_not_found ()
      | exn -> Lwt.fail exn )

  let create ?timeout ?stop ?(ctx=Cohttp_uwt_net.default_ctx) ?(mode=`TCP (`Port 8080)) spec =
    Conduit_uwt.serve ?timeout ?stop ~ctx:ctx.Cohttp_uwt_net.ctx ~mode
      (callback spec)
end
