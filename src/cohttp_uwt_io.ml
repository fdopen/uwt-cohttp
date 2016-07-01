(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

module CD = Cohttp_uwt_debug
let () = match Sys.os_type with
  | "Win32" -> ()
  | _ -> Sys.(set_signal sigpipe Signal_ignore)

type 'a t = 'a Lwt.t
open Lwt.Infix
let (>>=) = Lwt.bind
let return = Lwt.return

type ic = Uwt_io.input_channel
type oc = Uwt_io.output_channel
type conn = Conduit_uwt.flow

let read_line ic =
  if !CD.debug_active then
    Uwt_io.read_line_opt ic >>= function
    | None -> CD.debug_print "<<< EOF\n"; Lwt.return_none
    | Some l as x -> CD.debug_print "<<< %s\n" l; Lwt.return x
  else
    Uwt_io.read_line_opt ic

let read ic count =
  let count = min count Sys.max_string_length in
  if !CD.debug_active then
    Uwt_io.read ~count ic >|= fun buf ->
    CD.debug_print "<<<[%d] %s" count buf;
    buf
  else
    Uwt_io.read ~count ic

let write oc buf =
  if !CD.debug_active then
    (CD.debug_print ">>> %s" buf; Uwt_io.write oc buf)
  else
    Uwt_io.write oc buf

let flush oc =
  Uwt_io.flush oc
