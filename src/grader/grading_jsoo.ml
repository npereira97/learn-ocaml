(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2019 OCaml Software Foundation.
 * Copyright (C) 2016-2018 OCamlPro.
 *
 * Learn-OCaml is distributed under the terms of the MIT license. See the
 * included LICENSE file for details. *)

exception Timeout

open Grader_jsoo_messages
open Lwt.Infix
(*

let raml_analysis_thread solution = 
	let open Learnocaml_report in
	let worker_file = "/js/raml_worker.js" in 
	let t,u = Lwt.task () in 
	let worker = Worker.create worker_file in 
	let _ = Lwt.on_cancel t (fun () -> worker##terminate) in 
	let onmessage ev = 
		begin
			let str :string = ev##.data in 
			let raml_report : Learnocaml_report.t = 
					match (Resource_analysis.report_of_string str) with
					| Some report -> report 
					| None -> [Message ([Text "A resource bound could not be derived"],Informative)] in 
			let _ = worker##terminate in 
			let _ = Lwt.wakeup u raml_report in 
			Js._true 
		end in
	let _ = worker##.onmessage := Dom.handler onmessage in 
	let _ = worker##(postMessage "") in
	t;;
*)

	

let get_grade
    ?(worker_js_file = "/js/learnocaml-grader-worker.js")
    ?(callback = (fun _ -> ()))
    ?(timeout = infinity)
    exercise =
  let t, u = Lwt.task () in
  let worker = Worker.create worker_js_file in
  Lwt.on_cancel t (fun () -> worker##terminate) ;
  let onmessage (ev : Json_repr_browser.Repr.value Worker.messageEvent Js.t) =
    let json = ev##.data in
    begin match Json_repr_browser.Json_encoding.destruct from_worker_enc json with
      | Callback text -> callback text
      | Answer (report, stdout, stderr, outcomes) ->
          worker##terminate ;
          Lwt.wakeup u (report, stdout, stderr, outcomes)
    end ;
    Js._true
  in
  worker##.onmessage := Dom.handler onmessage ;
  Lwt.return @@
  fun solution ->(*
    let raml_thread = raml_analysis_thread solution in 
    let combined_result_thread = raml_thread >>= (fun raml -> 
				t >>= (fun (report,stdout,stderr,outcomes) -> 
				Lwt.return (raml@report,stdout,stderr))) in

*)

    let req = { exercise ; solution } in
    let json = Json_repr_browser.Json_encoding.construct to_worker_enc req in
    worker##(postMessage json) ;
    let timer =
      Lwt_js.sleep timeout >>= fun () ->
      worker##terminate ;
      Lwt.fail Timeout in
    Lwt.pick [ timer ; t ] 






