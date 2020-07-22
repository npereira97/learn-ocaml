module Serialize = struct 
  (*
	open Lwt
	open Cohttp
	open Cohttp_lwt
	open Cohttp_lwt_unix
*)

  	open Core_kernel

	let (>>) f g = (fun x -> g (f x)) (* Function compostion left to right*)

	let to_string = Sexp.to_string;;
	let of_string = Sexp.of_string;;

	let log s =
		let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "a.txt" in
		output_string oc (s ^ "\n");
		close_out oc;
		s

	type ('a,'b) bridged_function = {
					sexp_of_a : ('a -> Ppx_sexp_conv_lib.Sexp.t) 
					;b_of_sexp : ( Ppx_sexp_conv_lib.Sexp.t -> 'b)
					;sexp_of_b : ('b -> Ppx_sexp_conv_lib.Sexp.t) 
					;a_of_sexp : ( Ppx_sexp_conv_lib.Sexp.t -> 'a)
					}

	type uri = string

	(*


	let ask : ('a,'b) bridged_function -> uri -> ('a -> 'b option) = 
	(fun f uri -> (fun x -> 
				try 
					(let arg = x |> f.sexp_of_a |> to_string in 
					Client.post ~body:(`String ( arg)) 
					(Uri.of_string uri) >>= fun (resp, body) ->
	  				body |> Cohttp_lwt.Body.to_string >|= (fun body ->
					( body) |> of_string |> f.b_of_sexp |> (fun x -> Some x))) |> Lwt_main.run

				with 
				| _ -> None ))

	let n = 5000


	(* let reply : ('a,'b) bridged_function -> ('a -> 'b) -> server = *)
	let reply =
		let server f =
			  let callback _conn req body =
			    body |> Cohttp_lwt.Body.to_string >|= (fun body ->
			      f body)
			    >>= (fun body -> Server.respond_string ~status:`OK ~body ())
			  in
			  Server.create ~mode:(`TCP (`Port n)) (Server.make ~callback ()) in 

	 	(fun bf f ->   let g s = try 
						s
						|> of_string 
						|> bf.a_of_sexp
						|> f
						|> bf.sexp_of_b
						|> to_string
					with
					| _ -> "(" (* Broken Sexp indicates failure*)   

				in
					server g)




*)

	let (>=>) : ('a -> 'b option) -> ('b -> 'c option) -> ('a -> 'c option) =
	(fun f g -> (fun x -> match f x with 
				| None -> None
				| Some x -> g x))


	let (>>=) : 'a option -> ('a -> 'b option) -> 'b option =
	(fun x f -> match x with 
						| None -> None
						| Some y -> f y)

	
	type 'a sexp_helper = {
								sexp_of_a : ('a -> Ppx_sexp_conv_lib.Sexp.t) 
								;a_of_sexp : ( Ppx_sexp_conv_lib.Sexp.t -> 'a)

							}
	
	let sig_gen : ('a sexp_helper) -> ('b sexp_helper) -> ('a,'b) bridged_function = 
	(fun a b -> {
					sexp_of_a = a.sexp_of_a
					;b_of_sexp = b.a_of_sexp
					;sexp_of_b = b.sexp_of_a
					;a_of_sexp = a.a_of_sexp;
				})

	(*Converters for all base types can be found in Sexplib.Std*)
    let unit_helper = {
        sexp_of_a = Sexplib.Std.sexp_of_unit;
		a_of_sexp = Sexplib.Std.unit_of_sexp
    }

	let int_helper = {
					sexp_of_a = Sexplib.Std.sexp_of_int;
					a_of_sexp = Sexplib.Std.int_of_sexp
	}

	let float_helper = {
					sexp_of_a = Sexplib.Std.sexp_of_float;
					a_of_sexp = Sexplib.Std.float_of_sexp
	}
	let string_helper = {
					sexp_of_a = Sexplib.Std.sexp_of_string;
					a_of_sexp = Sexplib.Std.string_of_sexp
	}

	let bool_helper = {
					sexp_of_a = Sexplib.Std.sexp_of_bool;
					a_of_sexp = Sexplib.Std.bool_of_sexp
	}

	let option_helper helper = {	
								sexp_of_a =  Sexplib.Std.sexp_of_option helper.sexp_of_a 
								; a_of_sexp = Sexplib.Std.option_of_sexp helper.a_of_sexp
								}

	
	let list_helper helper = {	
								sexp_of_a =  Sexplib.Std.sexp_of_list helper.sexp_of_a 
								; a_of_sexp = Sexplib.Std.list_of_sexp helper.a_of_sexp
								}





	let int_to_int = (sig_gen int_helper int_helper);;
	let int_to_string = (sig_gen int_helper string_helper);;
	let int_to_bool = (sig_gen int_helper bool_helper);;

	let int_to_int_list = (sig_gen int_helper (list_helper int_helper));;

	let int_list_to_int = (sig_gen (list_helper int_helper) int_helper);;

	let temp = (list_helper int_helper)

	
	let coerce = (fun (Some x) -> x)

	let join x = match x with
				| None -> None
				| Some x -> x



	let report_helper = {a_of_sexp = Learnocaml_report.t_of_sexp; sexp_of_a = Learnocaml_report.sexp_of_t}

	let report_of_string (s:string) : Learnocaml_report.t option = 
			try
				s |> 
				of_string |>
				report_helper.a_of_sexp |>
				(fun x -> Some x)
			with
			| _ -> None


	
	let string_id s = [s] |> (list_helper string_helper).sexp_of_a |> to_string

	




  end 


let report_of_string = Serialize.report_of_string
type 'a tree = Leaf | Node of 'a * ('a tree) * ('a tree) [@@deriving sexp]
open Serialize



let coerce = Serialize.coerce
let string_id = Serialize.string_id

let rec gen_tree n = if n = 0 then Leaf 
		else let child = (gen_tree (n-1)) in 
			Node (n,child,child)

let sample_tree = (gen_tree 10)

let show_tree t = t |> (sexp_of_tree int_helper.sexp_of_a) |> to_string






