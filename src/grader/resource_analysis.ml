

module Serialize = struct 
  open Core_kernel;;

	let (>>) f g = (fun x -> g (f x)) (* Function compostion left to right*)


	let to_string = Sexp.to_string;;
	let of_string = Sexp.of_string;;


	let log s =
		let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 "a.txt" in
		output_string oc (s ^ "\n");
		close_out oc


	type ('a,'b) bridged_function = {
					sexp_of_a : ('a -> Ppx_sexp_conv_lib.Sexp.t) 
					;b_of_sexp : ( Ppx_sexp_conv_lib.Sexp.t -> 'b)
					;sexp_of_b : ('b -> Ppx_sexp_conv_lib.Sexp.t) 
					;a_of_sexp : ( Ppx_sexp_conv_lib.Sexp.t -> 'a)
					}

	type command = string
	type args = string

	let ask : ('a,'b) bridged_function -> command -> ('a -> 'b option) = 
	(fun f c ->    
					f.sexp_of_a  >>
					to_string >>
					(fun s -> "echo \'" ^ s ^ "\' |" ^  c ) >>
					(fun s -> try 
			
								Some ((Unix.open_process_in >> input_line >> of_string >> (f.b_of_sexp)) s)
							  with
							  |_ -> None))


	
	let reply : ('a,'b) bridged_function -> ('a -> 'b) -> unit =
	(fun bf f ->  
				() 
				|> read_line 
				|>
				(fun s ->
					log s;
					try 
						(of_string >>
						bf.a_of_sexp >>
						f >>
						bf.sexp_of_b >>
						to_string) s
					with
					|_ ->  "(")  (* Malformed Sexp indicates failure*) 
				|> (fun s ->  print_string s)
			)

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

	let f = ask int_to_int "./second"



	
	let coerce = (fun (Some x) -> x)

	let join x = match x with
				| None -> None
				| Some x -> x



	type 'a tree = Leaf | Node of 'a * ('a tree) * ('a tree) [@@deriving sexp]

	let tree_helper helper = {
								a_of_sexp = tree_of_sexp helper.a_of_sexp;
								sexp_of_a = sexp_of_tree helper.sexp_of_a
							}
	

  end 


open Serialize


let test =  ask (sig_gen unit_helper string_helper) "/home/neil/final-env-2/learn-ocaml/src/grader/raml-1.4.2/main"

let id = (fun x -> x ^ x)