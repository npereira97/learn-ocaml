
let sys_time = Sys.time;;


exception Timeout
(* I suspect that Core shadows the std sys module*)
module Sys_std = Sys

(* Note Does not work if we pause because of Unix.sleep 

Tries to run function for atmost t seconds, returns None if computation time exceeds t
*)

let limit_fun : int -> ('a -> 'b) -> ('a -> 'b option)=

(fun t f x ->
    try
        Sys_std.set_signal Sys_std.sigalrm (Sys_std.Signal_handle (fun _ -> raise Timeout));
        ignore (Unix.alarm t);
        let t = (f x) in
        ignore (Unix.alarm 0);
        Sys_std.set_signal Sys_std.sigalrm Sys_std.Signal_default;
	Some t
	
    with Timeout -> Sys_std.set_signal Sys_std.sigalrm Sys_std.Signal_default; None

)







open Core

open Format
open Toolbox
open Rconfig

let raml_version = "1.4.2"
let raml_release_date = "July 2018"
let raml_authors = ["Jan Hoffmann (Carnegie Mellon)"
		   ;"Shu-Chun Weng (Google)"
                   ;"Benjamin Lichtman (Carnegie Mellon)"
                   ;"Chan Ngo (Carnegie Mellon)"
                   ;"Ankush Das (Carnegie Mellon)"
                   ;"Yue Niu (Carnegie Mellon)"                     
		   ]
let raml_website = "http://www.raml.co"

let _ = Config.load_path := [""; !Rpath.ocaml_raml_runtime]

let print_welcome () =
  printf "\nResource Aware ML, Version %s, %s\n\n" raml_version raml_release_date

let sys_name = Sys.executable_name



  
let print_expression print_types form e =
  printf "%s:\n@." form
   ; Pprint.print_expression ~print_types e
   ; printf "\n@."


let print_module print_types form m =
  let fprint_raml_type f t = Pprint.fprint_raml_type f t
  in printf "%s:\n@." form
   ; List.iter m (fun (f, e) ->
         printf "===== %s : %a =====@."
           f fprint_raml_type e.Expressions.exp_type
       ; Pprint.print_expression ~print_types e
       ; printf "\n@.")


let tcheck_prog e env =
  printf "Typechecking expression ...\n"
  ; Typecheck.typecheck ~linear:false e env
  ; printf "  Typecheck successful.\n"
  ; ignore @@ Typecheck.typecheck_stack ~linear:false e env
  ; printf "  Stack-based typecheck successful.\n@."


let tcheck_module m env =
  printf "Typechecking module ...\n"
  ; let f (g,e) =
      Typecheck.typecheck ~linear:false e env
    in
    List.iter ~f m
  ; printf "  Typecheck successful.\n"
  ; let f (g,e) = 
      ignore @@ Typecheck.typecheck_stack ~linear:false e env
    in
    List.iter ~f m
  ; printf "  Stack-based typecheck successful.\n@."


let eval ?(cost_only=false) e =
    printf "Evaluating expression ...\n"
  ; let metrics = [Metric.m_eval; Metric.m_tick; Metric.m_heap] in
    let (result, costs) = Eval.evaluate e metrics in
    if cost_only then
      ()
    else begin
      printf "\n  Return value:\n    "
      ; match result with
        | Some loc_heap -> Pprint.print_value loc_heap
        | None -> printf "Exception (undefined)" 
    end;
    match costs with
    | [(eval1,_); (tick1,_); (heap1,_)] ->
      printf (
        "\n" ^^
	"  Evaluation steps: %.2f\n" ^^
        "  Ticks:            %.2f\n" ^^
        "  Heap space:       %.2f\n@."
      ) eval1 tick1 heap1 
    | _ -> Misc.fatal_error "This is dead code."

let print_data amode_name m_name degree time constr =
  printf (
    "\n" ^^
      "  Mode:          %s\n" ^^    
      "  Metric:        %s\n" ^^
      "  Degree:        %d\n" ^^
      "  Run time:      %.2f seconds\n" ^^
      "  #Constraints:  %d\n@."
  ) amode_name m_name degree time constr


let analyze_prog analysis_mode m_name metric deg1 deg2 collect_fun_types e env =
  let start_time = sys_time () in
  let amode_name =
    match analysis_mode with
    | Mupper -> "upper"
    | Mlower -> "lower"
    | Mconstant -> "constant"
  in
  let () = tcheck_prog e env in
  let e_normal = Shareletnormal.share_let_normal "#" e in
  let e_normal_stack = Typecheck.typecheck_stack ~linear:true e_normal env in
  let () = printf "Analyzing expression ...\n" in
  let rec analyze_exp deg deg_max =
    assert (deg <= deg_max);
    assert (deg >= 1);
    printf "%i" deg;
    let module Clp = (
      val (
        match analysis_mode with
        | Mlower -> (
            module Solver.Clp( Solver.Clp_std_maximize )
          )
        | Mupper
        | Mconstant -> (
            module Solver.Clp( Solver.Clp_std_options )
          )
      ) : Solver.SOLVER
    )
    in
    let module Amode =
    struct
      let mode = analysis_mode
    end
    in
    let module Analysis = Analysis.Make( Clp )(Amode) in
    match Analysis.analyze_expression e_normal_stack ~metric ~degree:deg ~collect_fun_types with
    | None ->
      let _ = if deg < deg_max then printf ", %!" else printf "\n%!" in
      if deg < deg_max then
	analyze_exp (deg+1) deg_max
      else
	begin
	  let _ = printf "\n  No bound could be derived. The linear program is infeasible.\n" in	   
	  let constr = Clp.get_num_constraints () in
	  let time = sys_time () -. start_time in
	  print_data amode_name m_name deg time constr
	end
    | Some (q,fun_type_list) ->
      let _ =
	match fun_type_list with
	| [] -> ()
	| _ ->
	  begin
	    let () = printf "\n\n  Function types:\n" in
	    let print_fun_types atype =
	      Pprint.print_anno_funtype ~indent:("  ") ~simple_name:true atype
	    in
	    let _ = List.iter fun_type_list print_fun_types in
	    printf "===="
	  end
      in
      let _ = printf "\n\n  Derived %s bound: %.2f\n" amode_name q in
      let constr = Clp.get_num_constraints () in
      let time = sys_time () -. start_time in
      print_data amode_name m_name deg time constr
  in
  printf "\n  Trying degree: ";
  analyze_exp deg1 deg2


(*
let analyze_module analysis_mode m_name metric deg1 deg2 collect_fun_types m env =
  let amode_name =
    match analysis_mode with
    | Mupper -> "upper"
    | Mlower -> "lower"
    | Mconstant -> "constant"
  in

  let analyze_fun f_name e =
    let start_time = sys_time () in
    let e_normal = Shareletnormal.share_let_normal "#" e in
    let e_normal_stack = Typecheck.typecheck_stack ~linear:true e_normal env in
    let () = printf "Analyzing function %s ...\n" f_name in
    let rec analyze_f deg deg_max =
      assert (deg <= deg_max);
      assert (deg >= 1);
      printf "%i" deg;
      let module Clp = (
        
        val (
          match analysis_mode with
          | Mlower -> (
              module Solver.Clp( Solver.Clp_std_maximize )
            )
          | Mupper
          | Mconstant -> (
              module Solver.Clp( Solver.Clp_std_options )
            )
        ) : Solver.SOLVER
      )
      in
      let module Amode =
      struct
        let mode = analysis_mode
      end
      in
      let module Analysis = Analysis.Make( Clp )(Amode) in
      (*Try to derive bound with deg, increase deg in attempt to derive bound so long as you are below deg_max *)
      match Analysis.analyze_function e_normal_stack ~metric ~degree:deg ~collect_fun_types with
      | None ->
            let _ = if deg < deg_max then printf ", %!" else printf "\n%!" in
            if deg < deg_max then
              analyze_f (deg+1) deg_max
            else
              begin
                let _ = printf "\n  A bound for %s could not be derived. The linear program is infeasible.\n" f_name in
                let constr = Clp.get_num_constraints () in
                let time = sys_time () -. start_time in
                printf "\n--";
                print_data amode_name m_name deg time constr;
                printf "====\n\n"
	            end
      | Some (atarg,atres,fun_type_list) ->
              begin
                
                printf "\n%!";
                let _ = Pprint.print_anno_funtype ~indent:("  ") (f_name, atarg, atres) in
                let constr = Clp.get_num_constraints () in
                let time = sys_time () -. start_time in
                printf "--";
                print_data amode_name m_name deg time constr;
                      let () =
                        if List.length fun_type_list = 0 then
                          printf "====\n\n" 
                        else
                    match fun_type_list with
                    | [] -> ()
                    | _ ->
                      begin
                        let () = printf "-- Function types:\n" in
                        let print_fun_types atype =
                          Pprint.print_anno_funtype ~indent:("  ") ~simple_name:true atype
                        in
                        let _ = List.iter fun_type_list print_fun_types in
                              printf "====\n\n"
                        end
                    in
                        ()
	            end
    in
    let _ = printf "\n  Trying degree: " in
    analyze_f deg1 deg2
  in

  let () = tcheck_module m env in
  
  (* FLAG 
    Actual code that runs the analysis of an individual function, limit execution time to 3 seconds
  *)
  let f  =
    limit_fun 3
    (fun (f_name,e) ->
      match e.Expressions.exp_type with
      | Rtypes.Tarrow _ -> Some (analyze_fun f_name e)
      | _ -> None
    )
  in
    let join x = match x with 
                | Some s -> s
                | None -> None
  in 
    let rec process lst = match lst with 
                          | [] -> []
                          | None :: xs -> process xs 
                          | (Some t) :: xs -> t :: (process xs)
  in

    process (List.map m (fun x -> print_string "checking \n" ; join (f x)))



*)


let rec open_implicit_module m env =
  try
    Env.open_pers_signature m env
  with Not_found ->
  try 
      gen_pervasives (String.lowercase m ^ ".mli");
      Env.open_pers_signature m env
  with Not_found -> 
    Misc.fatal_error (Printf.sprintf "cannot open implicit module %S" m)

and initial_env (() : unit) : Env.t =
  Ident.reinit();
  let env =
    if !Clflags.nopervasives
    then Env.initial
    else
      open_implicit_module "Pervasives" Env.initial
  in env

and parse_interface (env : Env.t) (mli : string) : Parsetree.signature =
  let file_name = Filename.concat !Rpath.ocaml_raml_runtime mli in
  let ch = In_channel.create file_name in
  let buf = Lexing.from_channel ch in
  let _ = Location.init buf file_name in
  let parsetree = Parse.interface buf in
  let _ = In_channel.close ch in        
  parsetree

and interface sourcefile env_inital =
  Location.input_name := sourcefile;
  let modulename =
    String.capitalize(Filename.basename (Misc.chop_extension_if_any sourcefile)) in
  let outputprefix = Misc.chop_extension_if_any sourcefile in
  Env.set_unit_name modulename;
  let ast = parse_interface env_inital sourcefile in 
  let tsg = Typemod.transl_signature env_inital ast in
  let sg = tsg.sig_type in
  ignore (Includemod.signatures env_inital sg sg);
  begin
    let _ = printf "  Compiling %s " modulename in
    let sg = Env.save_signature sg modulename 
        (Filename.concat !Rpath.ocaml_raml_runtime (outputprefix ^ ".cmi")) in
    let _ = 
      Typemod.save_signature modulename tsg outputprefix sourcefile
        env_inital sg ;
    in
    printf "... done.\n"
  end

and gen_pervasives sourcefile = interface sourcefile Env.initial
and gen_runtime sourcefile = interface sourcefile (initial_env ())






let analyze_module_learn_ocaml analysis_mode m_name metric deg1 deg2 collect_fun_types m env =
  let amode_name =
    match analysis_mode with
    | Mupper -> "upper"
    | Mlower -> "lower"
    | Mconstant -> "constant"
  in

  let analyze_fun f_name e =
    let e_normal = Shareletnormal.share_let_normal "#" e in
    let e_normal_stack = Typecheck.typecheck_stack ~linear:true e_normal env in
    let () = printf "Analyzing function %s ...\n" f_name in
    let rec analyze_f deg deg_max =
      assert (deg <= deg_max);
      assert (deg >= 1);
      printf "%i" deg;
      let module Clp = (
        val (
          match analysis_mode with
          | Mlower -> (
              module Solver.Clp( Solver.Clp_std_maximize )
            )
          | Mupper
          | Mconstant -> (
              module Solver.Clp( Solver.Clp_std_options )
            )
        ) : Solver.SOLVER
      )
      in
      let module Amode =
      struct
        let mode = analysis_mode
      end
      in
      let module Analysis = Analysis.Make( Clp )(Amode) in
      (*Try to derive bound with deg, increase deg in attempt to derive bound so long as you are below deg_max *)
      match Analysis.analyze_function e_normal_stack ~metric ~degree:deg ~collect_fun_types with
      | None ->
            let _ = if deg < deg_max then printf ", %!" else printf "\n%!" in
            if deg < deg_max then
              analyze_f (deg+1) deg_max
            else
              None
      | Some (atarg,atres,fun_type_list) ->Some (f_name,atarg,atres,fun_type_list)
                        
	            
    in
    analyze_f deg1 deg2
  in
  let _ = print_string "starting typecheck \n" in
  let () = tcheck_module m env in
  let _ = print_string "typecheck done \n" in
  
  (* FLAG 
    Actual code that runs the analysis of an individual function, limit execution time to 3 seconds
  *)
  let f  =
    limit_fun 1
    (fun (f_name,e) ->
      match e.Expressions.exp_type with
      | Rtypes.Tarrow _ -> Some (analyze_fun f_name e)
      | _ -> None
    )
  in
    let join x = match x with 
                | Some s -> s
                | None -> None
  in 
    let rec process lst = match lst with 
                          | [] -> []
                          | None :: xs -> process xs 
                          | (Some t) :: xs -> t :: (process xs)
  in

    process (List.map m (fun x -> print_string "checking \n" ; join (f x)))




module Learnocaml_report = struct 
	type t = item list

	and item =
	   | Section of text * t
	   | SectionMin of text * t * int
	   | Message of text * status

	 and status =
	   | Success of int | Penalty of int | Failure
	   | Warning | Informative | Important

	 and text = inline list

	 and inline =
	   | Text of string
	   | Break
	   | Code of string
	   | Output of string
end



module Serialize = struct 
  
	open Lwt
	open Cohttp
	open Cohttp_lwt
	open Cohttp_lwt_unix


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
					| _ -> "(" (* Broke Sexp indicates failure*)   

				in
					server g)






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




  end 


open Serialize
    

let rec printList lst = match lst with 
                        | [] -> ()
                        | x :: xs -> print_string x; printList xs

let main argv = 
  (* Uses JS CORE List and not ocaml stdlib List *)
  let args = List.tl_exn (Array.to_list argv) in
  match args with
  | action::args -> begin
    match action with
    | "gen-runtime" -> 
                    begin match List.hd args with
                      | Some sourcefile ->
                        gen_runtime sourcefile
                      | None ->
                        let () = printf "Generating runtime mli files.\n\n" in
                        let _ = printList persistent_modules in
                        let () =
                          List.iter persistent_modules (fun m -> gen_runtime (String.lowercase m ^ ".mli"))
                        in
                        printf "\n"
                    end
                  end
  | _ -> 
                    let analysis_mode = Mupper in 
                    let m_name = "steps" in 
                    let metric = Metric.m_eval in 
                    (* Attempts to derive bound with lower degree, if it fails, it increase the degree and tries again *)
                    let deg1 = 4 in (* lower degree*) 
                    let deg2 = 4 in (* upper degree *)
                    let pmode = Rconfig.Pnone in 
                    
                    (*let analyze_p = analyze_prog analysis_mode m_name metric deg1 deg2 pmode in *)
                    let analyze_m = analyze_module_learn_ocaml analysis_mode m_name metric deg1 deg2 pmode in
                    (*let buf = Lexing.from_channel In_channel.stdin in
                    let _ = Location.init buf "<stdin>" in *) (* read from stdin *)
                   
                    (* let (e, env) = Parseraml.parse_raml_module "timed.ml" in *)

                    (* let lst = analyze_m e env in (*   (f_name * atarg * atres * fun_type_list) list   *)
                    () *)

                    ignore @@ Lwt_main.run @@ reply (sig_gen unit_helper string_helper) (fun () -> "This is a test")






                    
let _ = main (Sys.argv);;






