open Test_lib 
open Report 


module Style_check = Style_checking.Make ()

let forbidden_construct_str =
  "Unable to process your code for style checking because you have used " ^
  "a language construct that is not supported in our course."
let forbidden_construct_msg =
  Message ([Text forbidden_construct_str], Failure)

let test () =
  try 
    let tast = Typed_ast_lib.tast_of_parsetree_structure code_ast in
    let checkers = Style_check.all_checkers () in
    Style_check.ast_style_check_structure checkers tast
  with exn -> [forbidden_construct_msg]

(* Generate the report to be shown to the student *)
(* 
 let log = Resource_analysis.log_error_raml



let tick = Resource_analysis.tick;; *)

exception Tester

let () =
  set_result   @@
  ast_sanity_check code_ast @@ (fun () -> match (None) with 
                                        | None -> [Section ([Break;Text ( (Sys.executable_name) ^ "This is t")],[])]
                                        | Some x -> [Section ([Break;Text (x ^ "This is t")],[])])

