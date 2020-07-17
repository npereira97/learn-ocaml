open Test_lib 
open Report 


module Style_check = Style_checking.Make ()

let forbidden_construct_str =
  "Unable to process your code for style checking because you have used " ^
  "a language construct that is not supported in our course."
let forbidden_construct_msg =
  Message ([Text forbidden_construct_str], Failure)

let text_sample = [Text "Resource Analysis report";Break;Break;Code "fun x -> x"]
let message_sample = Message ([Text "This is an informative message"], Informative)
let temp = message_sample :: [Section (text_sample,[])]
let temp = [Section ([Text " Resource analysis report "],[Message ([Text "This is the beginning of the resource analysis output";Break;Break;Break;Output "I'm not sure what this is supposed to do ";Break;Break;Text "Output should end here"],Informative);Message ([Text "This is some sample code";Break;Code " fun x -> x "],Warning);Section ([Text " This is a subsection "],[Message ([Text " You got to the message "],Informative)])])]




let test () =
  try
    let tast = Typed_ast_lib.tast_of_parsetree_structure code_ast in
    let checkers = Style_check.all_checkers () in
     (Style_check.ast_style_check_structure checkers tast)
  with exn -> [forbidden_construct_msg]



let () =
  set_result (*temp*) @@
  ast_sanity_check code_ast @@ test
  
(*




let g = (fun () -> match Resource_analysis.test1 () with 
			| None -> [Message ([Text "Womp womp"],Informative)]
			| Some x -> x) 



-- type t = item list

-- and item =
--   | Section of text * t
--   | SectionMin of text * t * int
--   | Message of text * status

-- and status =
--   | Success of int | Penalty of int | Failure
--   | Warning | Informative | Important

-- and text = inline list

-- and inline =
--   | Text of string
--   | Break
--   | Code of string
--   | Output of string
*)

