open Types

let main () = begin
	(*try*)
        let lexbuf = Lexing.from_channel stdin in
        print_string "| ?- ";
        flush stdout;    
        let filename = Parser.file Lexer.token lexbuf in
	    let file_handle = open_in filename in
	    let lexbuf = Lexing.from_channel file_handle in
    (*while true do*)
        let program = Parser.interpreter Lexer.token lexbuf in
        (*print_program program;*)
        print_newline();
        flush stdout;
        let lexbuf = Lexing.from_channel stdin in
        while true do
            print_string "| ?- ";
            flush stdout;
            flag := 0;
            fir := 0; 
            to_do := [];
            let goals = Parser.toplevel Lexer.token lexbuf in
            (*print_goals goals;*)
            (*print_program program;*)
            print_newline();
            flush stdout;
            solve program goals;
            (*print_prbool u;*)
            print_newline();
            flush stdout;
        done
    
    (*done*)
    (*with *)
    (*|_      -> exit 0 *)

end;;

main();;
