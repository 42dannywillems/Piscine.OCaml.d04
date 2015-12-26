let () =
    let rec toStringAll f l = match l with
        | []            -> print_string ""
        | head::tail    -> print_endline (f head); toStringAll f tail
    in
    let rec toString_next_previous f print l = match l with
        | []            ->  print_string ""
        | head::tail    ->  print_endline ("Next of " ^ (print head) ^ " is " ^
                            (print (f head))); toString_next_previous f print
                            tail
    in
    print_endline "------------------ test all with toString -----------------";
    toStringAll (Value.toString) Value.all;
    print_endline "------------------ test all with toStringAll --------------";
    toStringAll (Value.toStringVerbose) Value.all;
    print_endline "------------------ test next with toString ----------------";
    toString_next_previous Value.next Value.toString Value.all;
    print_endline "------------------ test next with toStringVerbose ---------";
    toString_next_previous Value.next Value.toStringVerbose Value.all;
    print_endline "----------- test previous with toString -------------------";
    toString_next_previous Value.previous Value.toString Value.all;
    print_endline "----------- test previous with toStringAll ----------------";
    toString_next_previous Value.previous Value.toStringVerbose Value.all
