open Array;;


let get_current_box () = 
  let ic = open_in ".confident/status"
  in let box_name = input_line ic
     in close_in ic; box_name;;


let main () = 
  if length Sys.argv < 2 then begin
    print_string "Usage: supported commands"; 
    print_newline ();
    print_string "init";
    print_newline ();
    print_string "update";
    print_newline ();
    print_string "save";
    print_newline ();
    print_string "switch";
    print_newline ();
    print_string "search";
    print_newline ();
    print_string "list";
    print_newline ()
  end else begin
    match Sys.argv.(1) with
      "init" -> 
        if Sys.file_exists ".confident" 
        then begin 
          print_string "[Warning] repository already exists.";
          print_newline ()
        end else begin
          Unix.mkdir ".confident" 0o755;
          Unix.mkdir ".confident/default" 0o755;
          Unix.mkdir "conf" 0o755;
          Unix.mkdir "result" 0o755;
          let oc = open_out ".confident/status"
          in output_string oc "default\n";
          let oc = open_out (".confident/default/description")
          in output_string oc ( "This is the default description\n" ); close_out oc;
          print_string "[ok] repository created.";
          print_newline ();
        end
    | "update" -> 
      if not (Sys.file_exists ".confident") 
      then begin 
        print_string "[Error] not a repository.";
        print_newline ()
      end else let box_name = get_current_box () in
               ignore (Sys.command ("rm -rf .confident/" ^ box_name ^ "/conf"));
               ignore (Sys.command ("rm -rf .confident/" ^ box_name ^ "/result"));
               ignore (Sys.command ("cp -rf conf/ .confident/" ^ box_name ^ "/"));
               ignore (Sys.command ("cp -rf result/ .confident/" ^ box_name ^ "/"));
               print_string ( "[ok] " ^ box_name ^ " gets updated." );
               print_newline ()
    | "save" -> (* format : save name description*)
      ( match length Sys.argv with
        (* todo: support prompt name/description *)
        2 | 3 -> print_string "[Error] insufficient arguments"; print_newline () 
      | _ ->  let name = Sys.argv.(2) and desc = Sys.argv.(3) in
              if Sys.file_exists ( ".confident/" ^ name ) then begin
                print_string ("[Error] profile " ^ name ^ " already exists.");
                print_newline ();
                print_string "Do you mean update?";
                print_newline ()
              end else begin
                Unix.mkdir (".confident/" ^ name) 0o755;
                 (* write description *)
                let oc = open_out (".confident/" ^ name ^ "/description")
                in output_string oc ( desc ^ "\n" ); close_out oc;
                 (* copy conf and result *)
                ignore (Sys.command ("cp -rf conf/ .confident/" ^ name ^ "/"));
                ignore (Sys.command ("cp -rf result/ .confident/" ^ name ^ "/"));
                let oc = open_out ".confident/status"
                in output_string oc ( name ^ "\n" );
                print_string ( "[ok] " ^ name ^ " saved." );
                print_newline ()
              end 
      )
    | "switch" -> (* format : switch name *)
      ( match length Sys.argv with
        2 -> print_string "[Error] insufficient arguments"; print_newline ()
      | _ -> let name = Sys.argv.(2) in
             if Sys.file_exists ( ".confident/" ^ name ) then begin
               ignore (Sys.command "rm -rf conf/");
               ignore (Sys.command "rm -rf result/");
               ignore (Sys.command ("cp -rf .confident/" ^ name ^ "/conf ."));
               ignore (Sys.command ("cp -rf .confident/" ^ name ^ "/result ."));
               let oc = open_out ".confident/status"
               in output_string oc ( name ^ "\n" );
               print_string ( "[ok] switch to profile " ^ name ^ "." );
               print_newline ()
             end else begin
               (* todo: find the closest profile in name and switch to *)
               print_string ("[Error] profile " ^ name ^ " does not exists.");
               print_newline ();
             end
      )
    | "list" | "ls" -> 
      if not (Sys.file_exists ".confident") 
      then begin 
        print_string "[Error] not a repository.";
        print_newline ()
      end else 
        Array.iter 
          ( fun name -> if Sys.is_directory (".confident/" ^ name) 
            then begin 
              print_string name;
              (* todo: pretty box printing *)
              let ic = open_in (".confident/" ^ name ^ "/description")
              in print_string ( "\"" ^ ( input_line ic ) ^ "\"" );
              print_newline ()
            end else () )
          ( Sys.readdir ".confident/" );
    | _ -> print_string "[Error] failed to recognize the command."; print_newline ()
  end;;


if !Sys.interactive then () else main ();;
