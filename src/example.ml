let myroutes = [
  "/foo/bar", (fun _ _ -> print_endline "Meow");
  "/slt/:a/:b/", (fun _ args -> Printf.printf "a: %s, b: %s\n"
    (List.assoc "a" args) (List.assoc "b" args));
]
    
let _ = match Array.to_list Sys.argv |> List.tl with
  | [] ->
    print_endline "Available routes:";
    List.iter (fun (s, _) -> print_endline ("- " ^ s)) myroutes
  | r :: _ ->
    let matched = Route.handle (Route.import myroutes) r in
    if not matched then print_endline "No valid route found."
