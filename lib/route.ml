let reduce f = function [] -> raise (Invalid_argument "Empty List")
  | h::t -> List.fold_left f h t

let uncurry f = fun (a, b) -> f a b

(* ---------- *)

type handler = string list -> (string * string) list -> unit

type routes_shape =
| HardNode of (string * routes) list
| Wildcard of string * routes
and routes = (handler option) * routes_shape

(* / *)
let root (h: handler): routes = Some h, HardNode []

(* Helpers *)
let n n r = None, HardNode [n, r]
let nn l = None, HardNode l
let w s r = None, Wildcard (s, r)

(* /foo/:bar/bak *)
let ex1 h = None, HardNode ["foo",
                            (None, Wildcard ("bar",
                                             (None, HardNode ["bak", root h])))
                           ]

let ex1 h = n "foo" @@ w "bar" @@ n "bak" @@ root h

exception Handler_conflict
exception Wildcard_conflict
exception Wildcard_name_conflict

let rec merge r1 r2 =
  let merge_handlers h1 h2 = match (h1, h2) with
    | Some f, Some g when f == g -> h1
    | None, h | h, None -> h
    | _ -> raise Handler_conflict
  in

  match (r1, r2) with
  | (h1, HardNode l1), (h2, HardNode l2) ->
    merge_handlers h1 h2,
    HardNode (List.fold_left (fun l (s, r) ->
      try (s, merge r (List.assoc s l)) :: l
      with Not_found -> (s, r) :: l) l1 l2)

  | (h1, Wildcard (s1, r1)), (h2, Wildcard (s2, r2)) ->
    if s1 <> s2 then raise Wildcard_name_conflict;
    merge_handlers h1 h2, Wildcard (s1, merge r1 r2)

  | (_, Wildcard _), (_, HardNode _)
  | (_, HardNode _), (_, Wildcard _) -> raise Wildcard_conflict

let split_url u = 
  Str.split_delim (Str.regexp "/") u
  |> List.filter ((<>) "")

let route_of_string s h =
  split_url s
  |> List.rev
  |> List.fold_left (fun r s ->
    if s.[0] = ':' then
      w (String.sub s 1 (String.length s - 1)) r
    else
      n s r
  ) (root h)

let import (l: (string * handler) list): routes =
  reduce merge (List.map (uncurry route_of_string) l)

let handle routes url =
  let rec walk seen wildcards r u = match (u, r) with
    | [], (Some h, _) -> h (List.rev seen) wildcards; true
    | x::xs, (_, HardNode l) ->
      (try walk (x::seen) wildcards (List.assoc x l) xs with
        Not_found -> false)
    | x::xs, (_, Wildcard (s, r)) ->
      walk (x::seen) ((s, x)::wildcards) r xs
    | _, _ -> false
  in
  walk [] [] routes (split_url url)

