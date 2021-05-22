open Core
open Value

let printf = Printf.printf

let parse (json : string) =
  let linebuf = Lexing.from_string json in
  Parser.main Lexer.token linebuf
;;

exception Not_Numeric_Value
exception Not_Object

let as_number (v: Value.t) =
  match v with
  | Number v -> v
  | _ -> raise Not_Numeric_Value
;;

let get_child (v: Value.t) (k: string) =
  match v with
  | Object map -> Map.find map k
  | _ -> raise Not_Object
;;

exception Invalid_Structure
exception Invalid_Value

let%test "parse_int" = match (parse "5213") with
  | Number x when Float.(=) x 5213.0 -> true
  | Number _ -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_float" = match (parse "5213.69432") with
  | Number x when Float.(=) x 5213.69432 -> true
  | Number _ -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_bool_true" = match (parse "true") with
  | Bool x when x -> true
  | Bool _ -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_bool_false" = match (parse "false") with
  | Bool x when not x -> true
  | Bool _ -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_empty_array" = match (parse "[]") with
  | Array([]) -> true
  | _ -> raise Invalid_Structure
;;

let%test "parse_homogenous_array" = match (parse "[5,6,7]") with
  | Array((Number a)::(Number b)::(Number c)::[]) when Float.(=) a 5.0 && Float.(=) b 6.0 && Float.(=) c 7.0 -> true
  | Array(_) -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_mixed_number_bool_array" = match (parse "[5,false,7,true]") with
  | Array((Number a)::(Bool b)::(Number c)::(Bool d)::[]) when Float.(=) a 5.0 && not b && Float.(=) c 7.0 && d -> true
  | Array(_) -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_empty_object" = match (parse "{}") with
  | Object(map) when Map.length map = 0 -> true
  | Object(_) -> raise Invalid_Value
  | _ -> raise Invalid_Structure
;;

let%test "parse_sub_object" = match (parse "{ \"obj1\": { \"key1\": 5 } }") with
  | Object(outer_object) when Map.length outer_object = 1 -> (
    match get_child (Object outer_object) "obj1" with
    | Some inner_object -> (
      match get_child inner_object "key1" with
        | Some (Number x) when Float.(=) x 5.0 -> true
        | Some _ -> raise Invalid_Value
        | None -> raise Invalid_Structure
    )
    | None -> raise Invalid_Structure
  )
  | Object(_) -> raise Invalid_Value
  | _ -> raise Invalid_Structure
