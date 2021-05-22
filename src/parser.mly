%token <float> NUMBER
%token <string> STRING
%token END OPEN_OBJECT CLOSE_OBJECT OPEN_ARRAY CLOSE_ARRAY COMMA TRUE FALSE COLON

%type <Value.t> main
%start main
%{ open Value %}
%{ open Core %}

%%

main:
  | expr END { $1 }
;

expr:
  | NUMBER { Number($1) }
  | STRING { String($1) }
  | TRUE { Bool(true) }
  | FALSE { Bool(false) }
  | array { $1 }
  | json_object { $1 }
;

array:
  | OPEN_ARRAY CLOSE_ARRAY { Array([]) }
  | OPEN_ARRAY expr array_parts CLOSE_ARRAY { Array($2::$3) }
;

array_parts:
  | { [] }
  | COMMA expr array_parts { ($2::$3) }
;

json_object:
  | OPEN_OBJECT object_contents CLOSE_OBJECT { (Object $2) }
;

object_contents:
  | { Map.empty (module String) }
  | object_entry object_contents_rest {
    let (key, data) = $1 in
    match (Map.add $2 ~key ~data) with
    | `Ok a -> a
    | `Duplicate -> raise_s (sexp_of_string "Duplicate Object Entry")
  }
;

object_contents_rest:
  | { Map.empty (module String) }
  | COMMA object_entry object_contents_rest {
    let (key, data) = $2 in
    match Map.add $3 ~key ~data with
    | `Ok a -> a
    | `Duplicate -> raise_s (sexp_of_string "Duplicated Object Entry")
  }
;

object_entry:
  | STRING COLON expr { ($1, $3) }
;
