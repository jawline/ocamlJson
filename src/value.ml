open Core

type t =
  | Number of float
  | String of string
  | Array of t list
  | Bool of bool
  | Object of ((string, t, String.comparator_witness) Map.t)
