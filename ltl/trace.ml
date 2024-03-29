module State = struct
  include Set.Make (Char)
end [@@deriving equal]

let state_of states = State.of_list states

let state_of_string s =
  let state = ref State.empty in
  String.iter (fun c -> state := State.add c !state) s ;
  !state

let state_to_string : State.t -> string =
 fun s ->
  let str = ref "" in
  State.iter (fun c -> str := !str ^ String.make 1 c) s ;
  !str

type trace = State.t list

let equal_trace _ _ = false
    
let print_trace trace =
  "["
  ^ StdLabels.String.concat ~sep:", " (List.map state_to_string trace)
  ^ "]"

(* let setTraceState: (bool, char, int, trace) => trace = (enabled, name, i,
   trace) => { let modifyState = s => if enabled { Belt.Set.add(s, name) }
   else { Belt.Set.remove(s, name) } List.mapi((i', s) => if i == i' {
   modifyState(s) } else { s } , trace) } *)
