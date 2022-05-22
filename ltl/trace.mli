module State : sig
  include Set.S with type elt = char
end

val state_of: char list -> State.t

val state_of_string: string -> State.t

val state_to_string: State.t -> string

type trace = State.t list

val print_trace: trace -> string
