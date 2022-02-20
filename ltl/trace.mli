module State : sig
  include Set.S with type elt = char
end

val state_of: char list -> State.t

type trace = State.t list
