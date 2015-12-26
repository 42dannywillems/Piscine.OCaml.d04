type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart; Diamond; Club]

let toString color = match color with
    | Spade     -> "S"
    | Heart     -> "H"
    | Diamond   -> "D"
    | Club      -> "C"

let toStringVerbose color = match color with
    | Spade     -> "Spade"
    | Heart     -> "Heart"
    | Diamond   -> "Diamond"
    | Club      -> "Club"
