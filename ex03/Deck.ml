(* -------------------------------------------------------------------------- *)
module Card =
    struct
    module Color =
        struct
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
        end
    (* ---------------------------------------------------------------------- *)

    (* ---------------------------------------------------------------------- *)
    module Value =
        struct
            type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen
                        | King | As

            let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

            let toInt card = match card with
                | T2    -> 1
                | T3    -> 2
                | T4    -> 3
                | T5    -> 4
                | T6    -> 5
                | T7    -> 6
                | T8    -> 7
                | T9    -> 8
                | T10   -> 9
                | Jack  -> 10
                | Queen -> 11
                | King  -> 12
                | As    -> 13

            let toString card = match card with
                | T2    -> "2"
                | T3    -> "3"
                | T4    -> "4"
                | T5    -> "5"
                | T6    -> "6"
                | T7    -> "7"
                | T8    -> "8"
                | T9    -> "9"
                | T10   -> "10"
                | Jack  -> "J"
                | Queen -> "Q"
                | King  -> "K"
                | As    -> "A"

            let toStringVerbose card = match card with
                | T2    -> "2"
                | T3    -> "3"
                | T4    -> "4"
                | T5    -> "5"
                | T6    -> "6"
                | T7    -> "7"
                | T8    -> "8"
                | T9    -> "9"
                | T10   -> "10"
                | Jack  -> "Jack"
                | Queen -> "Queen"
                | King  -> "King"
                | As    -> "As"

            (* A function fromInt with a modulo 13 and toInt could be used to
             * prettify the next two functions but we must respect a norm.
            *)
            let next card = match card with
                | T2    -> T3
                | T3    -> T4
                | T4    -> T5
                | T5    -> T6
                | T6    -> T7
                | T7    -> T8
                | T8    -> T9
                | T9    -> T10
                | T10   -> Jack
                | Jack  -> Queen
                | Queen -> King
                | King  -> As
                | As    -> invalid_arg "As hasn't got a next value"

            let previous card = match card with
                | T2    -> invalid_arg "T2 hasn't got a previous value"
                | T3    -> T2
                | T4    -> T3
                | T5    -> T4
                | T6    -> T5
                | T7    -> T6
                | T8    -> T7
                | T9    -> T8
                | T10   -> T9
                | Jack  -> T10
                | Queen -> Jack
                | King  -> Queen
                | As    -> King
        end

        type t = {value: Value.t; color: Color.t}
        (* ------------------------------------------------------------------ *)
        let newCard value color = {value; color}
        (* ------------------------------------------------------------------ *)

        (* ------------------------------------------------------------------ *)
        let allSpades   = List.map (fun value -> newCard value Color.Spade)
                            Value.all

        let allHearts   = List.map (fun value -> newCard value Color.Heart)
                            Value.all

        let allDiamonds = List.map (fun value -> newCard value Color.Diamond)
                            Value.all

        let allClubs    = List.map (fun value -> newCard value Color.Club)
                            Value.all

        let all         = allClubs @ allHearts @ allSpades @ allDiamonds
        (* ------------------------------------------------------------------ *)
        let getColor {value; color} = color

        let getValue {value; color} = value
        (* ------------------------------------------------------------------ *)

        (* ------------------------------------------------------------------ *)
        let toString {value; color}         =
            Printf.sprintf "%s%s" (Value.toString value) (Color.toString color)

        let toStringVerbose {value; color}  =
            Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose value)
            (Color.toStringVerbose color)
        (* ------------------------------------------------------------------ *)

        (* ------------------------------------------------------------------ *)
        (* Comparaisons *)
        let compare card1 card2 =
            let value1 = getValue card1 and value2 = getValue card2 in
            (Value.toInt value1) - (Value.toInt value2)

        let max card1 card2     =   if compare card1 card2 >= 0 then card1
                                    else card2

        let min card1 card2     =   if compare card1 card2 <= 0 then card1
                                    else card2

        let best card_list      = match card_list with
            | []            -> invalid_arg "Empty list"
            | card_list     -> List.fold_left max (List.hd card_list) card_list
        (* ------------------------------------------------------------------ *)

        (* ------------------------------------------------------------------ *)
        let isOf card color         = let c = getColor card in c = color

        let isSpade card    = isOf card Color.Spade

        let isHeart card    = isOf card Color.Heart

        let isDiamond card  = isOf card Color.Diamond

        let isClub card     = isOf card Color.Club
    (* ---------------------------------------------------------------------- *)
    end

type t                      = Card.t list

let newDeck ()              =
    let rec randomDeck nb =
        if nb = 0 then []
        else (List.nth Card.all (Random.int 52)) :: randomDeck (nb - 1)
    in randomDeck 52

let toStringList t          = List.map Card.toString t
let toStringListVerbose t   = List.map Card.toStringVerbose t

let drawCard t              = match t with
    | []            -> raise (Failure ("Empty deck"))
    | head :: tail  -> (head, tail)
