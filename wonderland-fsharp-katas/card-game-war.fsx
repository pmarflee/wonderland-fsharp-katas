// See the file card-game.md for detailed information.

// feel free to use these cards or use your own data structure

type Player =
    | Player1
    | Player2

type Suit =
    | Spade
    | Club
    | Diamond
    | Heart

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Suit * Rank

let suits = [ Spade; Club; Diamond; Heart ]
let heads = [ Jack; Queen; King; Ace ]
let ranks =
    [   for v in 2 .. 10 -> Value v
        for head in heads -> head
    ]
let deck : seq<Card> = seq {
    for suit in suits do
        for rank in ranks -> (suit, rank) }

let getHighestRankedCard (card1:Card) (card2:Card) : Card =
    let getCardRankIndex (card:Card) = deck |> Seq.findIndex ((=) card)
    if getCardRankIndex card1 > getCardRankIndex card2 then card1 else card2

let playRound (hand1:Card list, hand2:Card list) : Card list * Card list =
    let card1 = List.head hand1
    let card2 = List.head hand2
    let winner = getHighestRankedCard card1 card2
    if winner = card1 then
        (List.tail hand1) @ [card1; card2], List.tail hand2
    else
        List.tail hand1, (List.tail hand2) @ [card1; card2]

let playGame (hand1:Card list, hand2:Card list) : Player =
    let rec playGame' (hand1':Card list, hand2':Card list) : Card list * Card list =
        let newHand1', newHand2' = playRound (hand1', hand2')
        if List.isEmpty newHand1' || List.isEmpty newHand2' then
            newHand1', newHand2'
        else
            playGame' (newHand1', newHand2')
    let hand1', hand2' = playGame' (hand1, hand2)
    if List.isEmpty hand1' then
        Player2
    else
        Player1

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

// fill in tests for your game
let tests () =

    // playRound
    // the highest rank wins the cards in the round
    test <@ playRound ([(Suit.Spade, Value(2))], [(Suit.Spade, Value(3))]) = ([], [(Suit.Spade, Value(2)); (Suit.Spade, Value(3))]) @>
    test <@ playRound ([(Suit.Spade, Value(3))], [(Suit.Spade, Value(2))]) = ([(Suit.Spade, Value(3)); (Suit.Spade, Value(2))], []) @>
    test <@ playRound ([(Suit.Spade, Value(3))], [(Suit.Spade, Value(4))]) = ([], [(Suit.Spade, Value(3)); (Suit.Spade, Value(4))]) @>
    test <@ playRound ([(Suit.Spade, Value(4))], [(Suit.Spade, Value(3))]) = ([(Suit.Spade, Value(4)); (Suit.Spade, Value(3))], []) @>
    test <@ playRound ([(Suit.Spade, Value(4))], [(Suit.Spade, Value(5))]) = ([], [(Suit.Spade, Value(4)); (Suit.Spade, Value(5))]) @>
    test <@ playRound ([(Suit.Spade, Value(5))], [(Suit.Spade, Value(4))]) = ([(Suit.Spade, Value(5)); (Suit.Spade, Value(4))], []) @>
    test <@ playRound ([(Suit.Spade, Value(5))], [(Suit.Spade, Value(6))]) = ([], [(Suit.Spade, Value(5)); (Suit.Spade, Value(6))]) @>
    test <@ playRound ([(Suit.Spade, Value(6))], [(Suit.Spade, Value(5))]) = ([(Suit.Spade, Value(6)); (Suit.Spade, Value(5))], []) @>
    test <@ playRound ([(Suit.Spade, Value(6))], [(Suit.Spade, Value(7))]) = ([], [(Suit.Spade, Value(6)); (Suit.Spade, Value(7))]) @>
    test <@ playRound ([(Suit.Spade, Value(7))], [(Suit.Spade, Value(6))]) = ([(Suit.Spade, Value(7)); (Suit.Spade, Value(6))], []) @>
    test <@ playRound ([(Suit.Spade, Value(7))], [(Suit.Spade, Value(8))]) = ([], [(Suit.Spade, Value(7)); (Suit.Spade, Value(8))]) @>
    test <@ playRound ([(Suit.Spade, Value(8))], [(Suit.Spade, Value(7))]) = ([(Suit.Spade, Value(8)); (Suit.Spade, Value(7))], []) @>
    test <@ playRound ([(Suit.Spade, Value(8))], [(Suit.Spade, Value(9))]) = ([], [(Suit.Spade, Value(8)); (Suit.Spade, Value(9))]) @>
    test <@ playRound ([(Suit.Spade, Value(9))], [(Suit.Spade, Value(8))]) = ([(Suit.Spade, Value(9)); (Suit.Spade, Value(8))], []) @>
    test <@ playRound ([(Suit.Spade, Value(9))], [(Suit.Spade, Value(10))]) = ([], [(Suit.Spade, Value(9)); (Suit.Spade, Value(10))]) @>
    test <@ playRound ([(Suit.Spade, Value(10))], [(Suit.Spade, Value(9))]) = ([(Suit.Spade, Value(10)); (Suit.Spade, Value(9))], []) @>

    // queens are higher rank than jacks
    test <@ playRound ([(Suit.Spade, Jack)], [(Suit.Spade, Queen)]) = ([], [(Suit.Spade, Jack); (Suit.Spade, Queen)]) @>
    test <@ playRound ([(Suit.Spade, Queen)], [(Suit.Spade, Jack)]) = ([(Suit.Spade, Queen); (Suit.Spade, Jack)], []) @>

    // kings are higher rank than queens
    test <@ playRound ([(Suit.Spade, Queen)], [(Suit.Spade, King)]) = ([], [(Suit.Spade, Queen); (Suit.Spade, King)]) @>
    test <@ playRound ([(Suit.Spade, King)], [(Suit.Spade, Queen)]) = ([(Suit.Spade, King); (Suit.Spade, Queen)], []) @>

    // aces are higher rank than kings
    test <@ playRound ([(Suit.Spade, King)], [(Suit.Spade, Ace)]) = ([], [(Suit.Spade, King); (Suit.Spade, Ace)]) @>
    test <@ playRound ([(Suit.Spade, Ace)], [(Suit.Spade, King)]) = ([(Suit.Spade, Ace); (Suit.Spade, King)], []) @>

    // if the ranks are equal, clubs beat spades
    test <@ playRound ([(Suit.Club, Ace)], [(Suit.Spade, Ace)]) = ([(Suit.Club, Ace); (Suit.Spade, Ace)], []) @>

    // if the ranks are equal, diamonds beat clubs
    test <@ playRound ([(Suit.Diamond, Ace)], [(Suit.Club, Ace)]) = ([(Suit.Diamond, Ace); (Suit.Club, Ace)], []) @>

    // if the ranks are equal, hearts beat diamonds
    test <@ playRound ([(Suit.Heart, Ace)], [(Suit.Diamond, Ace)]) = ([(Suit.Heart, Ace); (Suit.Diamond, Ace)], []) @>

    // playGame
    test <@ playGame ([(Suit.Spade, Value(2)); (Suit.Spade, Value(4))], [(Suit.Spade, Value(3)); (Suit.Spade, Value(5))]) = Player2 @>
    test <@ playGame ([(Suit.Spade, Value(3)); (Suit.Spade, Value(5))], [(Suit.Spade, Value(2)); (Suit.Spade, Value(4))]) = Player1 @>

// run the tests
tests ()
