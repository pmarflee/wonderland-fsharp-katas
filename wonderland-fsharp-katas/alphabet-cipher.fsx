// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

let substitutionChart : Lazy<Map<char, (char * char)[]>> =
    lazy
    (
        let alphabet = [|'a'..'z'|]
        let length = Array.length alphabet
        let getPermutation offset = 
            let permutation =
                if (offset < length) then
                    Array.permute (fun index -> (index + offset) % length) alphabet
                else
                    alphabet
            Array.zip alphabet permutation
        [ for i in length .. -1 .. 1 -> (alphabet.[length - i], getPermutation i) ] |> Map.ofList
    )

let getLetter (keyLetter:char) (letter:char) (comparer:(char * char) -> char) (selector:(char * char) -> char) : char =         
    substitutionChart.Value.[keyLetter] 
        |> Array.find (fun item -> (comparer item) = letter) 
        |> selector

let encode_decode (key:Keyword) (message:Message) (comparer:(char * char) -> char) (selector:(char * char) -> char) : Message =
    let encodeLetter i letter = getLetter key.[i % key.Length] letter comparer selector
    Seq.mapi encodeLetter message |> System.String.Concat

let encode (key:Keyword) (message:Message) : Message = encode_decode key message fst snd

let decode (key:Keyword) (message:Message) : Message = encode_decode key message snd fst

let decipher (cipher:Message) (message:Message) : Keyword =
    let keyText = Seq.map2 (fun c m -> getLetter m c snd fst) cipher message |> System.String.Concat
    seq {for i in 1 .. keyText.Length do
            let chunks = Seq.chunkBySize i keyText
            let pairedChunks = keyText |> Seq.chunkBySize i |> Seq.pairwise |> List.ofSeq
            if pairedChunks |> Seq.forall (fun (first, second) -> first = second || first.Length <> second.Length) then
                yield pairedChunks |> Seq.head |> fst |> System.String.Concat
    } |> Seq.head

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
