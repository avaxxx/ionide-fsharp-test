namespace TestingProject

open Xunit
open FsUnit.Xunit
open Argu
open MathNet.Numerics.LinearAlgebra

module Tests =
    let (|EndOfGame|IncompleteStrike|Strike|Normal|Other|) (l, frame) =
        match l with
        | _ when frame = 11            -> EndOfGame(0)
        | [10;s]                       -> IncompleteStrike(10+s+s)
        | 10::s::n::tail               -> Strike(10+s+n, s::n::tail)
        |  f::s::n::tail when f+s = 10 -> Normal(f+s+n,  n::tail)
        |  f::s::n::tail               -> Normal(f+s,    n::tail)
        | ls                           -> Other(List.sum ls)

    let scoreBowls bowls =
        let rec scoreBowls' frame l currentscore =
            let nextframe = scoreBowls' (frame+1)
            match (l, frame) with
            | EndOfGame(score)        -> currentscore + score
            | IncompleteStrike(score) -> currentscore + score
            | Strike(score, l)        -> nextframe l (currentscore + score)
            | Normal(score, l)        -> nextframe l (currentscore + score)
            | Other(score)            -> currentscore + score
        scoreBowls' 1 bowls 0

    


    [<Fact>]
    let ``My test`` () =
        let i = 10 
        let m = matrix [[ 1.0; 2.0 ]
                        [ 3.0; 4.0 ]]
        let m' = m.Inverse()
        Assert.True(true)

    [<Fact>]
    let ``My test 2`` () =
        Assert.True(true)

    [<Fact>]
    let ``Fail every time`` () = Assert.True(false)

    [<Fact>]
    let ``with simple scores should get the expected score.`` () =
        scoreBowls [1;2;3] |> should equal 6
   
