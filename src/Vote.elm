module Vote exposing (VoteCount, cast, total)

import Dict exposing (Dict)


type alias VoteCount =
    Dict String Int


cast : String -> VoteCount -> VoteCount
cast candidate voteCount =
    let
        updater =
            Maybe.map (\count -> count + 1)
    in
    Dict.update candidate updater voteCount


total : VoteCount -> Int
total voteCount =
    List.sum (Dict.values voteCount)
