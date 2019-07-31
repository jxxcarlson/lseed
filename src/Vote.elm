module Vote exposing (VoteCount, cast)

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
