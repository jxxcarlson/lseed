module TestData exposing (passwordDict, userDict, voteCount)

import Dict
import Time
import User exposing (PasswordDict, UserDict, UserInfo)
import Vote exposing (VoteCount)


passwordDict : PasswordDict
passwordDict =
    Dict.fromList
        [ ( "jxxcarlson", "!@oboLocol@!" )
        , ( "socrates", "!@citpeks@!" )
        ]


userDict =
    Dict.fromList
        [ ( "jxxcarlson", { email = "jxxcarlson@gmail.com", admin = True, voted = False } )
        , ( "socrates", { email = "socrates@gmail.com", admin = False, voted = False } )
        ]


voteCount =
    Dict.fromList
        [ ( "Charles Babbage", 0 )
        , ( "Alexander Graham Bell", 0 )
        , ( "Thomas Edison", 0 )
        , ( "Johannes Gutenberg", 0 )
        , ( "Gugliemo Marconi", 0 )
        , ( "Nikolas Tesla", 0 )
        , ( "James Watt", 0 )
        , ( "Wright Brothers", 0 )
        ]
