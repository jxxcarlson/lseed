module TestData exposing (passwordDict, userDict)

import Dict
import Time
import User exposing (PasswordDict, UserDict, UserInfo)


passwordDict : PasswordDict
passwordDict =
    Dict.fromList
        [ ( "jxxcarlson", "!@oboLocol@!" )
        , ( "socrates", "!@citpeks@!" )
        ]


userDict =
    Dict.fromList
        [ ( "jxxcarlson", { email = "jxxcarlson@gmail.com", admin = True, counter = 0 } )
        , ( "socrates", { email = "socrates@gmail.com", admin = False, counter = 0 } )
        ]
