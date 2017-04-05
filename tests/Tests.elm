module Tests exposing (..)

import Expect
import Scythe exposing (..)
import Test exposing (..)


-- import Fuzz exposing (list, int, tuple, string)
-- import String

import Scythe exposing (..)


all : Test
all =
    describe "Sycthe Scorer"
        [ describe "setPower"
            [ test "Can't add power < 0" <|
                \() ->
                    Expect.equal (setPower initPlayer -1) initPlayer
            , test "Cant' add power > 7" <|
                \() ->
                    Expect.equal
                        (setPower initPlayer 8)
                        { power = 7, card = 0 }
            ]
        , describe "setCard"
            [ test "Can't add card < 0" <|
                \() ->
                    Expect.equal (setCard initPlayer -1) initPlayer
            , test "Cant' add card > 5" <|
                \() ->
                    Expect.equal (setCard initPlayer 6) { power = 0, card = 5 }
            , test "Can't add card == 1" <|
                \() ->
                    Expect.equal (setCard initPlayer 1) initPlayer
            ]
        , describe "updatePlayer"
            [ test "handles SetPower" <|
                \() ->
                    Expect.equal
                        (updatePlayer (SetPower 5) initPlayer)
                        { power = 5, card = 0 }
            , test "handles SetCard" <|
                \() ->
                    Expect.equal
                        (updatePlayer (SetCard 3) initPlayer)
                        { power = 0, card = 3 }
            , test "ignores other messages" <|
                \() ->
                    Expect.equal (updatePlayer Reset initPlayer) initPlayer
            ]
        , describe "updatePage"
            [ test "Defending succeeds Attacking" <|
                \() ->
                    Expect.equal (updatePage Attacking) Defending
            , test "Results succeeds Defending" <|
                \() ->
                    Expect.equal (updatePage Defending) Results
            , test "Results succeeds itself" <|
                \() ->
                    Expect.equal (updatePage Results) Results
            ]
        , describe "Reset"
            [ test "Completely resets the model" <|
                \() ->
                    Expect.equal
                        (update Reset
                            { attackingPlayer =
                                { power = 3
                                , card = 3
                                }
                            , defendingPlayer =
                                { power = 2
                                , card = 4
                                }
                            , page = Results
                            }
                        )
                        initialModel
            ]
        , describe "NextPage"
            [ test "Moves to the next page" <|
                \() ->
                    Expect.equal
                        (update NextPage initialModel)
                        { initialModel | page = Defending }
            ]
        , describe "SetPower"
            [ test "sets attacking power when it's the attacking player's turn" <|
                \() ->
                    Expect.equal (update (SetPower 5) initialModel)
                        { initialModel
                            | attackingPlayer = { power = 5, card = 0 }
                        }
            , test "sets defending power when it's the defending player's turn" <|
                \() ->
                    Expect.equal
                        (update (SetPower 5)
                            { initialModel
                                | page = Defending
                            }
                        )
                        { initialModel
                            | defendingPlayer = { power = 5, card = 0 }
                            , page = Defending
                        }
            ]
        , describe "SetCard"
            [ test "sets attacking card when it's the attacking player's turn" <|
                \() ->
                    Expect.equal (update (SetCard 4) initialModel)
                        { initialModel
                            | attackingPlayer = { power = 0, card = 4 }
                        }
            , test "sets defending card when it's the defending player's turn" <|
                \() ->
                    Expect.equal
                        (update (SetCard 4)
                            { initialModel
                                | page = Defending
                            }
                        )
                        { initialModel
                            | defendingPlayer = { power = 0, card = 4 }
                            , page = Defending
                        }
            ]
        ]
