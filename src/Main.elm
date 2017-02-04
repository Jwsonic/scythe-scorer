module Main exposing (..)


type Model
    = AttackingPlayerEnter
    | DefendingPlayerEnter
    | CombatResolution


type alias Player =
    { power : Power
    , powerCards : List PowerCard
    }


type Power
    = Power0
    | Power1
    | Power2
    | Power3
    | Power4
    | Power5
    | Power6
    | Power7
    | Power8
    | Power9
    | Power10
    | Power11
    | Power12
    | Power13
    | Power14
    | Power15
    | Power16


type PowerCard
    = PowerCard2
    | PowerCard3
    | PowerCard4
    | PowerCard5
