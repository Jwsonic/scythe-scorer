module Tests exposing (..)

import PlayerTests
import PowerTests
import Test exposing (..)


all : Test
all =
    describe "Scythe" [ PowerTests.all, PlayerTests.all ]
