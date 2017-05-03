module Tests exposing (..)

import PlayerTests
import ScytheTests
import Test exposing (..)


all : Test
all =
    describe "Scythe" [ PlayerTests.all, ScytheTests.all ]
