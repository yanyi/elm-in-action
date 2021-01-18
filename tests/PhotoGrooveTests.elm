module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeString)
import PhotoGroove
import Test exposing (..)



-- A unit test is defined as a test that runs once, and whose test logic does not perform effects.


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)"
        (\_ ->
            """
                {"url": "fruits.com", "size": 5}
            """
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)" })
        )
