module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove
import Test exposing (..)



-- A unit test is defined as a test that runs once, and whose test logic does not perform effects.


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue PhotoGroove.photoDecoder
                {- Test only the `title` field in the Photo record.
                   `.title` gives us a function that takes a record and returns the contents of its `title` field.
                -}
                |> Result.map .title
                |> Expect.equal
                    (Ok "(untitled)")
