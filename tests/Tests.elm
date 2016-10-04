module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Time exposing (..)
import TimeAgo exposing (..)


all : Test
all =
    describe "A Test Suite"
        [ test "Treats 30 seconds or less as less than a minute" <|
            \() ->
                Expect.equal (timeAgoInWords (30 * second)) "less than a minute"
          --
        , test "Rounds 31 secinds to 1 minute" <|
            \() ->
                Expect.equal (timeAgoInWords (31 * second)) "1 minute"
          --
        , test "Rounds 89 secnds to 1 minute" <|
            \() ->
                Expect.equal (timeAgoInWords (89 * second)) "1 minute"
          --
        , test "Rounds 90 seconds to 2 minutes" <|
            \() ->
                Expect.equal (timeAgoInWords (90 * second)) "2 minutes"
          --
        , test "Reports 44 minues as 44  minutes" <|
            \() ->
                Expect.equal (timeAgoInWords (44 * minute)) "44 minutes"
          --
        , test "Rounds 45 minutes to about an hour" <|
            \() ->
                Expect.equal (timeAgoInWords (45 * minute)) "about an hour"
          --
        , test "Rounds 89 minutes to about an hour" <|
            \() ->
                Expect.equal (timeAgoInWords (89 * minute)) "about an hour"
          --
        , test "Rounds 90 minutes to about 2 hours" <|
            \() ->
                Expect.equal (timeAgoInWords (90 * minute)) "about 2 hours"
          --
        , test "Rounds 149 minutes to about 2 hours" <|
            \() ->
                Expect.equal (timeAgoInWords (149 * minute)) "about 2 hours"
          --
        , test "Rounds 150 minutes to about 3 hours" <|
            \() ->
                Expect.equal (timeAgoInWords (150 * minute)) "about 3 hours"
          --
        , test "Rounds 23 hours, 29 minutes to about 23 hours" <|
            \() ->
                Expect.equal (timeAgoInWords (23 * hour + 29 * minute)) "about 23 hours"
          --
        , test "Rounds 23 hours, 30 minutes to about 24 hours" <|
            \() ->
                Expect.equal (timeAgoInWords (23 * hour + 30 * minute)) "about 24 hours"
          --
        , test "Rounds 23 hours, 59 minutes to about 24 hours" <|
            \() ->
                Expect.equal (timeAgoInWords (23 * hour + 59 * minute)) "about 24 hours"
          --
        , test "Rounds 24 hours to 1 day" <|
            \() ->
                Expect.equal (timeAgoInWords (24 * hour)) "1 day"
          --
        , test "Rounds 35 hours 59 minutes to 1 day" <|
            \() ->
                Expect.equal (timeAgoInWords (35 * hour + 59 * minute)) "1 day"
          --
        , test "Rounds 36 hours to 2 days" <|
            \() ->
                Expect.equal (timeAgoInWords (36 * hour)) "2 days"
          --
        , test "Rounds 59 hours 59 minutes to 2 days" <|
            \() ->
                Expect.equal (timeAgoInWords (59 * hour + 59 * minute)) "2 days"
          --
        , test "Rounds 60 hours to 3 days" <|
            \() ->
                Expect.equal (timeAgoInWords (60 * hour)) "3 days"
          --
        , test "Rounds 30 days to 1 month" <|
            \() ->
                Expect.equal (timeAgoInWords (30 * 24 * hour)) "about 1 month"
          --
        , test "Rounds 44 days to 1 month" <|
            \() ->
                Expect.equal (timeAgoInWords (44 * 24 * hour)) "about 1 month"
          --
        , test "Rounds 45 days to 2 month" <|
            \() ->
                Expect.equal (timeAgoInWords (45 * 24 * hour)) "about 2 months"
          --
        , test "Rounds 59 days to 2 month" <|
            \() ->
                Expect.equal (timeAgoInWords (59 * 24 * hour)) "about 2 months"
          --
        , test "Rounds 60 days to 2 month" <|
            \() ->
                Expect.equal (timeAgoInWords (60 * 24 * hour)) "2 months"
        ]
