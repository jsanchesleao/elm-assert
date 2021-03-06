module Test where

import Assert exposing (report, suite, test)

main =
  report "My App" 
    [ suite "Testing Strings" 
      [ test "values are equal" "ok" "ok"
      , test "values are not equal" "bla" "ble"
      , test "records" {a = 1} {a = 1}
      , test "records failing" {a = 1} {a = 2}
      ]
    ]
