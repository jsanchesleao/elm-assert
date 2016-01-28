module Assert (report, suite, test) where

{-| this library is for unit testing code

# Making HTML reports
@docs report, suite, test
-}

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Assert x = {expected: x, actual: x}

type TestResult x
  = Success
  | Failure x

successStyle : Attribute
successStyle = 
  style [ ("backgroundColor", "lightGreen") 
        , ("fontFamily", "monospace")
        , ("fontWeight", "bold")
        , ("marginBottom", "5px")
        ]
  
failureStyle : Attribute
failureStyle = 
  style [ ("backgroundColor", "lightCoral") 
        , ("fontFamily", "monospace")
        , ("marginBottom", "5px")
        ]
        
italic : Attribute
italic = style [("fontStyle", "italic")]

bold : Attribute
bold = style [("fontWeight", "bold")]


checked : Assert String
checked = Assert "ok" "bla"

doCheck : Assert x -> TestResult x
doCheck assert =
  if assert.expected == assert.actual
  then Success
  else Failure assert.actual

renderTest : String -> Assert x -> Html
renderTest name assertion =
  let result = doCheck assertion
  in
    case result of
      Success -> 
        div [successStyle] [text (name ++ ": SUCCESS")]
      Failure x ->
        div [failureStyle] 
          [ div [bold] [text (name ++ ": FAILURE")]
          , div [italic] [text ("expected : " ++ (toString assertion.expected))] 
          , div [italic] [text ("actual   : " ++ (toString assertion.actual))]
          ]

{-| Defines a test case
-}
test : String -> x -> x -> Html
test name expected actual =
  renderTest name <| Assert expected actual
  
{-| Groups together test cases
-}
suite : String -> List Html -> Html
suite name results =
  div [style [ ("border", "solid black 1px")
             , ("margin", "10px") 
             ]] 
    [ h2 [] [text ("Suite: " ++ name)]
    , div [] results
    ]

{-| Groups together test suites

-}
report : String -> List Html -> Html
report name suites =
  div []
    [ h1 [] [text name]
    , div [] suites
    ]
