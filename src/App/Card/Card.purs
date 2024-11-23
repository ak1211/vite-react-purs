module App.Card.Card
  ( CardContents
  , card
  ) where

import Prelude
import Shadcn.Components as Shadcn
import React.Basic.DOM as DOM
import React.Basic.Hooks (JSX)

type CardContents
  = { className :: String
    , title :: String
    , description :: String
    , children :: Array JSX
    , footer :: String
    }

card :: CardContents -> JSX
card contents =
  Shadcn.card { className: "drop-shadow-md " <> contents.className }
    [ Shadcn.cardHeader_
        [ Shadcn.cardTitle_ [ DOM.text contents.title ]
        , Shadcn.cardDescription_ [ DOM.text contents.description ]
        ]
    , Shadcn.cardContent_ contents.children
    , Shadcn.cardFooter_ [ DOM.text contents.footer ]
    ]
