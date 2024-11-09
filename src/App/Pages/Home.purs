module App.Pages.Home
  ( HomeProps
  , mkHome
  ) where

import Prelude
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type HomeProps
  = Unit

mkHome :: Component HomeProps
mkHome = do
  component "Home" \_props -> React.do
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Home" ]
              , DOM.p_ [ DOM.text "ダッシュボードになるはずのページ" ]
              ]
          }
