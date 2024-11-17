module App.Pages.Home
  ( HomeProps
  , mkHome
  ) where

import Prelude
import App.Config (Config)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type HomeProps
  = { config :: Config }

mkHome :: Component HomeProps
mkHome = do
  component "Home" \(_props :: HomeProps) -> do
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "Home" ]
              , DOM.p_ [ DOM.text "ダッシュボードになるはずのページ" ]
              ]
          }
