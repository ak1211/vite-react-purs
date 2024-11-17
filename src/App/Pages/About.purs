module App.Pages.About
  ( AboutProps
  , mkAbout
  ) where

import Prelude
import App.Config (Config)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type AboutProps
  = { config :: Config }

mkAbout :: Component AboutProps
mkAbout =
  component "About" \(_props :: AboutProps) -> React.do
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "About" ]
              , DOM.p_ [ DOM.text "ABOUTページになるはずのページ" ]
              ]
          }
