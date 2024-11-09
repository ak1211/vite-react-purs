module App.Pages.About
  ( AboutProps
  , mkAbout
  ) where

import Prelude
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

type AboutProps
  = Unit

mkAbout :: Component AboutProps
mkAbout = do
  component "About" \_props -> React.do
    pure
      $ DOM.div
          { children:
              [ DOM.h1_ [ DOM.text "About" ]
              , DOM.p_ [ DOM.text "ABOUTページになるはずのページ" ]
              ]
          }
