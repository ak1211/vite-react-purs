module App.Shadcn.Button
  ( ButtonProps
  , button
  ) where

import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React

type ButtonProps
  = { onClick :: EventHandler
    , children :: Array JSX
    , className :: String
    }

foreign import buttonImpl :: forall a. ReactComponent { | a }

button :: ButtonProps -> JSX
button props = React.element buttonImpl props
