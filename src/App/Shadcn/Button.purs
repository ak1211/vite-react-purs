module App.Shadcn.Button
  ( ButtonProps
  , button
  ) where

import Prim.Row (class Union)
import React.Basic.DOM (Props_button)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React

type ButtonProps
  = ( variant :: String
    , size :: String
    , asChild :: Boolean
    | Props_button
    )

foreign import buttonImpl :: forall a. ReactComponent { | a }

button :: forall attrs attrs_. Union attrs attrs_ ButtonProps => Record attrs -> JSX
button props = React.element buttonImpl props
