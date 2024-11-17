module App.Shadcn.Button
  ( ButtonProps
  , button
  ) where

import Prelude
import Prim.Row (class Union)
import React.Basic.DOM (Props_button)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

type ButtonProps
  = ( variant :: String
    , size :: String
    , asChild :: Boolean
    | Props_button
    )

foreign import buttonImpl :: forall a. ReactComponent { | a }

button :: forall attrs attrs_. Union attrs attrs_ ButtonProps => Record attrs -> Array JSX -> JSX
button props children =
  React.element buttonImpl
    $ Record.merge { children } (unsafeCoerce props :: Record ButtonProps)
