module Shadcn.Input
  ( input
  ) where

import Prim.Row (class Union)
import React.Basic.DOM (Props_input)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React

foreign import inputImpl :: forall a. ReactComponent { | a }

input :: forall attrs attrs_. Union attrs attrs_ Props_input => Record attrs -> JSX
input props = React.element inputImpl props
