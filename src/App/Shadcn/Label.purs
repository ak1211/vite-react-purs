module App.Shadcn.Label (label) where

import Prelude
import Prim.Row (class Union)
import React.Basic.DOM (Props_label)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import labelImpl :: forall a. ReactComponent { | a }

label :: forall attrs attrs_. Union attrs attrs_ Props_label => Record attrs -> Array JSX -> JSX
label props children =
  React.element labelImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_label)
