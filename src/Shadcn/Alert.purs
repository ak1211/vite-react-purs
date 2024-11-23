module Shadcn.Alert
  ( alert
  , alertDescription
  , alertDescription_
  , alertTitle_
  , alert_
  ) where

import Prelude
import Prim.Row (class Union)
import React.Basic.DOM (Props_div)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import alertImpl :: forall a. ReactComponent { | a }

foreign import alertDescriptionImpl :: forall a. ReactComponent { | a }

foreign import alertTitleImpl :: forall a. ReactComponent { | a }

type AlertProps
  = ( variant :: String
    | Props_div
    )

alert :: forall attrs attrs_. Union attrs attrs_ AlertProps => Record attrs -> Array JSX -> JSX
alert props children =
  React.element alertImpl
    $ Record.merge { children } (unsafeCoerce props :: Record AlertProps)

alert_ :: Array JSX -> JSX
alert_ children = React.element alertImpl { children }

alertDescription :: forall attrs attrs_. Union attrs attrs_ AlertProps => Record attrs -> Array JSX -> JSX
alertDescription props children =
  React.element alertDescriptionImpl
    $ Record.merge { children } (unsafeCoerce props :: Record AlertProps)

alertDescription_ :: Array JSX -> JSX
alertDescription_ children = React.element alertDescriptionImpl { children }

alertTitle_ :: Array JSX -> JSX
alertTitle_ children = React.element alertTitleImpl { children }
