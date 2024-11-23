module Shadcn.Switch
  ( OnCheckedChangeHandler
  , SwitchProps
  , switch
  ) where

import Prelude
import Effect.Uncurried (EffectFn1)
import Prim.Row (class Union)
import React.Basic.DOM (Props_input)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React

foreign import switchImpl :: forall a. ReactComponent { | a }

type OnCheckedChangeHandler
  = EffectFn1 Boolean Unit

type SwitchProps
  = ( onCheckedChange :: OnCheckedChangeHandler
    | Props_input
    )

switch :: forall attrs attrs_. Union attrs attrs_ SwitchProps => Record attrs -> JSX
switch props = React.element switchImpl props
