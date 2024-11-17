module App.Shadcn.Dialog
  ( DialogProps
  , OnOpenChangeHandler
  , dialog
  , dialogClose
  , dialogClose_
  , dialogContent_
  , dialogDescription_
  , dialogFooter_
  , dialogHeader_
  , dialogTitle_
  , dialogTrigger
  , dialogTrigger_
  , dialog_
  ) where

import Prelude
import Effect.Uncurried (EffectFn1)
import Prim.Row (class Union)
import React.Basic.DOM (Props_div)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

type OnOpenChangeHandler
  = EffectFn1 Boolean Unit

type DialogProps
  = ( defaultOpen :: Boolean
    , open :: Boolean
    , onOpenChange :: OnOpenChangeHandler
    , modal :: Boolean
    | Props_div
    )

type Props_with_asChild
  = ( asChild :: Boolean
    | Props_div
    )

foreign import dialogImpl :: forall a. ReactComponent { | a }

foreign import dialogCloseImpl :: forall a. ReactComponent { | a }

foreign import dialogContentImpl :: forall a. ReactComponent { | a }

foreign import dialogDescriptionImpl :: forall a. ReactComponent { | a }

foreign import dialogFooterImpl :: forall a. ReactComponent { | a }

foreign import dialogHeaderImpl :: forall a. ReactComponent { | a }

foreign import dialogTitleImpl :: forall a. ReactComponent { | a }

foreign import dialogTriggerImpl :: forall a. ReactComponent { | a }

dialog :: forall attrs attrs_. Union attrs attrs_ DialogProps => Record attrs -> Array JSX -> JSX
dialog props children =
  React.element dialogImpl
    $ Record.merge { children } (unsafeCoerce props :: Record DialogProps)

dialog_ :: Array JSX -> JSX
dialog_ children = React.element dialogImpl { children }

dialogClose :: forall attrs attrs_. Union attrs attrs_ Props_with_asChild => Record attrs -> Array JSX -> JSX
dialogClose props children =
  React.element dialogCloseImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_with_asChild)

dialogClose_ :: Array JSX -> JSX
dialogClose_ children = React.element dialogCloseImpl { children }

dialogContent_ :: Array JSX -> JSX
dialogContent_ children = React.element dialogContentImpl { children }

dialogDescription_ :: Array JSX -> JSX
dialogDescription_ children = React.element dialogDescriptionImpl { children }

dialogFooter_ :: Array JSX -> JSX
dialogFooter_ children = React.element dialogFooterImpl { children }

dialogHeader_ :: Array JSX -> JSX
dialogHeader_ children = React.element dialogHeaderImpl { children }

dialogTitle_ :: Array JSX -> JSX
dialogTitle_ children = React.element dialogTitleImpl { children }

dialogTrigger :: forall attrs attrs_. Union attrs attrs_ Props_with_asChild => Record attrs -> Array JSX -> JSX
dialogTrigger props children =
  React.element dialogTriggerImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_with_asChild)

dialogTrigger_ :: Array JSX -> JSX
dialogTrigger_ children = React.element dialogTriggerImpl { children }
