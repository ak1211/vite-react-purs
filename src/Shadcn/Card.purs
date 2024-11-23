module Shadcn.Card
  ( card
  , cardContent
  , cardContent_
  , cardDescription
  , cardDescription_
  , cardFooter
  , cardFooter_
  , cardHeader
  , cardHeader_
  , cardTitle
  , cardTitle_
  , card_
  ) where

import Prelude
import Prim.Row (class Union)
import React.Basic.DOM (Props_div)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import cardImpl :: forall a. ReactComponent { | a }

foreign import cardContentImpl :: forall a. ReactComponent { | a }

foreign import cardDescriptionImpl :: forall a. ReactComponent { | a }

foreign import cardFooterImpl :: forall a. ReactComponent { | a }

foreign import cardHeaderImpl :: forall a. ReactComponent { | a }

foreign import cardTitleImpl :: forall a. ReactComponent { | a }

card :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
card props children =
  React.element cardImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

card_ :: Array JSX -> JSX
card_ children = React.element cardImpl { children }

cardContent :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
cardContent props children =
  React.element cardContentImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

cardContent_ :: Array JSX -> JSX
cardContent_ children = React.element cardContentImpl { children }

cardDescription :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
cardDescription props children =
  React.element cardDescriptionImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

cardDescription_ :: Array JSX -> JSX
cardDescription_ children = React.element cardDescriptionImpl { children }

cardFooter :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
cardFooter props children =
  React.element cardFooterImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

cardFooter_ :: Array JSX -> JSX
cardFooter_ children = React.element cardFooterImpl { children }

cardHeader :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
cardHeader props children =
  React.element cardHeaderImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

cardHeader_ :: Array JSX -> JSX
cardHeader_ children = React.element cardHeaderImpl { children }

cardTitle :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
cardTitle props children =
  React.element cardTitleImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

cardTitle_ :: Array JSX -> JSX
cardTitle_ children = React.element cardTitleImpl { children }
