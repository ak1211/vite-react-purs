module App.Shadcn.NavigationMenu
  ( navigationMenu
  , navigationMenuContent_
  , navigationMenuIndicator_
  , navigationMenuItem
  , navigationMenuItem_
  , navigationMenuLink_
  , navigationMenuList
  , navigationMenuList_
  , navigationMenuTrigger_
  , navigationMenu_
  ) where

import Prelude
import Prim.Row (class Union)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React
import React.Basic.DOM (Props_div, Props_nav)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

foreign import navigationMenuImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuContentImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuIndicatorImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuItemImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuLinkImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuListImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuTriggerImpl :: forall a. ReactComponent { | a }

foreign import navigationMenuViewportImpl :: forall a. ReactComponent { | a }

navigationMenu :: forall attrs attrs_. Union attrs attrs_ Props_nav => Record attrs -> Array JSX -> JSX
navigationMenu props children =
  React.element navigationMenuImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_nav)

navigationMenu_ :: Array JSX -> JSX
navigationMenu_ children = React.element navigationMenuImpl { children }

navigationMenuContent_ :: Array JSX -> JSX
navigationMenuContent_ children = React.element navigationMenuContentImpl { children }

navigationMenuIndicator_ :: Array JSX -> JSX
navigationMenuIndicator_ children = React.element navigationMenuIndicatorImpl { children }

navigationMenuItem_ :: Array JSX -> JSX
navigationMenuItem_ children = React.element navigationMenuItemImpl { children }

navigationMenuItem :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
navigationMenuItem props children =
  React.element navigationMenuItemImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

navigationMenuLink_ :: Array JSX -> JSX
navigationMenuLink_ children = React.element navigationMenuLinkImpl { children }

navigationMenuList :: forall attrs attrs_. Union attrs attrs_ Props_div => Record attrs -> Array JSX -> JSX
navigationMenuList props children =
  React.element navigationMenuListImpl
    $ Record.merge { children } (unsafeCoerce props :: Record Props_div)

navigationMenuList_ :: Array JSX -> JSX
navigationMenuList_ children = React.element navigationMenuListImpl { children }

navigationMenuTrigger_ :: Array JSX -> JSX
navigationMenuTrigger_ children = React.element navigationMenuTriggerImpl { children }
