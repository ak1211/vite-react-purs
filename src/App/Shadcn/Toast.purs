module App.Shadcn.Toast
  ( Toast
  , ToastOption
  , toaster
  , useToast
  , toast
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React

foreign import toasterImpl :: ReactComponent {}

toaster :: JSX
toaster = React.element toasterImpl {}

foreign import useToastImpl :: Effect Toast

useToast :: Effect Toast
useToast = useToastImpl

type Toast
  = EffectFn1 ToastOption Unit

type ToastOption
  = { variant :: String
    , title :: String
    , description :: String
    }

toast :: Toast -> ToastOption -> Effect Unit
toast toastImpl toastOption = runEffectFn1 toastImpl toastOption
