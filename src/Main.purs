module Main (main) where

import Prelude
import App as App
import App.Config (devConfig)
import App.Router as AppRouter
import App.Shadcn.Shadcn as Shadcn
import Control.Monad.Reader.Trans (runReaderT)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM.Client (createRoot, renderRoot)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  -- React
  maybeRoot <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case maybeRoot of
    Nothing -> throw "Root element not found."
    Just r -> do
      router <- AppRouter.mkRouter
      app <- runReaderT App.mkApp devConfig
      root <- createRoot r
      -- Shadcn/ui ToastのToasterコンポーネントをid=root要素の最後に配置する
      renderRoot root (router [ app unit, Shadcn.toaster ])
