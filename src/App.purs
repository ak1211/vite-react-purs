module App where

import Prelude hiding ((/))
import App.Pages.About as About
import App.Pages.Charts as Charts
import App.Pages.Home as Home
import App.Router as AppRouter
import App.Routes (Page(..), Route(..))
import App.Shadcn.Button as ShadcnButton
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React

mkApp :: Effect (Unit -> JSX)
mkApp = do
  home <- Home.mkHome
  about <- About.mkAbout
  charts <- Charts.mkCharts
  React.component "App" \_ -> React.do
    router <- AppRouter.useRouter
    pure
      $ React.fragment
          [ R.div_
              [ R.text "Menu: "
              , R.ul_
                  [ ShadcnButton.button
                      { onClick: handler_ $ router.navigate Home
                      , className: "m-1"
                      , children: [ R.text "Go to Home page" ]
                      }
                  , ShadcnButton.button
                      { onClick: handler_ $ router.navigate About
                      , className: "m-1"
                      , children: [ R.text "Go to About page" ]
                      }
                  , ShadcnButton.button
                      { onClick: handler_ $ router.navigate Charts
                      , className: "m-1"
                      , children: [ R.text "Go to Charts page" ]
                      }
                  ]
              ]
          , case router.route of
              Page page -> case page of
                Home -> home unit
                About -> about unit
                Charts -> charts unit
              NotFound -> React.fragment []
          ]
