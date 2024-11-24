module App (mkApp) where

import Prelude
import App.Config (Config)
import App.Header as Header
import App.Pages.About as About
import App.Pages.Charts as Charts
import App.Pages.Home as Home
import App.Router as AppRouter
import App.Routes (Page(..), Route(..))
import App.SqliteDatabaseState (closeSqliteDatabase, initialSqliteDatabaseState)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Reader.Trans as MonadReader
import Effect (Effect)
import React.Basic.DOM as DOM
import React.Basic.Hooks ((/\), JSX, useEffect, useState)
import React.Basic.Hooks as React
import React.Basic.StrictMode (strictMode)

mkApp :: ReaderT Config Effect (Unit -> JSX)
mkApp = do
  (config :: Config) <- MonadReader.ask
  MonadReader.lift do
    header <- Header.mkHeader
    home <- Home.mkHome
    about <- About.mkAbout
    charts <- Charts.mkCharts
    React.component "App" \(_props :: Unit) -> React.do
      sqliteDatabaseStateHook@(sqliteDatabaseState /\ _) <- useState initialSqliteDatabaseState
      -- クリーンアップ関数を返却するのみ
      useEffect unit do pure $ closeSqliteDatabase sqliteDatabaseStateHook
      --
      router <- AppRouter.useRouter
      pure
        $ strictMode
        $ DOM.div
            { className: "flex flex-col h-screen"
            , children:
                [ header { config: config, sqliteDatabaseStateHook: sqliteDatabaseStateHook }
                , DOM.main
                    { className: "h-appMainContents bg-sky-200 flex-grow overflow-scroll"
                    , children:
                        [ case router.route of
                            Page page -> case page of
                              Home -> home { config: config, sqliteDatabaseState: sqliteDatabaseState }
                              About -> about { config: config }
                              Charts -> charts { config: config, sqliteDatabaseState: sqliteDatabaseState }
                            NotFound ->
                              DOM.div
                                { className: "w-full h-full flex justify-center items-center text-4xl"
                                , children: [ DOM.text "404 Not found" ]
                                }
                        ]
                    }
                ]
            }
