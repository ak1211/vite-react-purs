module App.Header (mkHeader) where

import Prelude
import App.Config (Config)
import App.Router (Router, routerContext)
import App.Routes (Page(..), Route(..))
import App.SqliteDatabaseState (SqliteDatabaseStateHook, closeSqliteDatabase, openSqliteDatabase)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Effect.Exception (Error)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetFiles)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (type (/\), (/\), Component, JSX, component, useContext, useState)
import React.Basic.Hooks as React
import Shadcn.Components as Shadcn
import Sqlite3Wasm.Sqlite3Wasm as Sq3
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList (FileList)
import Web.File.FileList as FileList
import Web.File.FileReader.Aff (readAsArrayBuffer)

type HeaderProps
  = { config :: Config
    , sqliteDatabaseStateHook :: SqliteDatabaseStateHook
    }

type State
  = { dialogOpened :: Boolean
    }

initialState :: State
initialState = { dialogOpened: false }

type StateHook
  = (State /\ ((State -> State) -> Effect Unit))

mkHeader :: Component HeaderProps
mkHeader = do
  component "Header" \(props :: HeaderProps) -> React.do
    -- useStateフックでStateを管理する
    (stateHook :: StateHook) <- useState initialState
    -- Shadcn/uiのToast
    (toast :: Shadcn.Toast) <- React.unsafeRenderEffect Shadcn.useToast
    router <- useContext routerContext
    pure
      $ DOM.header
          { className: "sticky top-0 h-appHeader py-2 flex flex-column justify-between px-5 py-1 border-b border-slate-900/20"
          , children:
              [ Shadcn.navigationMenu
                  { className: "" }
                  [ Shadcn.navigationMenuList_
                      [ DOM.div
                          { className: "mr-5 font-serif font-extrabold text-lg text-slate-900"
                          , children:
                              [ DOM.text "計測データ分析" ]
                          }
                      , navItem router Home "HOME"
                      , navItem router About "ABOUT"
                      , navItem router Charts "CHARTS"
                      ]
                  ]
              , DOM.div
                  { className: "border-l border-slate-900"
                  , children:
                      [ DOM.div
                          { className: "m-1 flex items-center space-x-2"
                          , children:
                              [ uploadDatabaseFileDialog toast stateHook props.config.sqliteDatabaseFile ]
                                <> switchItem props.sqliteDatabaseStateHook props.config.sqliteDatabaseFile
                          }
                      ]
                  }
              ]
          }

navItem :: Router -> Page -> String -> JSX
navItem router page text =
  Shadcn.navigationMenuItem
    {}
    [ Shadcn.button
        { onClick: handler_ $ router.navigate page
        , variant: "link"
        }
        [ DOM.span
            { className: "mx-2" <> if router.route == Page page then " font-bold border-b-2 border-slate-900" else ""
            , children: [ DOM.text text ]
            }
        ]
    ]

switchItem :: SqliteDatabaseStateHook -> Sq3.OpfsDatabaseFilePath -> Array JSX
switchItem stateHook@(state /\ _setState) sqliteDatabaseFile =
  [ Shadcn.label { className: "font-bold", htmlFor: id_ }
      [ DOM.text "Open Database" ]
  , Shadcn.switch
      { id: id_
      , checked: Maybe.isJust state.maybeDbId
      , onCheckedChange: mkEffectFn1 checkedChangeHandler
      }
  ]
  where
  id_ = "db-open-switch"

  checkedChangeHandler :: Boolean -> Effect Unit
  checkedChangeHandler checked = case checked of
    true -> openSqliteDatabase stateHook sqliteDatabaseFile
    false -> closeSqliteDatabase stateHook

uploadDatabaseFileDialog :: Shadcn.Toast -> StateHook -> Sq3.OpfsDatabaseFilePath -> JSX
uploadDatabaseFileDialog toast stateHook@(state /\ setState) sqliteDatabaseFile =
  Shadcn.dialog
    { open: state.dialogOpened
    , onOpenChange: mkEffectFn1 openChangeHandler
    }
    [ Shadcn.dialogTrigger
        { asChild: true }
        [ Shadcn.button
            { className: "mx-2"
            , variant: ""
            }
            [ DOM.text "Upload Data" ]
        ]
    , Shadcn.dialogContent_
        [ Shadcn.dialogHeader_
            [ Shadcn.dialogTitle_ [ DOM.text "SQLite3 データーベースファイルアップロード" ]
            , Shadcn.dialogDescription_ [ DOM.text "指定のSQLite3データーベースファイルをOrigin Private File System(OPFS)領域にアップロードします。" ]
            ]
        , DOM.div
            { className: ""
            , children:
                [ Shadcn.label { htmlFor: "databaseFile" } [ DOM.text "環境データを蓄積したSQLite3データーベースファイルを選択してください。" ]
                , Shadcn.input
                    { id: "databaseFile"
                    , type: "file"
                    , onChange: handler targetFiles (loadFileHandler toast stateHook sqliteDatabaseFile)
                    }
                ]
            }
        , Shadcn.dialogFooter_
            [ Shadcn.dialogClose
                { asChild: true }
                [ Shadcn.button {} [ DOM.text "Close" ] ]
            ]
        ]
    ]
  where
  openChangeHandler :: Boolean -> Effect Unit
  openChangeHandler open = setState _ { dialogOpened = open }

-- OPFS上のデーターベースファイルにローカルファイルを上書きする
loadFileHandler :: Shadcn.Toast -> StateHook -> Sq3.OpfsDatabaseFilePath -> Maybe FileList -> Effect Unit
loadFileHandler _toast _st _filepath Nothing = mempty

loadFileHandler toast (_ /\ setState) opfsDatabaseFileToUse (Just filelist) =
  maybe (Console.error "FileList is empty") go
    $ FileList.item 0 filelist
  where
  go :: File -> Effect Unit
  go file = Aff.runAff_ (either fail $ const success) $ overwriteOpfsFile file

  overwriteOpfsFile :: File -> Aff Unit
  overwriteOpfsFile file = do
    ab <- readAsArrayBuffer (File.toBlob file)
    Sq3.overwriteOpfsFileWithSpecifiedArrayBuffer opfsDatabaseFileToUse ab

  fail :: Error -> Effect Unit
  fail err =
    Shadcn.toast toast
      { variant: "destructive"
      , title: "アップロード失敗"
      , description: Aff.message err
      }

  success :: Effect Unit
  success = do
    --
    Shadcn.toast toast
      { variant: ""
      , title: "アップロード成功"
      , description: "データーベースファイルをOrigin Private File Systemに書き込みました。"
      }
    -- ダイアログを閉じる
    setState _ { dialogOpened = false }
