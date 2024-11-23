module App.Card.SqliteWasm
  ( module App.Card.Card
  , sqliteWasmOverview
  ) where

import Prelude
import App.Card.Card (CardContents, card)
import Data.Either (either)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Exception (Error)
import React.Basic.DOM as DOM
import React.Basic.Hooks (type (/\), (/\), Component, JSX, component, useState, useEffect)
import React.Basic.Hooks as React
import Result3 (Result3(..))
import Sqlite3Wasm.Sqlite3Wasm as Sq3

type State
  = Result3 String Sq3.ConfigGetResult

type SetState
  = (State -> State) -> Effect Unit

type StateHook
  = (State /\ SetState)

sqliteWasmOverview :: Component Unit
sqliteWasmOverview = do
  component "SqliteInfoCard" \_ -> React.do
    state /\ setState <- useState Initial
    -- 副作用フック(useEffect)
    useEffect unit do
      getSqliteWasmVersion setState
      pure mempty
    pure $ card
      $ case state of
          Initial -> default { footer = "取得中..." }
          Err str -> default { children = [ DOM.text str ], footer = "読み取りに失敗しました。" }
          Ok verinfo ->
            default
              { children =
                map block
                  [ "ライブラリバージョン" /\ verinfo.version.libVersion
                  , "BigInt型" /\ if verinfo.bigIntEnabled then "有効" else "無効"
                  , "VFSリスト" /\ String.joinWith ", " verinfo.vfsList
                  ]
              , footer = "正常に動作しています。"
              }
  where
  default =
    { className: "col-span-2"
    , title: "SQLite WebAssembly"
    , description: "SQLite WebAssemblyライブラリ情報"
    , children: []
    , footer: []
    }

block :: String /\ String -> JSX
block (first /\ second) =
  DOM.div
    { className: "mx-auto grid grid-cols-2 gap-2"
    , children:
        [ DOM.p { className: "mr-2 my-auto text-sm", children: [ DOM.text first ] }
        , DOM.p { className: "ml-2 my-auto text-sm", children: [ DOM.text second ] }
        ]
    }

-- SQLite WASMバージョン情報を得る
getSqliteWasmVersion :: SetState -> Effect Unit
getSqliteWasmVersion setState = Aff.runAff_ (either fail success) $ configGet
  where
  configGet :: Aff Sq3.ConfigGetResult
  configGet = Sq3.configGet =<< Sq3.createWorker1Promiser

  fail :: Error -> Effect Unit
  fail err = setState (const $ Err $ Aff.message err)

  success :: Sq3.ConfigGetResult -> Effect Unit
  success r = setState (const $ Ok r)
