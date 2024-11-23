module Result3 (Result3(..)) where

-- 初期値付きのResult型
data Result3 e a
  = Initial
  | Err e
  | Ok a
