module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

add1 :: Int -> Int -> Maybe Int
add1 x y = Just (x + y)

main :: Effect Unit
main = do
  log "🍝"
  log $ show $ add1 5 7
