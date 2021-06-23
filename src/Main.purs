module Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), intercalate, (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Console (log)

type State = { selectedUsers :: List { user :: { username :: String } }, status :: List Int }
type Attempts = List { response :: Either String { status :: Int }, who :: String }

reducer :: State -> Attempts -> State
reducer state attempts = 
  state 
    { status = case successStatus of 
        Nothing -> failureStatus
        Just success -> success : failureStatus
    , selectedUsers = state.selectedUsers # List.filter (\{user: {username}} -> (failedAttempts <#> _.who) # List.elem username)
    }

  where
    { yes: successfulAttempts, no: failedAttempts } = List.partition wasSuccessful attempts

    wasSuccessful {response: Left _err} = false
    wasSuccessful {response: Right {status}}
      | 200 <= status && status < 300 = true
      | otherwise = false
    
    successStatus = 
      case intercalate ", " (successfulAttempts <#> _.who) of
        "" -> Nothing
        _ -> Just 1

    failureStatus = 
      case intercalate ", " (failedAttempts <#> _.who) of 
        "" -> Nil
        _ -> (1 : Nil) 


main :: Effect Unit
main = do
  log "🍝"
