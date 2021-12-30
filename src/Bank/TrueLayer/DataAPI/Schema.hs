{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DuplicateRecordFields #-}
module Bank.TrueLayer.DataAPI.Schema
  ( Accounts(..)
  , AccountId(..)
  , Cards(..)
  , Card(..)
  , CardBalances(..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON(..))
import Data.Time.LocalTime (ZonedTime)
import Data.Time.Calendar (Day)

import GHC.Generics (Generic)


newtype Accounts = Accounts
  { results :: [Account]
  } deriving (Generic, Show, ToJSON, FromJSON)

data Account = Account
  { update_timestamp :: ZonedTime
  , account_id :: Maybe AccountId
  , account_type :: Maybe String
  , display_name :: Maybe String
  , currency :: Maybe String
  , accountNumber :: AccountNumber
  , provider :: Provider
  } deriving (Generic, Show, ToJSON, FromJSON)

newtype AccountId = AccountId String deriving (Generic, Show, ToJSON, FromJSON)

data AccountNumber = AccountNumber
  { number :: Maybe String
  , sort_code :: Maybe String
  , swift_bic :: Maybe String
  , iban :: Maybe String
  , routing_number :: Maybe String
  } deriving (Generic, Show, ToJSON, FromJSON)

data Card = Card
  { account_id :: AccountId
  , card_network :: String
  , card_type :: String
  , currency :: String
  , display_name :: String
  , partial_card_number :: String
  , name_on_card :: String
  , valid_from :: Maybe String
  , valid_to :: Maybe String
  , update_timestamp :: ZonedTime
  , provider :: Provider
  } deriving (Generic, Show, ToJSON, FromJSON)

newtype Cards = Cards
  { results :: [Card]
  } deriving (Generic, Show, ToJSON, FromJSON)


newtype CardBalances = CardBalances
  { results :: [CardBalance]
  } deriving (Generic, Show, ToJSON, FromJSON)

data CardBalance = CardBalance
  { available :: Double
  , currency :: String
  , current :: Double
  , credit_limit :: Double
  , last_statement_balance :: Maybe Double
  , last_statement_date :: Maybe Day
  , payment_due :: Maybe Double
  , payment_due_date :: Maybe Day
  , update_timestamp :: ZonedTime
  } deriving (Generic, Show, ToJSON, FromJSON)


newtype Provider = Provider
  { provider_id :: String
  } deriving (Generic, Show, ToJSON, FromJSON)
