{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CheckCheck.Contracts.Budget
  ( BudgetAPI
  , GetBudget
  , ApplyBudgetDelta
  , BudgetResp(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Servant.API (ReqBody, JSON, (:>), Patch, (:<|>), Get)

type BudgetAPI = GetBudget :<|> ApplyBudgetDelta

type GetBudget = Get '[JSON] BudgetResp
type ApplyBudgetDelta = ReqBody '[JSON] Integer :> Patch '[JSON] BudgetResp

newtype BudgetResp = BudgetResp
  { amount :: Integer }
  deriving (Generic, FromJSON, ToJSON)

