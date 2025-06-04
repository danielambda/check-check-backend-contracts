{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CheckCheck.Contracts.API (API) where

import Servant.API ((:<|>)((:<|>)), (:>))

import CheckCheck.Contracts.Budget (BudgetAPI)
import CheckCheck.Contracts.Requests (RequestsAPI)

type API
  =    "requests" :> RequestsAPI
  :<|> "budget" :> BudgetAPI


