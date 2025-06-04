{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CheckCheck.Contracts.Requests
  ( RequestsAPI
  , PostRequest
  , GetRequests
  , CompleteRequest
  , RequestResp(..)
  , RequestItemResp(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Servant.API ((:<|>), Get, JSON, Capture, ReqBody, (:>), Put, Post)

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)

import CheckCheck.Contracts.Budget (BudgetResp)

type RequestsAPI
  =    PostRequest
  :<|> GetRequests
  :<|> CompleteRequest

type PostRequest = ReqBody '[JSON] PostRequestReqBody :> Post '[JSON] RequestResp
type GetRequests = Get '[JSON] [RequestResp]
type CompleteRequest = Capture "requestId" UUID :> "complete" :> Post '[JSON] BudgetResp

data RequestItemReqBody = RequestItemReqBody
  { name :: Text
  , price :: Integer
  } deriving (Generic, ToJSON, FromJSON)

newtype PostRequestReqBody = PostRequestReqBody (NonEmpty RequestItemReqBody)
  deriving (Generic, ToJSON, FromJSON)

data RequestResp = RequestResp
  { requestId :: UUID
  , items :: NonEmpty RequestItemResp
  , createdAt :: UTCTime
  , isPending :: Bool
  } deriving (Generic, ToJSON, FromJSON)

data RequestItemResp = RequestItemResp
  { name :: Text
  , price :: Integer
  } deriving (Generic, ToJSON, FromJSON)

