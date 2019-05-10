module Common.Types where

import Data.Pagination

import Common.Slack.Types
import Common.Slack.Types.Auth

type MessagesResponse = Either NotAuthorized (SlackUser, Paginated Message)
