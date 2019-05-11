module Common.Types where

import Data.Pagination

import Common.Slack.Types
import Common.Slack.Types.Auth
import Common.Slack.Types.Search

type MessagesResponse =
  Either
    NotAuthorized
    (SlackUser, (MessageFilters, Paginated Message))
