module Common.Types where

import Data.Text (Text)

import Data.Pagination

import Common.Slack.Types
import Common.Slack.Types.Auth
import Common.Slack.Types.Search

type MessagesResponse =
  Either
    NotAuthorized
    (SlackUser, Either () (MessageFilters, Paginated Message))

type ExamplesResponse =
  Either
    NotAuthorized
    (SlackUser, [(Text, Text)])
