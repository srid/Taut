module Common.Types where

import Common.Slack.Types
import Common.Slack.Types.Auth
import Common.Slack.Types.Search
import Data.Pagination
import Data.Text (Text)

type MessagesResponse =
  Either
    NotAuthorized
    (SlackUser, Either () (MessageFilters, Paginated Message))

type ExamplesResponse =
  Either
    NotAuthorized
    (SlackUser, [(Text, Text)])
