# Taut

An ambitious [Slack export data](https://get.slack.help/hc/en-us/articles/201658943-Export-your-workspace-data) viewer.

Taut is currently being used to access and search about 50,000 messages in a
Slack community I help manage.

## Features

- [X] Read directly from the Zip archive that Slack provides
- [X] Full access to view all messages and search them by user, channel, date or
      keywords.
- [X] Authorization using Slack login (only the organization's Slack users can
      access Taut).
- [ ] Ready for general use (not yet).

## Running

1. Install [obelisk](https://github.com/obsidiansystems/obelisk)
1. Appropriately configure the app by changing `config/backend/*`
  - You will need your Slack OAuth keys
  - And specify the path to your Slack export zip file.
1. Clone this repo, and run `ob run`
