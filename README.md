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
- [X] Ready for general use.

## Running locally

1. Install [obelisk](https://github.com/obsidiansystems/obelisk)
1. Set`config/backend/slackExportPath` to where you downloaded your Slack export
   zip.
1. Run `ob run` and visit http://localhost:8080

## Running production app

To deploy the app either locally or elsewhere follow these instructions. In
future we could automate all of this using Nix. Alternatively, if the machine
you are deploying to is reserved _exclusively_ for running Taut, you may use
[`ob deploy`](https://github.com/obsidiansystems/obelisk#deploying).

```bash
# Create a directory to hold deployment configuration.
mkdir -p deploy/prod
cp -r config deploy/prod/ 

# Create a Slack OAuth app, and add its keys here:
pushd deploy/prod 
echo "..." > config/backend/oauthClientID
echo "..." > config/backend/oauthClientSecret
# As a Slack admin download a copy of your Slack export data. 
# It should be a zip file. Add its path here.
echo "..." > config/backend/slackExportPath
popd

# When ready to create a new deployment:
# Do a full build of the app, and copy the binaries to deploy directory
nix-build -A exe  # This creates ./result 
rm -f deploy/prod/*  # This would leave the "config" directory as is
cp -r result/* deploy/prod/

# Run the app
./backend --port 9000

# Visit http://localhost:9000 for profit!
```
