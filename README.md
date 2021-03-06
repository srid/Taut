# Taut

An ambitious [Slack export
data](https://get.slack.help/hc/en-us/articles/201658943-Export-your-workspace-data)
viewer written in Haskell.

Taut was used to access and search over 50,000 messages in a Slack community I helped manage (we have migrated to Zulip), and is now ready for general use.

## Features

- Read directly from the Slack export data zip archive
- Access _all_ messages, and search them as you would on Slack.
- Every message comes with its own permalink
- Only accessible to the organization's members (uses Slack's login)

## Running locally

1. Install [obelisk](https://github.com/obsidiansystems/obelisk)
1. Set`config/backend/slackExportPath` to where you downloaded your Slack export
   zip.
1. Run `ob run` and visit http://127.0.0.1:8000

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
