#!/usr/bin/env bash
source "$HOME/.bash_profile"; # NOTE: Some editors need help finding 'ob' and this assumes that `ob` is made available on your `$PATH` in `.bash_profile`
ob internal export-ghci-configuration > "$HIE_BIOS_OUTPUT"
