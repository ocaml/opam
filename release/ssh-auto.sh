#!/bin/sh

set -euo pipefail

password=$1
shift
retries=$1
shift
ssh_or_scp=$1
shift

set -- -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no "$@"

for i in $(seq 1 "$retries"); do
  sleep 1
  if sshpass "-p$password" "$ssh_or_scp" "$@"; then
    exit 0
  fi
done
exit 1
