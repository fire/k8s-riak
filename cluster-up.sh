#!/usr/bin/env bash
set -x #echo on

BOOTSTRAPPER="/usr/sbin/k8s-riak-bootstrapper"

kubectl get pods $1 -l role=node -o template -t "{{range .items}}{{.metadata.name}} {{end}}" \
| xargs -t -n 1 -I {} kubectl exec {} -- $BOOTSTRAPPER
