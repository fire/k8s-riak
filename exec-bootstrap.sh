#!/usr/bin/env bash
set -x #echo on

STACK="stack --stack-yaml /home/jloos/stack.yaml"

# kubectl exec $1 -- bash -x -c "${STACK} setup && ${STACK} build && ${STACK} install && ${STACK} exec k8s-riak-bootstrapper"
kubectl exec $1 -- bash -x -c "/home/jloos/src/Main.hs"
