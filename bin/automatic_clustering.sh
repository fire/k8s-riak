#!/usr/bin/env bash
set -x #echo on

env

if [[ ${RIAK_AUTOMATIC_DISCOVERY} = true ]]; then
  # Join node to the cluster
  sleep 5
  if [[ -n ${RIAK_DISCOVERY_PORT_8098_TCP_ADDR} ]]; then
    riak-admin cluster join "riak@${RIAK_DISCOVERY_PORT_8098_TCP_ADDR}"
    sleep 10l
    riak-admin cluster plan && riak-admin cluster commit
  else
    echo "Unable to join cluster, missing riak-discovery service"
    sleep 60
    exit 1
  fi
fi
