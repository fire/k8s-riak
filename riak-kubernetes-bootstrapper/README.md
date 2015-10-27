# riak-kubernetes-bootstrapper

Uses the kubernetes (1.0) api for auto joining of riak (2.0) nodes.

Basicly searches for endpoints for the given kubernetes service uses `riak-admin` to join the ring.

## Usage
