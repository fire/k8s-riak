# Riak with Kubernetes

Bits and pieces taken from [deis/riak](deis/riak) and modified to build a riak cluster via Kubernetes' replication controller and use scripts inspired by [hectcastro/docker-riak](hectcastro/docker-riak) for auto-clustering. Use it as a base for your cluster.

## TODO

- [ ] Define k8s services
- [ ] Fix & reactivate sed ${RIAK_BACKEND}
- [ ] Graceful leave cluster on size down
- [ ] Test cluster
