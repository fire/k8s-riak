#!/usr/bin/env bash
env

IP_ADDRESS=$(hostname -I)

# Ensure correct ownership and permissions on volumes
chown riak:riak /var/lib/riak /var/log/riak
chmod 755 /var/lib/riak /var/log/riak

# Open file descriptor limit
ulimit -n 4096

# Ensure the Erlang node name is set correctly
sed -i.bak "s/riak@127.0.0.1/riak@${IP_ADDRESS}/" /etc/riak/riak.conf

# Ensure the desired Riak backend is set correctly
sed -i.bak "s/storage_backend = \(.*\)/storage_backend = ${RIAK_BACKEND}/" /etc/riak/riak.conf

# Enable riak control
sed -i.bak "s/riak_control = \(.*\)/riak_control = on/" /etc/riak/riak.conf

# Delete old ring infos
rm -rf /var/lib/riak/ring

# Start Riak
exec /sbin/setuser riak "$(ls -d /usr/lib/riak/erts*)/bin/run_erl" "/tmp/riak" \
   "/var/log/riak" "exec /usr/sbin/riak console"
