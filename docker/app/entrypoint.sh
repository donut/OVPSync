#!/bin/sh

# Wait until MariaDB is up and listening to connections.
# Adapted from https://github.com/docker/compose/issues/374#issuecomment-310266246

echo "Checking that MariaDB is up on db service."
until $(nc -zv db 3306); do
  echo "...MariaDB not available yet."
  sleep 5
done

echo "Looks like MariaDB is up. Starting app..."
/app/bin/main.exe $@