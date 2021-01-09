#!/usr/bin/env bash

if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch)"
    export DBUS_SESSION_BUS_ADDRESS
fi

exec "$@"