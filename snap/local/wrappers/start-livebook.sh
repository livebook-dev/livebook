#!/bin/bash

LIVEBOOK_PORT="$(snapctl get port)" \
LIVEBOOK_IP="$(snapctl get ip)" \
exec $SNAP/livebook/bin/livebook start
