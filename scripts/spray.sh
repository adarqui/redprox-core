#!/bin/bash

while true; do
    redis-cli rpush proxy1 $RANDOM
    redis-cli publish psub:$RANDOM $RANDOM
done
