#!/bin/bash

QUEUES="proxy1 proxy2"

while true; do
    for q in $QUEUES; do redis-cli rpush $q $RANDOM; done
    for q in $QUEUES; do redis-cli lpush $q $RANDOM; done
    redis-cli publish sub $RANDOM
    redis-cli publish "psub:$RANDOM" $RANDOM
    sleep 1
done
