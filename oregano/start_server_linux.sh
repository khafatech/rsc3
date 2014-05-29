#!/bin/bash


jack_control  start
scsynth -u 57110 &
sleep 1
# after scsynth starts:
jack_connect -s default SuperCollider:out_1 system:playback_1
jack_connect -s default SuperCollider:out_2 system:playback_2


