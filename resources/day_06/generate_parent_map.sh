#!/usr/bin/env bash

sort -t\) -k2 resources/day_06/input.txt | \
     awk -F\) 'BEGIN { printf "{" } ; END { print "}" } ; { print "\"" $2 "\" \"" $1 "\""}'
