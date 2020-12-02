#!/bin/bash

curl -s https://julekalender-backend.knowit.no/challenges/1/attachments/numbers.txt | tr ',' '\n' | sort -n | 
awk 'BEGIN { x=1 } ; { if($0 == x) { x++; } else { print x ; exit ; } }' 
