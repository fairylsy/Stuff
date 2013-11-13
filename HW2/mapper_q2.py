#!/usr/bin/env python
# You need to change the name of file name to 'mapper.py' to run on Hadoop.
import sys
import math
# Actually does nothing, just ...

# input comes from STDIN (standard input)
for line in sys.stdin:
    # split the line into (group, value)
    line = line.strip() # Need to remove trailing '\n'
    entries = line.split('\t')
    temp = ""
    count = 0
    for entry in entries:
        num_floor = math.floor(10*float(entry))/10
        if (count == 0):
             temp = temp + str(num_floor) + ',' + str( num_floor+0.1) + ','
             count = 1
        else:
             temp = temp + str(num_floor) + ',' + str(num_floor+0.1)
    temp = temp + '\t' + '1'
    print temp

