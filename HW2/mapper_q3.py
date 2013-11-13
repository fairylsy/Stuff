#!/usr/bin/env python

import sys
import math
# Actually does nothing, just ...

# input comes from STDIN (standard input)
for line in sys.stdin:
    # split the line into (group, value)
    line = line.strip() # Need to remove trailing '\n'
    entries = line.split('\t')
    print entries[0] + '\t' + entries[1]

