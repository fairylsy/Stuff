#!/usr/bin/env python

from operator import itemgetter
import sys
import math
# Hideously ugly reduce example

current_sum = 0.0
current_group = None
current_count = 0
verbose = False
current_key = ''
# input comes from STDIN
for line in sys.stdin:
    # Remove trailing '\n'
    line = line.strip()

    # Extract (key,value)
    # Compute variance = \sum x_i^2 - n \bar{x}^2.
    # So we need to record cumulative sum of elements and their square
    tmp_string = line.split('\t')
    tmp_key = tmp_string[0]
    if (len(current_key) == 0):
        current_key = tmp_key
        current_count = 1
        current_sum_sq = float(tmp_string[1])**2
        current_sum = float(tmp_string[1])
    if (current_key == tmp_key):
        current_count = current_count + 1
        current_sum_sq = current_sum_sq + float(tmp_string[1])**2
        current_sum = current_sum + float(tmp_string[1]) 
    else:
        # print key name, mean and variance
        print current_key + ','+ str(current_sum/current_count) + ',' +str((current_sum_sq - current_sum**2/current_count)/current_count)
        current_key = tmp_key
        current_count = 1
        current_sum_sq = float(tmp_string[1])**2
        current_sum = float(tmp_string[1])




