#!/usr/bin/env python
# You need to change the name of file name to 'reducer.py' to run on Hadoop.

from operator import itemgetter
import sys

# Hideously ugly reduce example

current_sum = 0.0
current_count = 0
current_key = ''
# input comes from STDIN
for line in sys.stdin:
    # Remove trailing '\n'
    line = line.strip()
    # split the key and value.
    # The first part b[0] contains string of key. bb[1] is value.
    bb = line.split('\t')
    # Get the key. Maybe it is unnecessary
    aa = bb[0].split(',',3 )
    myString = [aa[0],aa[1],aa[2],aa[3]]
    tmp_key = ",".join(myString)
    # If current_key = NULL, which is the first element.
    # Set the key of first element as current key
    if (len(current_key) == 0):
        current_key = tmp_key
    # If another key is the same as the current key, then count it by adding 1.
    if (current_key == tmp_key):
        current_count = current_count + 1
    else:
    # If the key is not equal to the current one, then the change current key\
    # and the value of current_count is number of the previous keys
        print current_key + ','+ str(current_count)
        current_key = tmp_key
        current_count = 1






