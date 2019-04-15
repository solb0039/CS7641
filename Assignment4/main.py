import os
import csv
import time
import sys

import pandas as pd

file = pd.read_pickle('HardGW/maps/QL Q-Learning L0.9 q100.0 E0.5 Hard Iter 50 Policy Map.pkl')

with open("QL Q-Learning L0.9 q100.0 E0.5 Hard Iter 50 Policy Map.csv", 'w', newline='\n') as myfile:
    wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
    wr.writerow(file)

print(file)

