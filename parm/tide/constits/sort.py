#!/usr/bin/python2.6

import csv
import os

mfile = open('../../master.csv','rb')
mreader = csv.reader(mfile)

curdir = os.getcwd()
dirListing = os.listdir(curdir)

for fname in dirListing:
    mfile.seek(0)
    if '.csv' in fname:
        for row in mreader:
            if row[1] == fname[0:7]:
                os.rename(curdir+'/'+fname,curdir+'/../keepers/'+fname)
                break

