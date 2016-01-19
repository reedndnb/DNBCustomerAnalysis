from subprocess import call
import fnmatch
import os
import re

pat = r'customers_chunk*.csv'
files = os.listdir(".")
files = fnmatch.filter(files, pat)

print files

for file in files:
    print file
    m = re.match(r'.*?_chunk(\d+)', file)
    if m:
        num = m.group(1)
    else:
        continue

    print "num: %s" % num
    cmd = "csvcut -c 1 %s > duns_chunk%s.csv" % (file, num)
    os.system(cmd)



