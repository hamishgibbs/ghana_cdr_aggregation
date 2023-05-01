import sys
import json
import pandas as pd
from snakemake.io import expand
import random

def chunks(l, n):
    """Yield n number of striped chunks from l."""
    for i in range(0, n):
        yield l[i::n]

def get_server_number(fn):
    # Conveniently, the servers we are using are
    # named in numerical order starting at 2
    return int(fn.split("_")[-1].split(".")[0][-1]) - 2

def main():

    jobs = pd.read_csv(sys.argv[1], header=None)[0].to_list()
    
    jobs.sort()
    random.seed(1)
    random.shuffle(jobs)
    jobs = list(chunks(jobs, 7)) # 7 servers
    
    server_jobs = jobs[get_server_number(sys.argv[-1])]

    with open(sys.argv[-1], "w") as f:
        for job in server_jobs:
            f.write(job + "\n")
    

if __name__ == "__main__":
    main()

