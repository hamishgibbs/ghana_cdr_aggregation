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
    
    intro_locations = pd.read_csv(sys.argv[1])["pcod"].to_list()

    with open(sys.argv[2]) as f:
        config = json.load(f)

    jobs = expand(
        "data/epi_modelling/results/{mobility_model}/{network}/R0_{R0}/infected_{infected}_trajectory_{iteration}.rds", 
        mobility_model = config["mobility_model_types"],
        network = config["network_types"],
        R0 = config["R0_values"],
        infected = intro_locations,
        iteration = range(0, 100)
    )
    jobs.sort()
    random.seed(1)
    random.shuffle(jobs)

    server_number = get_server_number(sys.argv[-1])

    jobs = list(chunks(jobs, 7)) # 7 servers

    with open(sys.argv[-1], "w") as f:
        for job in jobs[server_number]:
            f.write(job + "\n")

if __name__ == "__main__":
    main()

