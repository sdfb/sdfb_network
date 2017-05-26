#!/bin/sh

#SBATCH -J test                    # Job name
#SBATCH -o test.out                # Name of stdout output file (%j expands to jobId)
#SBATCH -p normal                        # Queue name
#SBATCH -N 1                          # Total number of nodes requested (20 cores/node)
#SBATCH -n 24
#SBATCH -t 48:00:00                   # Run time (hh:mm:ss) - 4 hours
#SBATCH -A A-ccsc


cd /home/00157/walling/work/sixdegs/jstor/text_processing

bash search_products-files.sh
