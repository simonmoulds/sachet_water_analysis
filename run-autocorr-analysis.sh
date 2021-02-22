#!/bin/bash

CONDA_BASE=$(conda info --base)
source $CONDA_BASE/etc/profile.d/conda.sh
conda deactivate

conda activate sachet
python3 autocorr-analysis.py
conda deactivate
