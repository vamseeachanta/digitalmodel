### Objective

Create a simple ecosystem for running OrcaFlex using python.

## Summary

The key steps are:

- Install Miniconda
  - Create an environment
- Install VS Code for development
- Keep a small python code handy

## Install Miniconda

Download and install Miniconda from [here](https://docs.conda.io/en/latest/miniconda.html).
For more detailed instructions:

- [Windows](https://docs.conda.io/projects/conda/en/latest/user-guide/install/windows.html)
- [Linux](https://docs.conda.io/projects/conda/en/latest/user-guide/install/linux.html)

#### Create an environment

Download envornment file from below

```bash
conda create env -f environment.yml
conda activate digitalmodel
```

## Install VS Code

Download and install VS Code from [here](https://code.visualstudio.com/)

## Keep a small python code handy

```python
import os
import subprocess
import sys

def run_orcaflex():
    orcaflex_path = r"C:\Program Files (x86)\OrcaFlex\OrcaFlex.exe"
    orcaflex_file = r"C:\Users\user\Documents\OrcaFlex\example.or6"
    subprocess.run([orcaflex_path, orcaflex_file])

if __name__ == "__main__":
    run_orcaflex()
```
