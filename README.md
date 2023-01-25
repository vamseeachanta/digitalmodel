# Introduction

Digital models for engineering assets. The objective of digital models is to utilize a single source of ascii inputs (single source of truth) to generate equivalent analytical models to encompass lifecycle operations of a product. 

These operations include (and not limited to) the following:
- Analytical calculations
- Computational analysis
- Compuational 3D animations

# Summary

- High level vision proposed:
<img src="docs/digital_model_architecure.svg" width=auto, height=auto/>


# Usage

A quick way to running code is:
    - Create a virtual environment:
        - Use [conda yaml file](https://raw.githubusercontent.com/vamseeachanta/digitalmodel/master/dev_tools/environment.yml) to create a new environment
        - (or) by installing [digitalmodel]((https://github.com/vamseeachanta/digitalmodel)) package in an environment
- Run the following batch files
    - Download this [digitalmodel repository](https://github.com/vamseeachanta/digitalmodel)
    - activate environment
    - Change command line to "digitalmodel" (outside not in src) folder
        - Run the following python files in tests:
            - python src\digitalmodel\tests\ {change_to_relevant}.py
            - i.e. for catenary riser, python src\digitalmodel\tests\test_catenary_riser.py
        - (or) Run the following batch files in tests:
            - python src\digitalmodel\tests\ {change_to_relevant}.bat
            - i.e. for catenary riser, python src\digitalmodel\tests\test_catenary_riser.bat


## Example Structures


### SALM

SALM (Single Anchor Line Mooring)

Relevant files:
- python src\digitalmodel\tests\test_fea_model_salm_buoy_01.py
- python src\digitalmodel\tests\test_fea_model_salm_buoy_02.py

### Ships or vessels

Light Service Vessels
Intervention vessels

### Risers

#### Catenary Risers (SCR, SLWR)

SCR - Simple Catenary Riser
SLWR - Simple Lazy Wave Riser

Relevant files:
- python src\digitalmodel\tests\test_catenary_riser.py
- python src\digitalmodel\tests\test_catenary_riser.bat

### Pipelines



### Flexibles or Umbilicals

### Umbilicals

### Rigid Jumpers


## Example Software Runs

### OrcaFlex

Relevant files:
- python src\digitalmodel\tests\test_orcaflex_analysis.py



## References


### Manufacturing/Fabrication

[ProdSim python packages](https://github.com/FuchsTom/ProdSim)
[ProdSim Background: An Open-source Python Package for Generating High-resolution Synthetic Manufacturing Data on Product, Machine and Shop-Floor Levels](https://www.sciencedirect.com/science/article/pii/S2212827122004395)
