/create-spec iterate mooring tensions for the model to a target values

The mooring lines are Line1, Line2, etc. The target tensions are defined as below:


Example iterate script for targetZ is given in : [targetZ iteration](<../docs/modules/orcaflex/scripts/orcfxapi_goby/58 - ScipyRootFinding.py>)

The current manual steps are:
- load the model in orcaflex
- fix the vessels in place
- run the model 
- get the mooring tensions (and associated fender forces)
- iterate the mooring length by stiffness to get the target tensions

A semi automated script is written in :

objetive is to iterate the mooring tensions for the model to a target values in OrcaFlex in python in 1 iteration.

Relevant orcaflex agent: agents\orcaflex


/create-spec research on how to create an agent 