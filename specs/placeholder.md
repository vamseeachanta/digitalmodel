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


/create-spec research on how to create an agent from mixed documentation as below
i.e. ascii files, markdown files, text files, readable PDF, scanned PDFs, excel workbooks, regulatory codes, user manuals, etc.
Create a good phased plan on how to create an agent from mixed documentation. Utilize the context generation tools (contex7, n8n etc.) as needed. Also, plan on how persist the generated context so as to not redo the work along with tracking etc.
Also create a plan on how to add additional documentation to the agent in the future. analyze and create a good plan. 
Add this plan to /create-module-agent slash command