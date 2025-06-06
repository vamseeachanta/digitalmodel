## Introduction


To make a multiple mooring-fender model converge and get the right tensions

## Summary


tests:
 -  tests\modules\orcaflex\orcaflex_analysis\ofx_mooring_analysis.yml

### Detailed Procedure

#### Technical:
- Define approximate mooring length and locations of the vessel right
- Vessels should be fixed at the predetermined fender compression 
- Mooring should be solved and successful analysis output is required

#### File Management:
- Save the model in yml file with only BaseFile reference and no other data
 - Note that this .yml file will be iterated and overwritten

#### Inputs:
- Update the target_pretension and line properties .csv file
  - Update mooring target line lengths. If more than 2 sections are contained, 1 of them (tail) should be fixed.
  - update line properties for calculations
  - Update the starting tensions required

- Update the fender properties .csv file
  - Update the fender properties

#### Analysis Procedure:
- Run the static analysis to be solved
- Postprocess to find the mooring and fender forces to balance
- Calculate the required mooring line length to achieve the target mooring pretension.
- 


#### Troubleshooting:
- If a lwl or hwl configuration does not run. Take the nearest line lengths and reiterate again.
- 