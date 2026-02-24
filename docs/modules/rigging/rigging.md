# Introduction

Design lifting gear (slings, shackles, etc.)

# Summary

TBA




## Go-by Guidance
- MBL = Minimum Breaking Load
- SWL = Safe Working Load
- WLL = Working Load Limit


- MBL/WLL = FOS

if unknown rigging design, 
Utilize manufacturer FOS if known
start with FOS = 5 (Conservative) 


## Components

 (15+ )

https://www.thecrosbygroup.com/catalog/shackles/

| Category |   No of Subcategories |  Implemented | Naming Conventions | Links | Comments |
|---------|--------|-----|-----|------------------|-------------|
| Shackles |  25+ | tba | tba | https://www.thecrosbygroup.com/catalog/shackles/ |  |
| Master Links |  15+ | tba | masterlink_wlll70 | https://www.thecrosbygroup.com/catalog/shackles/ | 134_masterlinks_oblong_a342 |
| wire rope |  15+ | tba | wirerope_wlll70 |  | |
| slings |  15+ | tba | sling_wlll70 |  | |

## Master links

| Category |   Subcategory |  Implemented | Naming Conventions | Link | Comments |
|---------|--------|-----|-----|------------------|-------------|
| Master Links |  134_masterlinks_oblong_a342.xlsx |  | tba | https://www.thecrosbygroup.com/products/master-links/oblong/alloy/crosby-a-342-forged-alloy-master-links/ | tba |
| Master Links |  tba |  | tba | tba | tba |


## Shackles

| Category | Subcategory |  Implemented | Naming Conventions | Link | Comments |
|---------|--------|-----|-----|------------------|-------------|
| Shackles |  Nautilus ROV Hooks |  | tba |  | nautilus_subsea_data_sheet |
| Shackles |  Bolt Type G2130 |  | tba |  | 26_shackles_bolt_type_anchor_g2130 |

## Using OrcaFlex wizard

https://www.orcina.com/webhelp/OrcFxAPI/Content/html/Pythonreference,OrcaFlexWizardObject.htm

## Tips for COnvergence

- ENsure the Z axis is aligned over the length of umblilical/sling for convergence
- Mesh is refined sufficiently at the end points. However, not below OD of the structure as this can lead to convergence issues.
- General: In statics, increase the tolerance (0.025) and damping (max Up to 100 and min up to 40)
- Solve 6D Buoys for "NOne" and them change to "X,Y,Z" and then to "ALL" after getting them running
