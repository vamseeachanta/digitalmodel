Robust Designs:

The top figure here links a load hazard curve (left) with a load-deformation curve (right). The hazard curve is rotated on its side such that loads are on the vertical axis and return periods on the horizontal. In the load-deformation curve, you will see performance levels: A (operational) up to D (survival). When plotted side by side, the load return periods (RP) are clearly linked to performance levels. 

The bottom left table expands this relationship. The first column has the load RP, followed by its annual exceedance, then project life (20 years) probability of exceedance (PE), then code factors of safety (API RP2A). The factor is an allowable increase applied to the basic allowed stresses (0.6 SMYS, tension). This shows why the code cannot take chances with loads that have a 100% Life PE (Operational) but when it’s a 0.2%, it is happy with survival.

Finally, the called-out table in the bottom right explains the 1-year RP statistics. I learned it from our Metocean Specialist (Bob Beard) and dear friend.  Using the Poisson Process, when a project life is equal to its RP, the PE is 63%.  That explains the use of the 63% for defining the annual exceedance of a 1-year RP event. There might be a more theoretical definition; please share if you know it.

The key design points to take away here are: 

1. Robustness and resilience cannot be based on 1-tier (performance) design. Economics drive the operational condition, extreme events define the maximum stresses, but rate or accidental events ensure the survival performance.  One cannot neglect any of these for an important structure.

2. There will be a governing loading condition that will drive your design: environmental loading, seismic, blast, etc. This is when the sharing here applies, i.e., an offshore platform in the Gulf of America (formerly GOM) will not be driven by seismic unlike a platform offshore California. 

3 .Finally, it’s better to be approximately correct than precisely wrong!  Search first for the load(s) that will drive each performance level. Then, ensure the design performs at all performance levels! No pun intended!

![capacity_curves]](capacity_curves.jpeg)


Comments

You can make coordinate transformations on those RAOs if both the amplitude and the phase are given. Sometimes we encounter situations where hydrodynamic coefficients such as added mass, damping and restoring matrices are also given at the undesired origin, which we can also perform coordinate transformations on.

Such a pain to work with an Excel sheet with RAOs with conventions missing! Figuring out the origin, what's positive, and finally if it's phase leading/lagging. Sometimes it's just better to generate the RAOs yourself. 