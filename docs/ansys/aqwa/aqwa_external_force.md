
## FDLL

An example External force function is given below

C:\Program Files\ANSYS Inc\v231\aqwa\utils\ExternalForceCalculation

Executes the function at every timestep.

### Detailed example -  External Force Dynamic Link Libraries

Ansys AQWA has the ability to include a user-defined external force algorithm via a dynamic link library (DLL). A DLL can be created to calculate a force based on time, position and/or velocity of a structure. Example interfaces are provided for C or FORTRAN?, but any programming language that can produce a DLL can be employed. An added mass matrix can be computed at each time step to simulate inertia forces. The calculation can be controlled by a set of up to 100 integer parameters and 100 real parameters that may be input and then passed to the external force routine.

This DLL facility can be used to model:

Dynamic positioning system
Steering system
Towing force provided by a tug
Damping system with unusual characteristics
Suction force between two ships close together, or between a ship and the sea bed
Fluid transfer between vessels
Calculated energy extracted from a wave-power device

### References

<https://forum.ansys.com/forums/topic/modelling-active-heave-compensation-in-ansys-aqwa-2/>

<https://forum.ansys.com/forums/topic/creating-user-defined-function/>

<https://www.cfd-online.com/Forums/fluent/117810-aqwa-compiler-f90-print.html>

<https://www.boatdesign.net/threads/ansys-aqwa-user_force-subroutine.46452/>
