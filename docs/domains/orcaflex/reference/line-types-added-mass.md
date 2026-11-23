# Line types: Added mass, inertia, slam data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Added mass coefficients

The [added mass](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryAddedMass) coefficients $\Ca$ are defined for normal ($x$ and $y$-directions) and axial ($z$-direction) flow; the $x$ and $y$ values may be, independently of each other, constant or [variable](Variabledata.htm), while the $z$ coefficient must be constant. The variability may be defined either [close to the sea surface](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTypeVariableAddedMassCloseToSeaSurface) or [close to the seabed](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTypeVariableAddedMassCloseToSeabed). Added mass effects are [calculated](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryAddedMass) using the displaced mass as the reference mass for each cylinder (either the instantaneous or the fully-submerged displacement is used, depending on the form of the data – see the [theory section](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryAddedMass) for details). A value of ~ for the $y$ coefficient may be used, to mean 'same as the $x$ coefficient'.

## Inertia coefficients

The inertia coefficients $\Cm$ are defined in the same way as $\Ca$, for normal and axial directions, though only constant values are allowed. A value of ~ here, however, is allowed for both $x$ and $y$ coefficients and tells OrcaFlex to use the usual ['Froude-Krylov plus added mass' formulation](Morison'sequation.htm#FroudeKrylov) for inertia: for a constant added mass coefficient $\Ca$, this is equivalent to setting the coefficient $\Cm$ to $1{+}\Ca$.

|  |  |
| --- | --- |
| Note: | Inertia coefficients are not specified for homogeneous pipe, but are internally set to '~'. |

## Slam force data

The [slam force](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheorySlamming), as the line enters or exits the water, is determined by the slam force data. Separate values are required for water entry and water exit, and each can be set either to a [constant slam coefficient](Slammingtheory.htm#ConstantSlamData) value or to be [variable with submergence](Slammingtheory.htm#VariableSlamData) relative to the surface. If a slam coefficient is zero then no slam force is applied for motion through the water surface in the corresponding direction.

[Slam force results](Lineresults,Fluidloads.htm#LineResultsSlamForce) are available at each node on lines which have non-zero slam data.
