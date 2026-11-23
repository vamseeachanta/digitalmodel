# 6D buoys: Lumped buoy properties

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

|  |  |
| --- | --- |
| Figure: | Lumped buoy |

**Lumped buoy** properties are specified relative to the buoy local frame of reference $\Bxyz$.

## Geometry

**Volume** is the total volume of the buoy, with its centre at the **centre of volume** defined relative to local axes.

**Height** is the buoy vertical extent and is assumed to be equally spaced about the centre of volume. Height is taken to be independent of buoy rotation.

The lumped buoy height is used for two main purposes:

* To determine the contact area used to calculate forces of [contact with shapes and the seabed](6Dbuoytheory,Contactforces.htm#6DBuoyContactForces).
* To calculate the [proportion wet](6Dbuoytheory,Overview.htm#6DBuoyProportionWet) (used to scale the hydrostatic and hydrodynamic forces) and the centre of *wetted* volume (the point at which they are applied).

## Damping

Hydrodynamic damping forces and moments may be applied to the buoy. These are directly proportional to the velocity or angular velocity of the buoy; this velocity may be [relative to the fluid or to earth](6Dbuoys,Commondata.htm#BuoyDampingRelativeTo). For each of the buoy axes directions, you define the magnitude of the **unit force** to be applied when the relative velocity is 1 length unit/second. OrcaFlex then scales these magnitudes by the instantaneous relative velocity and applies the resulting force at the buoy origin. Similarly you can specify **unit moments**, applied about each axis, when the relative angular velocity is 1 radian/second. Full details of these calculations are given under the [theory for lumped buoy damping](6Dbuoytheory,Lumpedbuoyaddedmass,dampinganddrag.htm#hcLumpedBuoyDamping).

## Drag

Hydrodynamic and aerodynamic drag forces and moments may be applied to the buoy. These are proportional to the square of the relative velocity, or angular velocity, of the sea or wind past the buoy.

|  |  |
| --- | --- |
| Note: | Aerodynamic drag is only included if the [include wind loads on 6D buoys](Environment,Winddata.htm#WindLoadsIncluded) option is enabled in the environment data. |

The drag forces are specified by giving, for each of the local buoy axes directions, the **drag area** that is subject to drag loading in that direction and the corresponding **drag coefficient**.

Drag moment properties are specified in a similar way, except that instead of
a drag area you give a **moment of area**. See the [theory for lumped buoy drag](6Dbuoytheory,Lumpedbuoyaddedmass,dampinganddrag.htm#Drag) for more details.

|  |  |
| --- | --- |
| Note: | Drag area moment is the third absolute moment of drag area about the axis. |

## Fluid inertia

Fluid inertia properties are those that are proportional to the acceleration of the sea and the buoy. These accelerations have two main effects. Firstly, they result in forces and moments being applied to the buoy – these are referred to as the fluid acceleration loads. Secondly, the buoy experiences an increase in inertia – this is known as the added mass. More details are presented in the [lumped buoy added mass and inertia](6Dbuoytheory,Lumpedbuoyaddedmass,dampinganddrag.htm) topic.

The fluid inertia properties can be specified by providing either only the **diagonal values** or the **full matrices**.

### Fluid inertia specified by diagonal values

These properties are specified separately for translational and rotational motions, and also separately for each local axis direction.

The translational fluid inertia properties of the buoy are specified, for each of the local buoy axis directions, by giving a reference **hydrodynamic mass** together with the two inertia coefficients, $\Ca$ and $\Cm$. The translational $\Cm$ values may take the value '~', meaning $1{+}\Ca$. The translational hydrodynamic mass values may also use '~', meaning in this case that the value used is the fully-submerged displaced mass (= volume $\times$ water density). This is often a convenient hydrodynamic reference mass to use.

The rotational fluid inertia properties of the buoy are given, for each buoy axis direction, by a reference **hydrodynamic inertia** together with a rotational added inertia coefficient $\Ca$.

|  |  |
| --- | --- |
| Note: | Rotational $\Cm$ values are not required. The sea motion is treated as irrotational, so its angular acceleration is zero. |

### Fluid inertia specified by full matrices

In this case, the added mass and fluid inertia properties are given as full $6{\times}6$ symmetric matrices. These matrices should be defined at the **centre of volume** and be for the fully submerged geometry that the buoy represents; when the buoy is not fully submerged they will be scaled by the [proportion wet](6Dbuoytheory,Overview.htm#6DBuoyProportionWet). The added mass matrix is simply added in to the buoy's virtual mass matrix. The fluid inertia matrix is multiplied by the instantaneous fluid acceleration vector to produce the fluid acceleration loads.

The units of the $3{\times}3$ blocks of the added mass and fluid inertia matrices are
\begin{equation} \nonumber
\left[
\begin{matrix}
M & ML \\
ML & ML^2
\end{matrix}
\right]
\end{equation}
where $M$ and $L$ denote the [units](Generaldata.htm) of mass and length, respectively.

## Slam/water exit force

The [slam force](6Dbuoytheory,Slamforce.htm), as the buoy enters or exits the water, can be modelled by setting the **slam force data** and (if constant slam coefficient data are specified) the **slam area**. Separate slam data are specified for water entry and water exit, and each can be either a constant coefficient or [variable with submergence](6Dbuoytheory,Slamforce.htm#VariableSlamData).

If variable slam data are in use for water entry/exit then the slam area is not used for that direction of motion. If constant slam coefficients are in use for entry/exit then no slam/water exit force will be applied for that direction of motion if the value for slam area is zero, and the [slam force results](6Dbuoys,Results.htm#6DBuoySlamForceResults) will not be available.
