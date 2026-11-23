# Line data: End connections

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\Ex}{E\urm{x}}$
$\newcommand{\Ey}{E\urm{y}}$
$\newcommand{\Ez}{E\urm{z}}$

The line end connection data specify whether the line ends are connected to other objects, the position, angle and stiffness of the connection, and whether the end is released during the simulation.

You can view and edit an individual line's connection data on the line's data form. Or you can view and edit the connection data for all the lines together on the [all objects data form](Allobjectsdataform.htm).

### Connect to object

The line spans from end A to end B and each end may be independently [connected to another object](Objectconnections.htm) in the model, or else **fixed**, **anchored** or left **free**.

### Position

Defines the position of the centre of the node at the line end in terms of the [connection coordinates](Objectconnections.htm#ConnectionCoordinates) $x$, $y$ and $z$.

### End offset

Only available when the [length and end orientations](Linedata.htm#LineLengthAndEndOrientations) are **calculated from end positions** and only if the line is not connected to another line or a [turbine blade](Turbinedata,Blades.htm) (i.e. the [z relative to](Objectconnections.htm#ConnectionRelativeTo) data is not available). In this case, the $z$-axis of the local axes at each end will point along the straight line running from end A to end B, which can be helpful when the line is being used to model a structural member forming part of a *truss structure*. It is often useful to further offset the end of the line from the defined connection coordinates, ($x$,$y$,$z$), by a small distance along the straight line running between the two ends. The end offset performs that function, with a positive offset being *into the line* (i.e. towards the other end). This allows many line ends to be connected to a common object (such as a [free constraint](Constraints.htm#FreeConstraints)) at the same coordinates, usually $(0,0,0)$. The end offset is a more natural way to set the ends back from the connector object, thus avoiding a nontrivial geometric computation.

### Height above seabed

This value is only available for **anchored** connections, and then only if the line type is not [profiled](Linetypes,Geometry,massexpansiondata.htm#ProfiledLineTypes); it is otherwise blank and not editable. The value is the vertical height above the seabed of the line underside, and is coupled to the [connection coordinate](Objectconnections.htm#ConnectionCoordinates) $z$ – changing either one causes a corresponding change in the other.

To illustrate how this data item is used consider, for simplicity, a line end anchored to a flat horizontal seabed. The object-relative $z$-coordinate defines the position of the centreline: if it is 0, the end node will penetrate the seabed by a distance of $d/2$, where $d$ is the  [outer contact diameter](Linetypes,Contactdata.htm#LineTypeOuterContactDiameter).

Consequently, the end node is 'buried' in the seabed and receives a large [seabed reaction force](Linetheory,Interactionwithseabedandshapes.htm). Because it is anchored this force cannot displace the end node, but the adjacent node is free to move and will try to take up a position sitting on the seabed. This in turn will lead to unrealistic values of curvature, bend moment etc. at the end.

If, instead, you set *height above seabed* to 0, then the end node centreline will be given a $z$-coordinate of $d/2$, relative to the seabed. The node sits just in contact with the seabed, and the unrealistic reaction forces do not arise.

If the seabed is not horizontal then the mathematics is slightly more complicated, as we must take into account the slope of the seabed. The recommendation of setting height above seabed to 0 remains valid, however.

### Orientation

The [end-fitting orientation](Linetheory,Lineendorientation.htm) relative to the [connection axes](Objectconnections.htm#ConnectionAxes) is determined by the **azimuth**, **declination** and **gamma** angles. These determine the axes directions, $(\Ex,\Ey,\Ez)$, of the end fitting.

$\Ez$ is the end-fitting axial direction and is completely specified by the azimuth and declination angles. When the end segment is aligned with $\Ez$, then no bending moment is applied, so $\Ez$ is sometimes called the *no-moment direction*. Note that $\Ez$ must be specified using the end A to end B convention, i.e. $\Ez$ is *into* the line at end A, but *out of* the line at end B.

|  |  |
| --- | --- |
| Figure: | No-moment directions |

$\Ex$ and $\Ey$ are perpendicular to $\Ez$ and they are defined by specifying the gamma angle, a rotation about $\Ez$. The $\Ex$ and $\Ey$ directions are used for reporting results (e.g. the components of shear force). Also, if the line has torsion included and the end connection twisting stiffness is non-zero, then $\Ex$ and $\Ey$ define the line end orientation for which no torsional moment results.

The connection at a line end is modelled as a *ball-joint*, with the orientation defined by the azimuth, declination and gamma angles being the preferred *no-moment* orientation of the line end, i.e. that which gives rise to no moment in any direction from any rotational stiffness of the connection.

If all of the end connection stiffness values are zero (e.g. to model a ball-joint that is completely free to rotate), then the end orientation angles have no effect on the line behaviour. The angles then only serve to define the local $x$, $y$ and $z$-directions for reporting results (e.g. shear and bend moment components, stress components, etc.) which depend on the local axes directions.

### End connections stiffness

The connection at a line end is modelled as a *ball-joint* with the specified rotational stiffness. The restoring moments applied by the connection depend on the deflection angle, which is the difference between the end-fitting orientation and the orientation of the line. The end orientation is therefore the orientation of the line that corresponds to zero moment being applied by the connection.

The connection stiffness is the slope of the curve of restoring moment against deflection angle.

The bending and twisting connection stiffnesses can be:

* **Zero**, free to rotate with no resistance
* **Non-zero, finite**, can rotate but with resistance
* **Infinity**, a rigid connection
* [Variable](Variabledata.htm), nonlinear (for bending connection stiffness only).

The **x bending** and **y bending** values define the connection bending behaviour for rotation about the end $\Ex$ and $\Ey$ directions, respectively. For an isotropic ball-joint the two values must be equal; this can be achieved conveniently by setting the y-bending value to '~', meaning 'same as x-bending'. A non-isotropic ball-joint can be modelled by giving different x-and y-bending values; in this case the line must [include torsion](Linedata.htm#IncludeTorsion).

The x-bending and y-bending behaviour may be either linear or nonlinear, as follows:

* For simple linear behaviour, define the bending stiffness to be the constant slope of the curve of restoring moment against deflection angle.
* For nonlinear behaviour, use [variable data](Variabledata.htm) to define a table of restoring moment against deflection angle. OrcaFlex uses linear interpolation for angles between those specified in the table, and linear extrapolation for angles beyond those specified in the table. The restoring bend moment must be zero at zero angle.

The **twisting stiffness** value is only relevant if [torsion is included](Linedata.htm#IncludeTorsion) for the line. It specifies the rotational stiffness about the end $\Ez$ direction. This variation is assumed linear, so the twisting stiffness you give should be the slope of the linear angle-moment curve.

A flex joint can be modelled by setting the stiffness values to be non-zero and finite.

|  |  |
| --- | --- |
| Warning: | To define a rigid connection use **infinity** rather than a large stiffness value. Large finite stiffness values can lead to numerical instability, but the value infinity is handled differently. |

### End connections damping

If [torsion is included](Linedata.htm#IncludeTorsion) then connection damping can be specified. The damping moment is given by $-C\omega$, where $C$ is the given value of connection damping and $\omega$ is the angular velocity of the end fitting relative to the angular velocity of the line, measured in deg/s.

### Release at start of stage

Line ends can be [disconnected](Generaldata,Analysis.htm#AnalysisStageRelease) at the start of a given stage of the simulation. For no disconnection, set this item to '~'.
