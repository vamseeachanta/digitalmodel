# Constraints: Common data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

### [Name](Themodel.htm)

Used to refer to the [constraint](Constraints.htm).

### In-frame connection

A constraint's in-frame can be **free**, **fixed**, **anchored** or [connected to another object](Objectconnections.htm).

### In-frame initial position

Defines the position of the constraint in-frame in terms of the [connection coordinates](Objectconnections.htm#ConnectionCoordinates) $x$, $y$ and $z$.

### In-frame initial orientation

The azimuth, declination and gamma angles, $\phi, \theta, \gamma$, that define the orientation, $\Ixyz$, of the constraint in-frame relative to the [connection axes](Objectconnections.htm#ConnectionAxes), $\Cxyz$. These angles specify the orientation of the in-frame as follows:

* Start with $\Ixyz$ aligned with $\Cxyz$.
* Then rotate $\Ixyz$ by an angle $\phi$ about $I\urm{z}$ ($= C\urm{z}$ at this point).
* Now rotate by an angle $\theta$ about the resulting $I\urm{y}$ direction.
* Finally rotate by an angle $\gamma$ about the resulting (and final) $I\urm{z}$ direction.

In all these rotations, a positive angle means rotation clockwise about the positive direction along the axis of rotation, and a negative angle means anti-clockwise.

Three-dimensional rotations are notoriously difficult to describe and visualise. When setting the azimuth, declination and gamma, it is best to check that the resulting $\Ixyz$ directions are correct by [drawing the local axes](Viewmenu.htm#MenuViewAxes) on the 3D view.

### Constraint type

Either [calculated DOFs](Constraints.htm#CalculatedDOFs) or [imposed motion](Constraints.htm#ImposedMotion).

### Solution method

Either [direct](Constraints,Solutionmethod.htm#DirectSolutionMethod) or [indirect](Constraints,Solutionmethod.htm#IndirectSolutionMethod).

### Double-sided connection

If the solution method is [indirect](Constraints,Solutionmethod.htm#IndirectSolutionMethod), then the constraint's out-frame can be independently [connected](Objectconnections.htm) to another object. Such a constraint is said to have a [double-sided connection](Constraints,Solutionmethod.htm#DoubleSidedConnections).

|  |  |
| --- | --- |
| Note: | If the constraint has a double-sided connection, then its connection data (detailed below) determines where the out-frame will be when the model is in reset state. This is independent of the position of the in-frame. OrcaFlex does not attempt to enforce the usual relationship between the in-frame and the out-frame (e.g. any **fixed** internal [degrees of freedom](Constraints,CalculatedDOFs.htm#ConstraintDOFs) for a Cartesian constraint) until the analysis begins. It is possible to specify out-frame connection data that is incompatible with the defined relationship between the in-frame and the out-frame; such models are insoluble and will fail to converge. It is important to be aware of such potential pitfalls when [building models](Constraints,Solutionmethod.htm#ModellingConsiderations) with constraints that have double-sided connections. |

### Out-frame connection

If a constraint has a [double-sided connection](Constraints,Solutionmethod.htm#DoubleSidedConnections), then its out-frame can independently be **free**, **fixed**, **anchored** or [connected to another object](Objectconnections.htm).

|  |  |
| --- | --- |
| Note: | The out-frame connection applies in addition to the usual relationship between the in-frame and the out-frame. For instance, a **free** out-frame ostensibly has six free degrees of freedom; however, OrcaFlex may eliminate some or all of these (once the analysis is underway) in order to enforce the stated conditions between the in-frame and the out-frame (such as a particular internal degree of freedom, e.g. $x$, being **fixed** on the [degrees of freedom](Constraints,CalculatedDOFs.htm#ConstraintDOFs) page). |

### Out-frame initial position

Defines the position of the constraint out-frame in terms of the [connection coordinates](Objectconnections.htm#ConnectionCoordinates) $x$, $y$ and $z$. These data are only available if the constraint has a [double-sided connection](Constraints,Solutionmethod.htm#DoubleSidedConnections).

### Out-frame initial orientation

The azimuth, declination and gamma angles, $\phi, \theta, \gamma$, that define the orientation, $\Ixyz$, of the constraint out-frame relative to the [connection axes](Objectconnections.htm#ConnectionAxes), $\Cxyz$. These angles have the same meaning as their [in-frame equivalents](#InFrameInitialAttitude) and are only available if the constraint has a [double-sided connection](Constraints,Solutionmethod.htm#DoubleSidedConnections).
