# Links: Data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

### Name

Used to refer to the link.

### Type

May be one of:

* **Tether:** a simple elastic tie having linear stiffness and no damping.
* **Spring/damper:** a combined spring and independent damper, each of which can be either linear or piecewise-linear.

### Connect to object

Each end of the link can be **fixed**, **anchored** or [connected to another object](Objectconnections.htm).

### Position

$x$, $y$ and $z$ [connection coordinates](Objectconnections.htm#ConnectionCoordinates) that determine the initial position of each end of the link.

### Release at start of stage

The link can be [released](Generaldata,Analysis.htm#AnalysisStageRelease) at the start of a given stage of the simulation, by setting this number to the stage number required. Once released a link no longer applies any forces to the objects it connects. If no release is required, then set this item to '~'.

## Tethers

Tethers are simple ties. They do not support compression, or damping, and under tension display a simple linear length-force stiffness relation. Consequently, their data requirements in addition to the above are minimal:

### Unstretched length

The unstretched length $l\_0$ of the tether.

### Stiffness

The stiffness $k$ of the tether.

The unstretched length and stiffness govern the tension $T$ in the tether as
\begin{equation} \label{TensionTether}
T =
\begin{cases}
\dfrac{k(l-l\_0)}{l\_0} &\text{for } l \gt l\_0 \\
0 &\text{for } l \leq l\_0
\end{cases}
\end{equation}
where $l$ is the stretched length between the two ends. If $l$ is less than the unstretched length $l\_0$, the tether falls slack.

## Spring/dampers

A spring/damper allows both compression and damping. The spring and damper are considered independent components, and the force relationship for each may, in each case, be a simple linear one or an arbitrary piecewise-linear form representing a more complex relation. Their data are therefore more extensive than those of a tether, as follows.

### Stiffness

For a **linear** spring in a spring/damper the tension $T$ is given by
\begin{equation}
T = k(l-l\_0)
\end{equation}
where $k$ is the linear stiffness and $l\_0$ the unstretched length. This spring does *not* go slack if $l$ is less than the unstretched length $l\_0$ but instead goes into compression, indicated by a negative value of $T$.

|  |  |
| --- | --- |
| Warning: | This is **not** the same formula as (\ref{TensionTether}) for tethers: note that the units of $k$ differ between the two. |

For a **nonlinear** spring, the force characteristic is specified as a table of tension against length, negative tension indicating compression. For lengths between those specified in the table OrcaFlex will obtain the tension by linear interpolation. For values outside the range of the table, linear extrapolation will be used to estimate the tension.

### Damping

A **linear** damper exerts an additional tension of
\begin{equation}
c\, \ODt{l}
\end{equation}
where

$c$ is the specified damping value

$\ODt{l}$ is the rate of change of stretched length $l$.

A **nonlinear** damper is represented by a table of tension against velocity. For a passive damper, the tension will normally have the same sign as the velocity for each row of the table (otherwise negative damping will be applied). For velocities between those given in the table, OrcaFlex will obtain the tension by linear interpolation. For values outside the range of the table, linear extrapolation will be used.
