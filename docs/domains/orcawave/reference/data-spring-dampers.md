# Data: Spring/dampers

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\Mxyz}{\mat{M}\_{\xyz}} %Mesh coordinates $

**Spring/dampers** are simple connections linking two points in the model, for example a [body](Data,Bodies.htm) to a point fixed in space, or one body to another. They pull the two points together, or hold them apart, with a force that depends linearly on their relative positions and velocities. They have no mass or hydrodynamic loading and simply apply an equal and opposite force to the two points.

|  |  |
| --- | --- |
| Note: | Spring/dampers can be used to improve the [displacement RAOs](Results,DisplacementRAOs.htm) and results which depend on them, such as QTFs and sea state RAOs. Spring/dampers do not affect results for load RAOs, added mass or damping. |

Spring/dampers combine independent spring and damping effects:

* The spring can take both compression and tension, and has a linear relationship between length and force.
* The damper has a linear relationship between velocity and force.

## Spring/damper data

### Unstretched length

The unstretched length $l\_0$ of the spring/damper.

### Stiffness

The spring tension $T$ is given by
\begin{equation}
T = k(l-l\_0)
\end{equation}
where $k$ is the stiffness, $l$ is the stretched length between the two ends and $l\_0$ the unstretched length.

|  |  |
| --- | --- |
| Note: | The spring is *not* slack if $l < l\_0$ but instead it is in compression, corresponding to a negative value of $T$. |

### Damping

The damper exerts an additional tension of
\begin{equation}
c\, \ODt{l}
\end{equation}
where $c$ is the specified damping value.

### Pen

Drawing data for the spring/damper in the [mesh view](Meshview.htm).

### Connect to object

Each end of the spring/damper can be **fixed** or connected to a [diffraction body](Data,Bodies.htm).

### Position

The **position** defines the location of each end of the spring/damper:

* If the end is fixed, values should be given relative to the [global coordinates](Theory,Coordinatesystems.htm) $\GXYZ$.
* If the end is connected to a body, values should be given relative to the owner's [mesh coordinates](Theory,Coordinatesystems.htm#MeshCoordinates) $\Mxyz$.

|  |  |
| --- | --- |
| Note: | You can import the data for spring/dampers into OrcaFlex (as [links](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Links.htm)) by [opening](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata,OrcaWave.htm) the results file (.owr) in OrcaFlex. Spring/damper data cannot be imported using the [import hydrodynamic data](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata.htm) process in OrcaFlex that imports data into a vessel type in an existing OrcaFlex model. |
