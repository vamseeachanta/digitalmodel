# Data: Morison elements

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\dn}{d\urm{n}}$
$\newcommand{\da}{d\urm{a}}$
$\newcommand{\CD}[1]{\C{D #1}}$
$\newcommand{\hn}{h\urm{n}}$
$\newcommand{\ha}{h\urm{a}}$
$\newcommand{\Ca}[1]{\C{a #1}}$
$\newcommand{\Cm}[1]{\C{m #1}}$
$\newcommand{\Mxyz}{\mat{M}\_{\xyz}} %Mesh coordinates $

### Morison fluid velocity

This data item specifies the form of the fluid velocity used by OrcaWave to evaluate loads on Morison elements. Specifically, fluid velocity is used to compute a relative velocity for [drag forces](Theory,Morisonelements.htm#DragForce), and to compute a fluid acceleration for [inertia forces](Theory,Morisonelements.htm#InertiaForce). OrcaWave always includes the motion of the object itself in both forces.

* If *undisturbed incident wave* is selected, the fluid velocity is obtained from the incident wave velocity potential only; the [scattered and radiation velocity potentials](Theory,First-orderequations.htm#PotentialDecomposition) are neglected.
* If *full wave field* is selected, the fluid velocity is obtained from the full velocity potential, including contributions from incident, scattered and radiation velocity potentials.

When using the *full wave field* option, OrcaWave calculates the scattered and radiation velocity contributions using the same method as for [sea state RAO](Results,SeastateRAOs.htm) velocity.

|  |  |
| --- | --- |
| Warnings: | When using the undisturbed incident wave, the effect of all diffraction bodies on the wave field is neglected for the purpose Morison element loads. This simplification reduces run time, but is not valid if the wave field is significantly disturbed at locations where loads are calculated. |
|  | When using the full wave field, Morison elements should not be located inside a diffraction body. Inside a body, where there is no water, the full wave field is meaningless and an element would experience a meaningless load. You can check for this situation by selecting validation of [panel arrangement](Data,Calculationandoutput.htm#ValidatePanelArrangement). |

## Morison element type data

### Drag diameters

The normal drag diameter, $\dn$, and the axial drag diameter, $\da$. If $\da$ is set to '~' then the value of $\dn$ is used.

### Drag coefficients

The drag coefficients $\CD{x}$, $\CD{y}$ and $\CD{z}$, with respect to the element's local axes. The normal coefficients often take the same value; this can be indicated conveniently by setting '~' for $\CD{y}$, to mean 'same as $\CD{x}$'.

### Hydrodynamic diameters

The normal hydrodynamic diameter, $\hn$, and the axial hydrodynamic diameter, $\ha$. If $\ha$ is set to '~' then the value of $\hn$ is used.

Hydrodynamic diameters $\hn$ and $\ha$ are entirely independent from the drag diameters $\dn$ and $\da$. They are used to define the [volume](Theory,Morisonelements.htm#InertiaForce) of the Morison element, and in the calculation of [proportion wet](Theory,Morisonelements.htm#Submergence).

The element volume per unit length is an area. If your input data for added mass or fluid inertia provides a reference area, this can be input directly in the *cross-section area* column of the data form. The value of $\hn$ will automatically update with the corresponding circle diameter.

### Added mass coefficients

The added mass coefficients $\Ca{x}$, $\Ca{y}$ and $\Ca{z}$, with respect to the element's local axes. The normal coefficients often take the same value; this can be indicated conveniently by setting '~' for $\Ca{y}$, to mean 'same as $\Ca{x}$'.

### Fluid inertia coefficients

The fluid inertia coefficients $\Cm{x}$, $\Cm{y}$ and $\Cm{z}$, with respect to the element's local axes.

These coefficients can be given an explicit value, or can be set to '~'. Each direction is treated independently. The use of '~' for a fluid inertia coefficient instructs OrcaWave to calculate the coefficient using the equation $\Cm{}=1+\Ca{}$.

### Pen

Drawing data for the element type in the [mesh view](Meshview.htm).

## Morison element data

Use the drop-down list of bodies to select an owner, then define elements in the table.

### Element type

The [element type](#MorisonElementTypeData) holds the drag data, fluid inertia data and drawing data for the element.

### Position and orientation

The **position** defines the location of end A of the element, relative to the owner's [mesh coordinates](Theory,Coordinatesystems.htm#MeshCoordinates) $\Mxyz$.

The element's $z$-axis points from end A towards end B along along the direction defined by the **azimuth and declination** angles:

* Azimuth angle is measured from the $M\urm{x}$ axis towards the $M\urm{y}$ axis. Therefore the positive $M\urm{x}$ axis has azimuth = 0° and the positive $M\urm{y}$ axis has azimuth = 90°.
* Declination angle is measured from the $M\urm{z}$ axis. Therefore the positive $M\urm{z}$ axis has declination = 0°, any direction in the $M\urm{xy}$ plane has declination = 90° and the negative $M\urm{z}$ axis has declination = 180°.

The element's $x$-axis and $y$-axis are normal to the element's $z$-axis, in directions defined by the **gamma** angle. You can visualise these axes by using the [view options panel](Meshview.htm#ViewOptionsPanel) in the [mesh view](Meshview.htm).

### Length, $L$

The length of the element.

### Number of segments, $N$

The element is discretised into $N$ sub-elements of length $l=L/N$.

|  |  |
| --- | --- |
| Note: | You can import the data for Morison elements and their types into OrcaFlex by [opening](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata,OrcaWave.htm) the results file (.owr) in OrcaFlex. Morison element data cannot be imported using the [import hydrodynamic data](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata.htm) process in OrcaFlex that imports data into a vessel type in an existing OrcaFlex model. |
