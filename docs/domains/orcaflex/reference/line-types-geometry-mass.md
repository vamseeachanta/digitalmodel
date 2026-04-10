# Line types: Geometry, mass & expansion data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Outer and inner diameter

Used to determine buoyancy and mass of contents per unit length respectively.

These data may also be used for other purposes:

* If the [stress diameters](Linetypes,Stressdata.htm#LineTypesStressDiameters) are '~', then these diameters are used in wall tension and stress results calculations.
* If the [contact diameters](Linetypes,Contactdata.htm#LineTypeOuterContactDiameter) are '~', then these diameters are used in contact calculations.
* If the [drag / lift diameters](Linetypes,Dragliftdata.htm#LineTypeDragLiftDiameters) are '~', then the outer diameter is used in their place.

### Profiled line types

For homogeneous pipes only, the outer and inner diameters may [vary](Variabledata.htm) with arc length. To enable this, you define the diameter profile as a line type diameter [variable data source](Variabledata.htm) which is then referenced by the diameter data of the line type. Arc length is defined relative to the start of the [line section](Linedata,Structure.htm) which uses this line type and increases from end A towards end B.

This feature is used when modelling [stress joints](Modellingstressjoints.htm) (where you will find more details on setting up the profile data) and [bend stiffeners](Modellingbendrestrictors.htm#ModellingBendStiffeners).

## Centre of mass

The $x$ and $y$ coordinates of the centre of mass relative to the centreline. These values are only used when [torsion](Linedata.htm#IncludeTorsion) is being modelled. Note that, if the line has [contents](Linedata,Contents.htm), the *contents centre of mass* is assumed to be at the centreline and is not affected by this offset.

## Bulk modulus

Specifies the [compressibility](Buoyancyvariationwithdepth.htm) of the line type. If the line type is not significantly compressible, then the bulk modulus can be set to infinity to represent that it is incompressible.

## Material density

The density of the material (homogeneous pipe only).

## Mass per unit length

The mass of the line or pipe structure per unit length, excluding contents. For homogeneous pipes, this value is not editable: instead, it is calculated from the material density.

## Expansion table

A reference to the expansion table, if any, associated with this line type. An expansion table determines how the material expands and contracts due to changes in contents temperature and contents pressure. Each row in the table has the following independent variables:

* **Temperature**, $T$.
* **(Gauge) pressure**, $P$.

with the dependent variable:

* **Expansion factor**, $\epsilon$.

A strict requirement is that the data form a *complete* (but not necessarily uniform) grid over the independent variables. If there are $M$ distinct values of $T$ specified, and $N$ distinct values of $P$, then the complete grid requirement means that there must be exactly $M \times N$ rows of data specified, one for each possible $(T, P)$ pair. OrcaFlex will raise an error if this requirement is not satisfied.

The resultant expansion factor, $\epsilon(T,P)$, is computed for arbitrary values of $T$ and $P$ via barycentric interpolation, and applied to the owning line in the same way as a directly specified [line expansion factor](Linedata,Structure.htm#LineExpansionFactor). Outside the user-defined grid of $T$ and $P$ values, $\epsilon(T,P)$ is computed via *truncation*, which means that the value used is equal to the value at the nearest point on the boundary of the grid.

Whilst the table allows expansion factor to be specified in terms of both $T$ and $P$, it can also be used to specify contents that depend upon just $T$ (i.e. no $P$-dependence) or just $P$ (i.e. no $T$-dependence). This is achieved by setting the independent variable you wish to eliminate to a special value of N/A. This tells OrcaFlex that there is no dependence on this variable.

|  |  |
| --- | --- |
| Notes: | If one entry in an independent column is set to N/A, then all other values in the same column must also be set to N/A. |
|  | If all the values in the independent columns are set to N/A, this means that the line's expansion factor is constant. This may still be useful in some cases. |

A special value of ditto (") is also permitted in the table. This effectively says to *use the value from the row above*. If the row above is also ditto, then OrcaFlex will look at the row above that, and so on and so forth until it finds a non-ditto value.

|  |  |
| --- | --- |
| Note: | A value of ditto is not permitted in the first row of the expansion table. |

Ditto values can be useful as a way to demarcate the various sections of the table when it is a function of both $T$ and $P$. For instance, if there are are $M$ $T$ values and $N$ $P$ values, then you might specify this as $M$ blocks of $N$ rows, where the $T$ value is fixed in each block (with the $P$ data cycling through the $N$ distinct values). Specifying a temperature of ditto for all but the first row of each block can make the table much easier to scan by eye (since there will be $N\!-\!1$ ditto values within each block).

|  |  |  |  |
| --- | --- | --- | --- |
| Notes: | A given line is permitted to make use of an expansion table, via its line type, or a direct [expansion factor](Linedata,Structure.htm#LineExpansionFactor), but not both. ||  | For [frequency domain](Generaldata,Frequencydomain.htm) analysis, the expansion table is evaluated at the end of the static analysis and then assumed invariant with time and position. |
