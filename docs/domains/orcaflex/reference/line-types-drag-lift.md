# Line types: Drag & lift data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\Ct}{C\urm{t}}$

## Drag coefficients

Drag coefficients are given for the normal ($x$ and $y$) directions and axial $(z)$ direction. The $z$ coefficient is constant, while the $x$ and $y$ coefficients may take, independently, the form of:

* a fixed constant value
* a value that [varies](Variabledata.htm) with [Reynolds number](Environment,Seadata.htm#ReynoldsNumberCalculation)
* a value that varies with height above seabed
* a value that varies with both Reynolds number and height above seabed

The $x$ and $y$ coefficients often take the same value; this can be indicated conveniently by setting ~ for the $y$ coefficient, to mean 'same as the $x$ coefficient'. OrcaFlex also offers a choice, on the line data form, of formulation of the way in which [drag force components vary with the incidence angle](Linedata,Fluidloads.htm#LineDragFormulation).

If [wake interference effects](Linedata,Fluidloads.htm#WakeInterference) are being modelled for a given line, then these normal drag coefficients represent the *undisturbed* drag coefficients: the drag force will be calculated based on a drag coefficient modified according to the [wake model](Linedata,Fluidloads.htm#WakeInterference) used.

Full [details of the drag calculation](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryDrag) are given in the line theory section.

### Typical drag coefficient values

For circular cylinders, the drag coefficient for normal flow depends on [Reynolds number](Environment,Seadata.htm#ReynoldsNumberCalculation) $\Reyn$ and surface finish. For values of $\Reyn$ between 2e4 and 3e5 the drag coefficient takes the value 1.2 and is independent of surface roughness. $\Reyn$ values below this range are unlikely to occur in practice. For values of $\Reyn$ greater than 3e5, the drag coefficient is strongly dependent on both $\Reyn$ and surface roughness.

For very smooth cylinders the drag coefficient falls rapidly to 0.28 at $\Reyn$ of about 6e5 before recovering to a value of 0.5 for $\Reyn$ values above 2e6. For rough cylinders the effect is less marked, but remains significant.

In view of this behaviour, the use of [variable data](Variabledata.htm) for normal drag coefficients is strongly recommended. OrcaFlex will then use the value of drag appropriate to the instantaneous local value of Reynolds number throughout the simulation.

The functional form of this dependence is well documented in the open literature and also in proprietary data sources, such as [ESDU 80025](Referencesandlinks.htm#ESDU80025). Note that some of these sources take account of the effect of the amount of turbulence in the incoming flow by defining an *effective* Reynolds number, so care is needed to ensure that the data are presented in a form that is consistent with the [definition of Reynolds number](Environment,Seadata.htm#ReynoldsNumberCalculation) used by OrcaFlex.

The above values apply where vortex-induced vibration (VIV) is expected to be negligible. If significant VIV is anticipated, then drag coefficients may be increased significantly. If this is the case, a more detailed [VIV analysis](VIVanalysis.htm) should be carried out.

*Axial* drag results from skin friction only. In subcritical flow $(\Reyn \lt 3.8\text{E}5)$, the drag coefficient for axial flow, $\Ct$, is 0.008 for a smooth cylinder and 0.011 for a rough cylinder, based on ESDU data. At higher $\Reyn$, ESDU suggest that skin friction may be neglected, i.e. $\Ct{=}0$. In practice, axial drag is often negligible and $\Ct{=}0$ is therefore often acceptable.

Both [Reynolds number](Lineresults,Fluidloads.htm#LineReynoldsNumber) and [drag coefficient](Lineresults,Fluidloads.htm#LineResultsDragLiftCoefficients) are available as [time history results](Results,TimehistoryandXYgraphs.htm).

## Lift coefficient

Used in the calculation of lift force, which acts in the direction normal to the line axis and in the plane of that axis and the seabed normal. The coefficient may take the form of:

* a fixed constant value
* a value that [varies](Variabledata.htm) with [Reynolds number](Environment,Seadata.htm#ReynoldsNumberCalculation)
* a value that varies with height above seabed
* a value that varies with both Reynolds number and height above seabed

For further details see the [theory on lift force calculation](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryLift).

## Drag / lift diameters

These diameters are used when calculating [drag area](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryDrag) and [lift area](Linetheory,Hydrodynamicandaerodynamicloads.htm#LineTheoryLift). They are also used in [wake interference](Linedata,Fluidloads.htm#WakeInterference) and [VIV](VIVanalysis.htm) modelling if the [VIV diameter](Timedomainmodels.htm#TimeDomainModelsVIVDiameter) is '~'. A drag diameter of ~ tells OrcaFlex to use the value of the [outer diameter](Linetypes,Geometry,massexpansiondata.htm#LineTypeDiameters).

|  |  |
| --- | --- |
| Note: | OrcaFlex calculates the normal drag / lift area as $d\_\mathrm{n} l$ and the axial drag area as $\pi d\_\mathrm{a} l$ where $d\_\mathrm{n}$ is the normal drag / lift diameter, $d\_\mathrm{a}$ the axial diameter and $l$ the element length. Note that different programs handle these calculations in different ways: it is common for programs to use a single drag / lift area for both normal and axial flow, and some other programs do not include the factor of $\pi$ in the axial drag area. You must take care when comparing or transferring these data between different programs. |
