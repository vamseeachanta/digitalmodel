# Line types: Structure data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Young's modulus

For [homogeneous pipes](Linetypes,Data.htm#LineTypeCategory) only, a value may be given for the Young's modulus of the material. This determines the axial, bending and torsional stiffnesses – these stiffness data items are reported on the data form (in the **all**  [view mode](Linetypes,Data.htm#LineTypesViewMode)), although they cannot be edited.

The value may be constant or variable:

* A constant value results in linear material properties.
* A [variable data](Variabledata.htm) item defines a [nonlinear stress-strain relation](Modellingnonlinearhomogeneouspipes.htm#Data) which results in a bending stiffness with nonlinear elastic behaviour. Note however that the axial and torsional stiffnesses are still assumed to be linear.

## Bend stiffness

The bend stiffness is the slope of the bend moment-curvature curve. The $x$ and $y$ values will often be the same, and this can be indicated by setting the $y$-value to '~', meaning "same as $x$-value".

You can specify the bend stiffness to be **linear**, **elastic nonlinear**, **hysteretic nonlinear** or **externally calculated**, as follows. See [calculating bend moments](Linetheory,Calculationstage2bendmoments.htm#LinesCalculatingBendMoments) for further details of the bending model used.

### Linear bend stiffness

For normal simple linear behaviour, specify the bend stiffness to be the constant slope of the bend moment-curvature relationship. This slope is the equivalent $EI$ value for the line, where $E$ is Young's modulus and $I$ the second moment of area of the cross section. The bend stiffness represents the bend moment required to bend the line to a curvature of 1 radian per unit length.

### Nonlinear bend stiffness

For nonlinear behaviour, use [variable data](Variabledata.htm) to specify a table of bend moment magnitude against curvature magnitude. OrcaFlex uses linear interpolation within this table, and linear extrapolation for curvature values beyond those given. The bend moment must be zero at zero curvature. For nonlinear behaviour derived from a known [stress-strain relationship](Modellingnonlinearhomogeneouspipes.htm#Data) the [plasticity wizard](Linetypes,Plasticitywizard.htm) may be useful to help set up the table.

In the case of nonlinear bend stiffness, you must also specify whether the [hysteretic bending model](Linetheory,Calculationstage2bendmoments.htm#HystereticBending) should be used.

* Non-hysteretic means that the nonlinear stiffness is elastic. No hysteresis effects are included and the bend moment magnitude is entirely determined by the given function of the current curvature magnitude.
* Hysteretic means the bend moment includes hysteresis effects, so that the bend moment depends on the history of curvature applied as well as on the current curvature. Note that if the hysteretic model is used, then the line must [include torsion effects](Linedata.htm#IncludeTorsion).

|  |  |
| --- | --- |
| Warning: | You must check that the hysteretic model is appropriate for the line type being modelled. It is not suitable for modelling rate-dependent effects; it is intended for modelling hysteresis due to persisting effects, such as yield of material or slippage of one part of a composite line structure relative to another part. |

If you use the hysteretic bending model then the simulation speed may be significantly slowed if your table of bend moment against curvature has a large number of rows. You might be able to speed up the simulation, without significantly affecting accuracy, by removing superfluous rows in areas where the curve is very close to linear.

| Note: | If you are using nonlinear bend stiffness, then the [mid-segment curvature results](Lineresults,Moments.htm#LineResultsCurvature) reported depend on whether the bend stiffness is specified to be hysteretic or not. If the bend stiffness is **not hysteretic** then the mid-segment curvature reported is the curvature that corresponds to the mid-segment bend moment (which is the mean of the bend moments at either end of the segment). If the bend stiffness **is hysteretic** then the mid-segment curvature cannot be derived in this way (due to possible hysteresis effects) so the mid-segment curvature reported is the mean of the curvatures at the ends of the segment. This difference may be significant if the bend stiffness is significantly nonlinear over the range of curvatures involved. |

The choice of **statics model** controls the interpretation of the nonlinear bend stiffness table during the statics calculation. There are two options:

* **Pressurised:** the bend moment is calculated from the curvature by simple interpolation of the bend stiffness table. This is the same as the nonlinear elastic model during statics.
* **Depressurised:** the bend stiffness is linear, and determined by the slope of the final two rows of the bend stiffness table. Once the dynamic simulation starts, the line is assumed to be pressurised and the hysteretic model is applied. OrcaFlex enforces continuity in the transition from linear stiffness in statics to hysteretic, nonlinear stiffness in dynamics.

To understand better the rationale behind these options, consider the example of a flexible riser. A flexible riser is built of layers. When the riser is not pressurised, these layers are free to slide over each other. When the riser is pressurised, this introduces friction between the layers. As the riser is bent, this friction has the effect of increasing the apparent bend stiffness of the riser. Eventually, under bending, the friction reaches a certain limiting value and the layers are then able to slip over each other. This inter-layer friction is what gives rise to the hysteretic behaviour of a flexible riser.

Under the depressurised model, OrcaFlex is assuming that the post-slip stiffness is the same as the depressurised stiffness, and is given by the final two rows of the bend stiffness table. So the depressurised option is for scenarios in which the static analysis models the riser before it has been pressurised. Typically the riser will be installed without internal pressure and so its geometry will be determined by the much lower, post-slip stiffness. However, once the riser is pressurised, the dynamic bending stiffness is higher due to the inter-layer friction.

For further details see [nonlinear bend stiffness theory](Linetheory,Calculationstage2bendmoments.htm#NonLinearBendStiffnessTheory).

Finally, the **external results** option allows you to specify an [external function](Externalfunctions.htm) that can be used to track the bend stiffness calculation and provide user defined results.

### Externally calculated bend moment

This form of bend stiffness allows the bend moment to be calculated by an [external function](Externalfunctions.htm). If this option is used then the line must [include torsion effects](Linedata.htm#IncludeTorsion). The external function can be written by the user or other software writers. For details see the [OrcaFlex programming interface (OrcFxAPI)](Automation,Introduction.htm#OrcFxAPI) and the [OrcFxAPI documentation](https://www.orcina.com/webhelp/OrcFxAPI/).

|  |  |
| --- | --- |
| Warning: | Nonlinear behaviour breaks the assumptions of the [stress results](Lineresults,Stressstrain.htm) and [fatigue analysis](Fatigueanalysis,Introduction.htm) in OrcaFlex. You should therefore not use these facilities when there are significant nonlinear effects. |

## Axial stiffness

The axial stiffness is the slope of the curve relating wall tension to strain. The data define the behaviour in the unpressured state, i.e. atmospheric pressure inside and out. [Pressure effects](Linetheory,Linepressureeffects.htm), including the [Poisson ratio effect](#PoissonRatio), are then allowed for by OrcaFlex.

|  |  |
| --- | --- |
| Note: | Axial strain is defined as $(l - l\_0) / l\_0$, where $l$ and $l\_0$ are respectively the stretched and [unstretched length](Linedata,Structure.htm#LineSectionLength) of a given piece of pipe. Here, 'unstretched' means the length when unpressured and unstressed. When a pipe is pressured its tension at this 'unstretched' length is often not zero because of strains due to pressure effects. For a homogeneous pipe this can be modelled by specifying the Poisson ratio (see below); for a non-homogeneous pipe (e.g. a flexible), however, the Poisson ratio may not be able to capture the pressure effects. |

### Linear axial stiffness

For a simple linear behaviour, specify the axial stiffness to be the constant slope of the line relating wall tension to strain. This slope is the equivalent $EA$ value for the line, where $E$ is Young's modulus and $A$ is the cross section area. It represents the force required to double the length of any given piece of line, assuming perfectly linear elastic behaviour. (In practice, of course, lines would yield before such a tension was reached.)

### Nonlinear axial stiffness

For a nonlinear behaviour, use an axial stiffness [variable data source](Variabledata.htm) to define a table of wall tension against axial strain. OrcaFlex uses linear interpolation within the table and linear extrapolation for strain values beyond those given in the table.

In the case of nonlinear axial stiffness, you must also specify whether the hysteretic axial stiffness model should be used.

* Non-hysteretic means that the nonlinear stiffness is elastic. No hysteresis effects are included and the tension is entirely determined by the given function of the strain. In this case, the wall tension is allowed to be non-zero at zero strain.
* Hysteretic means the tension includes hysteresis effects, so that the tension depends on the history of strain applied as well as on the current strain. The model used is directly analogous to the one used for [hysteretic bending](Linetheory,Calculationstage2bendmoments.htm#HystereticBending). The independent variable in the hysteretic bending model is curvature, whereas for axial stiffness the independent variable is strain; similarly, the dependent variable in the bending model is bend moment, whereas for axial stiffness it is tension. In this case, the nonlinear stiffness data must always be single-sided.

For further details see [nonlinear axial stiffness theory](Linetheory,Calculationstage1tensionforces.htm#NonLinearAxialStiffnessTheory).

The **external results** option allows you to specify an [external function](Externalfunctions.htm) that can be used to track the axial stiffness calculation and provide user defined results.

### Axial static-dynamic stiffness

For an axial stiffness that varies between the [static analysis](Staticanalysis.htm) and the [dynamic analysis](Dynamicanalysis.htm), use an *axial static-dynamic stiffness*  [variable data source](Variabledata.htm). This can be desirable when modelling lines with rate-dependent stiffness, e.g. synthetic rope. By associating the system's steady response with the static analysis, e.g. under mean loading, and higher frequency response with the dynamic analysis, e.g. under wave loading, it allows you to nominate the prevailing axial stiffnesses that best characterises the line's behaviour.

An *axial static-dynamic stiffness* variable data source defines the stiffness during both the static and dynamic analyses. You must specify the *static stiffness* to be either: a non-zero, linear stiffness value; or a nonlinear axial stiffness variable data source. If the static stiffness is nonlinear, it cannot be hysteretic. The *dynamic stiffness method* dictates how you specify the *dynamic stiffness*, $k\_\mathrm{dyn}$, i.e. the rate of change of the wall tension in air, ${T\_\mathrm{w}}^\textrm{air}$, with strain, $\epsilon$,
\begin{equation}
k\_\mathrm{dyn} = \frac{\Delta {T\_\mathrm{w}}^\textrm{air}}{\Delta \epsilon}
\end{equation}
If the *coefficient* method is selected, you provide two coefficients, $a$ and $b$ (at least one of which must be non-zero). As presented in the [SYROPE model](Referencesandlinks.htm#Falkenberg2017), these coefficients are used to define the dynamic stiffness as a linear function of the static tension
\begin{equation}
k\_\mathrm{dyn} = a + b \times \max\{{T\_\mathrm{w}}^\textrm{air}\_\textrm{static}, 0 \}
\end{equation}
The $a$ coefficient directly contributes a linear stiffness term. The $b$ coefficient contributes a linear stiffness term that is proportional to the segment's static tension, allowing each segment to have a unique dynamic stiffness. This is important for modelling synthetic ropes, for which the dynamic stiffness can increase with mean tension.

|  |  |
| --- | --- |
| Note: | Ensure that the coefficients $a$ and $b$ are defined consistently with the model outlined above, where stiffness and tension are considered dimensional quantities (i.e., measured in force). In certain references, e.g. [Bureau Veritas NR432](Referencesandlinks.htm#BVNR432), stiffness and tension may be normalised by the minimum breaking strength. In these cases, the coefficients will not align with those used in the OrcaFlex model. |

Alternatively, the *tabular* dynamic stiffness method gives you more control over this relationship and allows you to specify the dynamic stiffness to be a piecewise linear function of the static tension
\begin{equation}
k\_\mathrm{dyn} = f(\max\{{T\_\mathrm{w}}^\textrm{air}\_\textrm{static}, 0 \})
\end{equation}
OrcaFlex uses linear interpolation within the table and linear extrapolation for static tension values beyond those given in the table. The below figure shows how these data are used to relate the wall tension in air to the strain

|  |  |
| --- | --- |
| Figure: | Axial static-dynamic stiffness |

During the *static* analysis, the relationship between tension and strain is represented by the blue curve. For linear behaviour, you provide a static stiffness value which defines the constant slope of this relationship. For nonlinear behaviour in statics, as shown in the figure, you select an axial stiffness [variable data source](Variabledata.htm) which defines the table of wall tension against axial strain. Once the static analysis is complete, and the system is in equilibrium, the static strain, $\epsilon\_\textrm{static}$, and tension, ${T\_\mathrm{w}}^\textrm{air}\_\textrm{static}$, are recorded for each line segment. At the start of the *dynamic* analysis, the tension-strain relationship transitions from the blue curve onto the red line at their intersection, i.e. at the static strain and tension, ensuring continuity in the system's equilibrium. The linear dynamic stiffness, i.e. constant slope of the red line is determined according to the dynamic stiffness method, discussed above. For the expressions that relate tension to the strain, see the [axial static-dynamic stiffness theory](Linetheory,Calculationstage1tensionforces.htm#AxialStaticDynamicStiffnessTheory).

|  |  |
| --- | --- |
| Note: | The *dynamic* axial stiffness will also be used for: [modal analysis](Modalanalysis,Dataandresults.htm); reporting [mooring stiffness matrices](Vesselmooringstiffnesscalculation.htm); and calculating the [axial numerical damping](Linetheory,Calculationstage1tensionforces.htm#C) and [shortest natural nodal period](Generaldata,Explicitintegration.htm#AlwaysUseRecommendedTimeSteps) for the explicit integration scheme. |

### Externally calculated wall tension

This form of axial stiffness allows the wall tension to be calculated by an [external function](Externalfunctions.htm). For details see the [OrcaFlex programming interface (OrcFxAPI)](Automation,Introduction.htm#OrcFxAPI) and the [OrcFxAPI documentation](https://www.orcina.com/webhelp/OrcFxAPI/).

### Direct tensile strain for hysteretic, externally calculated, or static-dynamic axial stiffness

After the simulation is complete, OrcaFlex can recover wall tension from the logged results. To present [direct tensile strain](Lineresults,Stressstrain.htm#LineDirectTensileStrain) results, OrcaFlex requires a unique correspondence between wall tension and strain. For hysteretic wall tension, and static-dynamic axial stiffness, this is not satisfied, and for externally calculated wall tension such behaviour cannot be assumed. As such the direct tensile strain result, and further results derived using direct tensile strain, will be unavailable.

## Poisson ratio

The Poisson ratio of the material that makes up the wall of the line type, used to model any length changes due to the radial and circumferential stresses caused by [contents pressure](Linedata,Contents.htm#LineContentsPressure) and external pressure.

A Poisson ratio of zero means no such length changes. For metals such as steel or titanium the Poisson ratio is about 0.3 and for polyethylene about 0.4. Most materials have Poisson ratio between 0.0 and 0.5.

|  |  |
| --- | --- |
| Note: | The Poisson ratio effect is calculated assuming that the line type is a pipe made from a homogeneous material. It is not really applicable to complex structures such as flexibles, whose length changes due to pressure are more complex, although an effective Poisson ratio could be specified as an approximation. |

## Torsional stiffness

The torsional stiffness specifies the relationship between [twist and torsional moment (torque)](Linetheory,Calculationstage4torsionmoments.htm#LinesCalculatingTorsionMoments). It is only used if [torsion is included](Linedata.htm#IncludeTorsion). You can specify linear or nonlinear behaviour.

### Linear torsional stiffness

For a simple linear behaviour, specify the torsional stiffness to be the constant slope of the torsional moment-twist per unit length relationship. This slope is the equivalent $GJ$ value for the line, where $G$ is the shear modulus and $J$ is the axial second moment of area. It represents the torque which arises if the line is given a twist of 1 radian per unit length.

### Nonlinear torsional stiffness

For a nonlinear behaviour, use [variable data](Variabledata.htm) to define a table of torque against twist per unit length. OrcaFlex uses linear interpolation within the table, and linear extrapolation for values outside those given in the table. The torque must be zero at zero twist.

In the case of nonlinear torsional stiffness, you must also specify whether the hysteretic stiffness model should be used.

* When defining nonlinear elastic torsional stiffness you should provide values for both positive and negative twist per unit length. This allows you, for example, to have different stiffnesses for positive and negative twisting. If the behaviour is mirrored for positive and negative twist then you must specify the full relationship – OrcaFlex does not automatically reflect the data for you.
* When intending to apply hysteretic torsional stiffness, you should only provide input values for positive twist per unit length. The behaviour model used is described by our existing documentation of [hysteretic bending](Linetheory,Calculationstage2bendmoments.htm#HystereticBending).

The **external results** option allows you to specify an [external function](Externalfunctions.htm) that can be used to track the torsional stiffness calculation and provide user defined results.

### Externally calculated torque

This option for torsional stiffness specifies that segment torque is calculated by an [external function](Externalfunctions.htm). For details see the [OrcaFlex programming interface (OrcFxAPI)](Automation,Introduction.htm#OrcFxAPI) and the [OrcFxAPI documentation](https://www.orcina.com/webhelp/OrcFxAPI/).

|  |  |
| --- | --- |
| Warning: | Nonlinear behaviour breaks the assumptions of the [stress results](Lineresults,Stressstrain.htm) and [fatigue analysis](Fatigueanalysis,Introduction.htm). |

## Tension / torque coupling

Defines a direct coupling between tension and torque. This coupling allows [axial strain to induce torque](Linetheory,Calculationstage4torsionmoments.htm), and allows [twist to induce tension](Linetheory,Calculationstage1tensionforces.htm). It is only used if [torsion is included](Linedata.htm#IncludeTorsion).

|  |  |
| --- | --- |
| Warning: | Tension / torque coupling breaks the assumptions of the [stress results](Lineresults,Stressstrain.htm) and [fatigue analysis](Fatigueanalysis,Introduction.htm). |

## Additional bending stiffness

Only available for [homogeneous pipes](Linetypes,Data.htm#LineTypeCategory), this value increases the overall bending stiffness of the line type. If the pipe has a constant Young's modulus, this value is simply added to the resulting bend stiffness; if the Young's modulus is variable, then the gradient of each section of the corresponding piecewise linear moment–curvature relationship is increased by this value.

The intent is to represent the extra stiffness a line may receive from any applied coatings or linings, for example the concrete coating often applied to steel flowlines. There is an assumed sharing of the loads between the original line type structure and that represented by the additional stiffness. For [general category](Linetypes,Data.htm#LineTypeCategory) line types, such load sharing can be represented by the line type [stress loading factors](Linetypes,Stressdata.htm#LineTypeStressLoadingFactors). For a homogeneous pipe, however, the stress loading factors cannot be modified: instead, OrcaFlex will [automatically modify results](Linetypes,Stressdata.htm#AdditionalBendingStiffness) that are affected by stress loading factors (typically [stress and strain](Lineresults,Stressstrain.htm) results) to reflect the additional bending stiffness.

## External results from nonlinear stiffness

As listed on this page, each of the variable data sources for line type axial stiffness, bend stiffness and torsional stiffness can accept an external function in order to provide external results. Here we note some details of how those external results are then presented in OrcaFlex.

External results associated with line type stiffness are reported at line *mid-segment*  [result points](Lineresults.htm#ResultPoints). External results associated with axial and torsional stiffness present values that are calculated by the node at the end of the line segment closest to line end A. External results associated with bend stiffness present values that are an *average* of the result values calculated by the nodes at either end of the segment.
