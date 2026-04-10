# Data: Calculation and output

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\SB}{S\_B} %body surface $
$\newcommand{\CWL}{C\_{WL}} %body waterline $

## Solve type

OrcaWave offers the following solve types:

### Potential formulation only

OrcaWave always solves the [potential formulation](Theory,First-orderequations.htm#PotentialFormulation). The solution yields results for the velocity potential, $\phi$, at all mesh panels. This enables pressure to be evaluated on the surface of all bodies and hence results include [added mass and damping](Results,Addedmassanddamping.htm), [load RAOs](Results,LoadRAOs.htm) and [displacement RAOs](Results,DisplacementRAOs.htm).

### Potential and source formulations

Solving the [source formulation](Theory,First-orderequations.htm#SourceFormulation) yields results for the source function, $\sigma$, at all mesh panels. This enables tangential fluid velocity to be evaluated on the surface of all bodies and hence additional results are available including [panel velocity](Results,Panelresults.htm#PanelVelocity) and mean drift loads using the [pressure integration method](Results,Quadraticloads.htm#PressureIntegrationMethod).

### Full QTF calculation

In addition to solving the potential and source formulations for each first-order frequency, OrcaWave will also solve for the [second-order potential](Theory,Second-orderequations.htm) at second-order frequencies. Results are generated for [potential loads](Results,Potentialloads.htm) and [quadratic loads](Results,Quadraticloads.htm), which together give [full QTFs](Theory,Second-orderequations.htm#SecondOrderLoads).

## Resonance damping lid

This option allows you to include a [damping lid](Theory,Dampinglid.htm) on the free surface. A damping lid can be used to damp resonant responses in the fluid, e.g. in a confined space such as a moonpool or a narrow gap between vessels. A damping lid cannot be included when a full QTF calculation is performed.

## Output options

These options allow you to decide whether to include certain quantities in the OrcaWave results:

* Panel pressure & panel velocity – i.e. [panel results](Results,Panelresults.htm) for the total first-order potential
* Body wire frames – for importing into OrcaFlex
* Intermediate results – for use in a [restart analysis](Data,Model.htm#RestartAnalyses), for [decomposed panel results](Results,Panelresults.htm#DecomposedPanelResults) or for calculating [time domain panel pressure](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Vesselresults.htm#TimeDomainPanelPressure) from an OrcaFlex simulation.

|  |  |
| --- | --- |
| Note: | Panel results and intermediate results can significantly increase the size of an OrcaWave results file. |

## Computation strategy

This option allows you to choose between two options that determine the computational approach of OrcaWave's solver. The two options give identical results, however they differ in their use of computational resources:

* The *optimised for run time* strategy completes the calculation of each individual wave frequency faster, but uses more [memory](Data,Validation.htm#RequiredMemoryEstimate)
* The *optimised for memory* strategy uses less [memory](Data,Validation.htm#RequiredMemoryEstimate), but the run time may be longer for each individual wave frequency

For large models, it is common that the appropriate [thread count](Userinterface,Toolsmenu.htm#ThreadCount) for parallel processing is limited by the memory available on your machine. For such models, selecting *optimised for memory* enables a larger thread count and increased parallel processing. This improvement outweighs the longer run time for an individual wave frequency and the total run time of the model will be faster.

## Load calculations

### Load RAO calculation method

OrcaWave offers two calculation methods for [load RAOs](Results,LoadRAOs.htm): diffraction and Haskind. If both types of load RAO are computed, then the **preferred load RAO calculation method** will determine which load RAOs are used to calculate displacement RAOs and also sets the default choice for which load RAOs to import when [importing results](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata.htm) into [OrcaFlex](https://www.orcina.com/webhelp/OrcaFlex/) (you can change your mind at the import stage).

|  |  |
| --- | --- |
| Note: | The two methods usually achieve equally accurate (converged) results for a given mesh. Nevertheless, including both methods may be useful during model building for purposes of consistency checking and/or error estimation. |

### Quadratic load calculation method

[Quadratic loads](Results,Quadraticloads.htm) include mean drift loads and, when a full QTF calculation is performed, time-varying quadratic loads. OrcaWave offers three calculation methods: [pressure integration](Results,Quadraticloads.htm#PressureIntegrationMethod), [control surface integration](Results,Quadraticloads.htm#ControlSurfaceIntegrationMethod) and [momentum conservation](Results,Quadraticloads.htm#MomentumConservationMethod). The **preferred quadratic load calculation method** sets the default choice for which quadratic loads to import when [importing results](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata.htm) into [OrcaFlex](https://www.orcina.com/webhelp/OrcaFlex/) (you can change your mind at the import stage).

|  |  |
| --- | --- |
| Notes: | Each method has different [availability criteria](Results,Quadraticloads.htm#MethodComparisonSummary). In addition, the control surface integration and momentum conservation methods often achieve more accurate (converged) results than the pressure integration method for a given mesh. Nevertheless, including multiple methods may be useful during model building for purposes of consistency checking and/or error estimation. |
|  | Momentum conservation cannot be the preferred method in multibody models or full QTF models, due to the [limitations](Results,Quadraticloads.htm#MomentumConservationMethod) of that method. |

### Momentum conservation node count

If the [momentum conservation](Results,Quadraticloads.htm#MomentumConservationMethod) quadratic load method is included in the calculation, this data item determines the number of quadrature nodes for the integration of the Kochin functions with respect to $\theta$.

## Calculation tolerances

These tolerances are used in the diffraction calculation. Their values may affect results. They are also used for the [validation](Data,Validation.htm) of input data.

The **length tolerance** is used to determine whether two points are equal or whether a point lies in a plane. For example, OrcaWave will classify a panel as being on the free surface if $|Z\_C|$ is less than the length tolerance, where $Z\_C$ is the vertical component of its centroid. Ideally, a free-surface panel would have $Z\_C=0$ exactly. The length tolerance allows some leeway, but it should always be as small as possible.

The **waterline Z** tolerance is used to classify whether a vertex of a body panel forms part of a body waterline, $\CWL$. OrcaWave will classify a vertex as part of a waterline if the magnitude of its $Z$ component is less than the waterline Z tolerance.

|  |  |
| --- | --- |
| Note: | Ideally, panel vertices that are part of a waterline should have a $Z$ coordinate of exactly 0. The waterline Z tolerance allows some leeway if this is not the case. |

The **waterline gap** tolerance is used to determine whether two segments on a body waterline, $\CWL$, are connected.

|  |  |
| --- | --- |
| Tip: | The waterline Z tolerance and waterline gap tolerance should usually be larger than the length tolerance, to allow for small errors and adjustments that are often present in panel data. |

## Divide non-planar panels

When this option is selected OrcaWave will divide any non-planar quadrilateral panels into two triangular panels. A panel is considered to be non-planar if its [projection length](Data,Meshdetails.htm#ProjectionLength) exceeds the [length tolerance](#CalculationTolerances).

## Linear solver options

### Linear solver method

Diffraction calculations rely on the solution of linear matrix equations of the form $A\vec{x} = \vec{b}$, where $\vec{x}$ is unknown. OrcaWave offers two solution methods:

* Direct LU
* Iterative AGS

In broad terms, the direct solver is faster for smaller meshes, and the iterative solver faster for larger meshes. A secondary factor that impacts performance is the number of headings. The direct solver can solve for multiple headings without performing much extra work beyond what is needed for a single heading. However, for iterative solvers the total work is proportional to the number of headings.

The iterative solver uses the accelerated Gauss-Seidel method described in [Lee 1988](Referencesandlinks.htm#Lee1988).

As a general guideline, we would typically recommend using the direct solver. Whilst the iterative solver is faster for large meshes, it is our experience that the mesh needs to be huge in order for the iterative solver to be faster. Such huge meshes are rare, and present other problems, notably with memory consumption.

The [diagnostics sheet of the results tables](Results,Tables.htm) reports timing information to help guide your choice.

### Max iterations (iterative solver only)

The maximum number of iterations that can be attempted before the solver fails. In the event of the iterative solver failing to find a converged solution, OrcaWave will fall back to use the direct solver.

### Tolerance (iterative solver only)

The solution tolerance, $\epsilon$, for the iterative solver. The iteration is deemed to have converged when $\| A\vec{x} - \vec{b} \| \leq \epsilon \| b \|$.

## Validation options

These options do not affect calculation results. They are only used in the [validation](Data,Validation.htm) of input data.

### Perform validation of panel arrangement

When this option is selected OrcaWave will perform [additional](Data,Validation.htm#PanelArrangementAdditionalChecks) validation checks such as checking for overlaps or gaps between panels. If selected, two additional [warning levels](Data,Validation.htm#Warninglevels) become active.

Validating panel arrangement takes more time than validating panels individually because it involves looping over *pairs* of panels. To mitigate this, the additional validation checks are performed in parallel (provided the [thread count](Userinterface,Toolsmenu.htm#ThreadCount) is greater than one).

|  |  |
| --- | --- |
| Tip: | If your mesh is particularly large you may want to perform validation of panel arrangement once and then subsequently uncheck this box. |

### Warning levels

These values determine the thresholds for various warning messages during [validation](Data,Validation.htm) of input data.
