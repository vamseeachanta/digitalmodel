# Data: QTFs

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\SF}{S\_F} %exterior free surface $
$\newcommand{\CWL}{C\_{WL}} %body waterline $

If your [solve type](Data,Calculationandoutput.htm#SolveType) is equal to *full QTF calculation* then you must specify the data items described below, all of which relate to calculating the [potential load](Results,Potentialloads.htm) contribution to [full QTFs](Theory,Second-orderequations.htm#SecondOrderLoads).

|  |  |
| --- | --- |
| Note: | In addition to the descriptions of specific data items, our documentation also includes a section of [guidance for specifying QTF data](#GuidanceForSpecifyingData). The guidance discusses approaches to model building, sensitivity testing and error estimation. It includes a [suggested recipe of free-surface data](#SuggestedFreeSurfaceData) that can be used, as a starting point, in an initial experimental model. |

### QTF calculation method

OrcaWave can compute the [potential load](Results,Potentialloads.htm) contribution to full QTFs using either the direct method (analogous to diffraction load RAOs), or the indirect method (analogous to the Haskind load RAOs), or both. If both types of potential load are computed, then the **preferred QTF calculation method** will set the default choice for which data to import when [importing results](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Importinghydrodynamicdata.htm) into [OrcaFlex](https://www.orcina.com/webhelp/OrcaFlex/) (you can change your mind at the import stage).

|  |  |
| --- | --- |
| Note: | The direct calculation method is faster and requires less [memory](Data,Validation.htm#RequiredMemoryEstimate) than the indirect method. In addition, the direct method often achieves more accurate (converged) results for a given body mesh. Nevertheless, including both methods may be useful during [model building](#GuidanceForSpecifyingData) for estimating discretisation error due to the body mesh. |

### Free surface panelled zone

The free surface [panelled zone](Theory,Second-orderequations.htm#SurfaceForcing) is the portion of the free surface that is exterior to the body (or bodies) and bounded by a circle centred on the global origin (the [inner circle](Theory,Second-orderequations.htm#SurfaceForcing)). It must be described by panels in order to evaluate the [surface forcing](Theory,Second-orderequations.htm#SurfaceForcing) contribution to full QTFs. The panels can be provided in a mesh file or you can ask OrcaWave to automatically generate them.

If your free surface panelled zone is **defined by a mesh file**, you should provide a mesh file with the same symmetry type as the global mesh. The coordinate data in the mesh file must be given with respect to global coordinates $\GXYZ$. You must also specify the **inner radius**, i.e. the radius of the [inner circle](Theory,Second-orderequations.htm#SurfaceForcing).

|  |  |
| --- | --- |
| Note: | You can exclude surface forcing from the calculation by leaving the mesh file name blank. Doing so reduces the calculation time, but must be justified carefully (e.g. [Engebretsen et al. 2020](Referencesandlinks.htm#EngebretsenPanFonseca2020) study this approximation for an FPSO vessel). In general, the approximation performs better at lower difference frequencies. |

If your free surface panelled zone is **automatically generated**, you must specify a target **panel size** and the **inner radius**.

|  |  |
| --- | --- |
| Notes: | Panel size should generally be similar to the body mesh. |
|  | If you want automatically generated panels, then all body waterlines must be [closed](Data,Meshdetails.htm#Waterlines). The quality of the mesh is better if the waterline segments all have similar length, comparable to the specified panel size. |

|  |  |
| --- | --- |
| Warning: | Using OrcaWave to automatically generate panels is convenient but may lead to mesh quality issues (e.g. if a waterline has some very short segments). Using specialist software to produce a mesh will give you more control over mesh quality. |

The automatically generated mesh can be saved as a WAMIT .gdf file by using the popup menu on the [mesh view](Meshview.htm) or [mesh details](Data,Meshdetails.htm) pages.

### Free surface quadrature zone

The free surface [quadrature zone](Theory,Second-orderequations.htm#SurfaceForcing) is a circular annulus, centred on the global origin, between the inner circle and outer circle. You do not need to provide panels to describe the quadrature zone, but instead define the extent of the zone and the density of collocation points.

The **number of annuli** defines the number of concentric annuli into which the quadrature zone is divided, each of which has a width given by the **radius step**. The radius of the outer circle is therefore given by the inner radius plus the product of these two values.

|  |  |
| --- | --- |
| Note: | If the number of annuli is zero, OrcaWave does not use a quadrature zone: the surface forcing is evaluated using only panelled and asymptotic zones. |

Within each concentric annulus, the surface forcing is integrated using Gaussian quadrature with the number of **radial nodes** and **azimuthal nodes** specified. The total number of collocation points for the quadrature is the product of the number of radial nodes, the number of azimuthal nodes and the number of annuli.

|  |  |
| --- | --- |
| Note: | If the global mesh has one symmetry plane, the number of azimuthal nodes defines the number of nodes, at each radius, per half plane; similarly, if two global symmetry planes are present, it defines the number, at each radius, per quadrant. |

### Free surface outer circle

The [outer circle](Theory,Second-orderequations.htm#SurfaceForcing) is a circle centred on the global origin that defines the outer boundary of the panelled and quadrature zones. Its radius is determined by the data entered for the panelled and quadrature zones. The **number of segments** defines how many line segments are used to perform the line integral around the outer circle as part of the [free surface forcing](Theory,Second-orderequations.htm#SurfaceForcing).

|  |  |
| --- | --- |
| Note: | If the global mesh has one symmetry plane, the number of segments defines the number used for half the outer circle; similarly, if two global symmetry planes are present, it defines the number used per quadrant. |

### Free surface asymptotic zone

The **expansion order** in the [asymptotic zone](Theory,Second-orderequations.htm#SurfaceForcing) determines how many terms are included in the asymptotic series approximations used to evaluate the surface forcing beyond the outer circle.

|  |  |
| --- | --- |
| Note: | The accuracy of the asymptotic zone's contribution to the second-order potential depends on both the expansion order **and** the outer radius. |

## Guidance for specifying QTF data

Full QTF calculations are often very time consuming. Meanwhile, the required input data is a combination of many inter-related data items, as documented above. These factors combine to make it challenging to build a full QTF model which gives both (i) acceptably converged results and (ii) reasonable run times. This section attempts to give guidance and a logical process which could be followed to identify suitable data.

|  |  |
| --- | --- |
| Warning: | The guidance in this section is intended to assist model building and we have tried to make it applicable for a wide range of models. However, modelling choices and data choices are always problem-specific. You must always validate your model by performing a combination of sensitivity analyses and mesh convergence tests. |

### First-order calculations

Before you begin to build a full QTF model, it is important to validate the first-order calculation in your model. Ideally this should include comparing first-order results (added mass and damping, load RAOs, displacement RAOs) from multiple body meshes with different panel sizes. [Load RAOs](Results,LoadRAOs.htm) are especially instructive, because you can also compare results for diffraction load RAOs versus Haskind load RAOs.

It is also important to check [mean drift loads](Results,Quadraticloads.htm) because convergence can be more challenging for these results than for RAOs. One of the reasons that convergence is more challenging is the presence of a line integral around the body waterline $\CWL$ (in both the [pressure integration method](Results,Quadraticloads.htm#PressureIntegrationMethod) and the [control surface integration method](Results,Quadraticloads.htm#ControlSurfaceIntegrationMethod)). To obtain converged mean drift loads you may need to refine the body mesh, in particular reducing the vertical height of panels at the waterline in order to reduce the discretisation error due to such line integrals. This refinement will also benefit the full QTF calculation because similar line integrals are present in the [second-order theory](Theory,Second-orderequations.htm#IntegralEquations).

Ultimately you should conclude the first-order study with a preferred body mesh that achieves acceptable convergence of first-order results. This means balancing a level of accuracy that is acceptable for your analysis against model run time and [memory](Data,Validation.htm#RequiredMemoryEstimate) requirement. You should be able to estimate the error level in the first-order results from your model, e.g. $1\%$ or $5\%$ or similar.

### Second-order calculations

Progressing to a second-order calculation, the task is to achieve acceptable convergence of full QTF results. We assume that a convergence study of the first-order calculation has been performed, as described above, to identify a preferred body mesh. We now consider the body mesh as fixed, and instead focus on identifying free-surface data (i.e. data for the panelled, quadrature and asymptotic zones) that will achieve acceptable convergence of full QTF results.

|  |  |
| --- | --- |
| Notes: | In practice, the error level that is achievable for full QTFs is likely to be greater than the error level for first-order results. That is because the second-order calculation is more complicated and involves more approximations. |
|  | See the [second-order theory](Theory,Second-orderequations.htm#SurfaceForcing) for definitions of the free surface zones ($PZ$, $QZ$ & $AZ$) and radii ($R\_{IC}$, $R\_{OC}$ & $R\_{SB}$) referred to in this section. |

The free-surface data in a full QTF model affects the [potential load](Results,Potentialloads.htm) contribution to [full QTFs](Theory,Second-orderequations.htm#SecondOrderLoads), and in particular the [surface forcing](Theory,Second-orderequations.htm#SurfaceForcing) contribution to potential load. The principal [sources of error](Theory,Second-orderequations.htm#SourcesOfError) in the calculation are documented on the theory page. The general principles applying to free-surface data are:

* Accuracy in the panelled zone ($PZ$) is improved by using more, smaller, panels. However, this also increases run time.
* Accuracy in the quadrature zone ($QZ$) is improved by using more nodes. However, this also increases run time.
* Integration in $QZ$ is significantly faster than in $PZ$. So run time is reduced if you [enlarge $QZ$ and shrink $PZ$](Theory,Second-orderequations.htm#SurfaceForcing), i.e. if you reduce $R\_{IC}$ with $R\_{OC}$ held constant.
* Accuracy in the asymptotic zone ($AZ$) is improved by using larger $R\_{OC}$ and larger expansion order, both of which increase run time.

The appropriate data to use is very problem-specific. The best approach is to run a set of experimental models, varying the data for the three zones:

* Compare [potential load](Results,Potentialloads.htm) results between your experimental models to test the impact of the free-surface data. The goal is to achieve robust results which are not sensitive to small changes in the free-surface data. On the other hand, comparing between [direct](Results,Potentialloads.htm#DirectMethod) and [indirect](Results,Potentialloads.htm#IndirectMethod) calculation methods indicates the discretisation error due to the body mesh, so for this purpose it is recommended to include both methods in your experimental models.
* There are a large number of free-surface data items and you may need to run a large number of experimental models. Therefore it is helpful to reduce the run time of the experimental models as much as possible:

+ Restrict your experimental models to a manageable subset of wave frequencies and headings. Consider restricting (i) the first-order frequencies and headings *and* (ii) the second-order frequencies and heading pairs.
+ Build your experimental models as [restart models](Data,Model.htm#RestartAnalyses). This avoids unnecessary re-calculation of the first-order results.
+ If your model includes a control surface, uncheck the [quadratic load calculation method](Data,Calculationandoutput.htm#LoadCalculationMethods) for *control surface integration* in your experimental models. [Quadratic loads](Results,Quadraticloads.htm) are independent of the free-surface data so this avoids unnecessary computation.

* As a general rule, panel size in $PZ$ should match the panel size in the body mesh. It is advisable to refine (or coarsen) both meshes in parallel.
* As a general rule, $R\_{IC}$ should be large enough to ensure that $QZ$ is not [too close](Theory,Second-orderequations.htm#SourcesOfError) to a body surface. The separation should be a distance equivalent to at least 5-10 panels.
* The following is a *suggested* recipe of free-surface data for a first experimental model. This provides a starting point for your experiments, which will subsequently test sensitivity to the various data choices:

+ In $PZ$, use [automatically generated](#FreeSurfacePanelledZone) panels with a panel size $\Delta$ that matches the body mesh. Set the inner radius to $R\_{IC} = R\_{SB} + 10\, \Delta$
+ Estimate an appropriate outer radius as $R\_{OC} = R\_{SB} + \min(h, \lambda\_\max)$. Here $\lambda\_\max$ is the longest wavelength in the model (including both first-order and second-order waves) and $h$ is the water depth.
+ In $QZ$, set the [number of annuli](#QTFQuadratureZone) to $(R\_{OC} - R\_{IC}) / (2 \lambda\_\min)$. Here $\lambda\_\min$ is the shortest wavelength in the model (including both first-order and second-order waves). Set the radial step to $(R\_{OC} - R\_{IC}) / (\textit{number of annuli})$ in order to achieve an outer radius of $R\_{OC}$, and set the number of radial nodes to 12. Set the number of azimuthal nodes to $12 \pi R\_{IC} / (2^\psi \lambda\_\min)$, where $\psi$ is the number of [global symmetry planes](Data,Bodies.htm#GlobalMeshSymmetry) (hence $\psi =$ 0, 1 or 2).
+ On the outer circle, set the [number of segments](#QTFOuterCircle) equal to $12 \pi R\_{OC} / (2^\psi \lambda\_\min)$.
+ In $AZ$, set the [expansion order](#QTFAsymptoticZone) to 24.

Using your first experimental model as a starting point, you can test the quality of the results by running further experiments. The goal is to achieve robust results which are not sensitive to small changes in the free-surface data. We suggest performing the sensitivity tests listed below. In the first instance we suggest performing these tests in the order given, and independently of each other. Testing simultaneous changes to multiple parameters, which is more complicated and more time-consuming, may not be necessary if satisfactory results can be found via this approach.

* Test that the number of radial nodes is sufficiently large in $QZ$. Results are typically insensitive to this parameter provided it is greater than a threshold value, so the procedure is to increase the number of nodes until the potential load results are stable.
* Test that the number of azimuthal nodes is sufficiently large in $QZ$. Increase the number of nodes until the potential load results are stable.
* Test that the expansion order is sufficiently large in $AZ$. Increase the value until the potential load results are stable.
* Test that the number of segments is sufficiently large in the outer circle. Increase the number until the potential load results are stable.
* An important, and more challenging, task is to identify a value of $R\_{OC}$ that is sufficiently large. There is no general-purpose formula for the optimal value, but estimates for the appropriate magnitude can be inferred from the [approximation errors](Theory,Second-orderequations.htm#SourcesOfError) in $AZ$. The easiest way to test $R\_{OC}$ is to increase the number of annuli in $QZ$.
* Test that $QZ$ is sufficiently far from the body surface(s) by increasing $R\_{IC}$. You may want to adjust the radius step in $QZ$ to keep $R\_{OC}$ fixed, but this is not necessary if you have already confirmed that your results are insensitive to $R\_{OC}$.

|  |  |
| --- | --- |
| Note: | Comparing potential load results between different models is the best way to evaluate convergence. Comparing between the [direct](Results,Potentialloads.htm#DirectMethod) and [indirect](Results,Potentialloads.htm#IndirectMethod) methods gives an indication of discretisation error due to the body mesh, but it **cannot** confirm the validity of the free-surface data. |

The experiments above are designed to find free-surface data which give robust potential load results. In other words, if the sensitivity tests are satisfactory you can be confident that further refinement of the free-surface data is not required. The remaining error will simply be the discretisation error inherent from your body mesh.

When you are happy with the robustness of the potential load results in your experimental model, you may want to experiment further – now with the objective to reduce model run time. The idea is to run sensitivity tests to discover if run time can be reduced whilst preserving the quality of the potential load results. It is recommended that these tests are performed on your experimental model *before* you reintroduce the full complexity of your production model (i.e. the full set of wave frequencies and headings and, if relevant, control surface integration). Tests could include:

* Excluding the [indirect calculation method](#QTFCalculationMethod) and using the direct method only.
* Reducing the outer radius.
* In $PZ$, increasing the panel size or reducing the inner radius.
* In $QZ$, reducing the number of radial or azimuthal nodes.

When your experimentation is finished and you return to your production model, performance can also be improved by setting environment data to restrict the number of full QTFs to be calculated. If appropriate for your analysis, specify a [QTF crossing angle range](Data,Environment.htm#QTFCrossingAngles), a [QTF period or frequency range](Data,Environment.htm#QTFPeriodsOrFrequencies) and select a single [QTF frequency type](Data,Environment.htm#QTFFrequencyTypes).
