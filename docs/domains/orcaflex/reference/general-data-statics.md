# General data: Statics

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Buoy degrees of freedom included in static analysis

[3D buoys](3Dbuoys.htm) and [6D buoys](6Dbuoys.htm) can either be included or excluded from the [static analysis](Staticanalysis.htm). When a buoy is included OrcaFlex calculates the static equilibrium position of the buoy; when it is excluded OrcaFlex simply leaves the buoy at the position specified by the user.

Which buoys are included in the static analysis is controlled by setting the buoy degrees of freedom included in static analysis (on the general data form), together with individual settings on each buoy's data form:

* **None** means that OrcaFlex does not find the true static equilibrium position of any of the buoys in the model, but instead simply places them all at their initial starting positions given by the buoy data.
* **X, Y, Z** includes all buoys in the statics calculation, but excludes rotational [degrees of freedom](6Dbuoys,Commondata.htm#6DBuoysDOFinAnalysis) for 6D buoys.
* **Individually specified** transfers control to the buoy data form, where each buoy may be included in, or excluded from, the static equilibrium calculation. With 6D buoys, you can also choose whether the rotational [degrees of freedom](6Dbuoys,Commondata.htm#6DBuoysDOFinAnalysis) are included or excluded.
* **All** is the usual setting, in which case the static analysis will include all the buoys along with the other objects in the model when it finds their static equilibrium positions.

You may need to set this value to none if the model is statically indeterminate, for example a free-floating buoy, or if the static analysis fails to converge for some other reason. The static analysis is an iterative calculation and, for some complex systems, may fail to converge, especially if the given initial position is far from the equilibrium position.

If statics fails, you can exclude some or all buoys (or, for 6D buoys, just the rotational degrees of freedom) from the static analysis; this simplifies the calculation and should allow convergence. Although the subsequent dynamic simulation then starts from a non-equilibrium position, it does allow the simulation to proceed and the initial non-equilibrium errors will normally be dissipated during the build-up period of the simulation with a reasonably long build-up period. In fact a modified dynamic simulation can then often be used to find the true static equilibrium position, by running a simulation with no waves; the results of this simulation give the true static equilibrium positions of the buoys, which can then be used as their starting positions for subsequent simulations with waves reinstated.

Another reason for excluding all buoys from statics is to deliberately start the simulation from a non-equilibrium position. You might do this, for example, in a simulation with no waves to determine the damping properties of the system.

## Starting velocity

The velocity of the whole model for the static analysis and the start of the simulation. It is defined by giving the speed (i.e. magnitude) and [direction](Directionconventions.htm).

Normally the starting speed is zero. For non-zero speed (e.g. for modelling a towed system), the static analysis becomes a steady-state analysis that finds the steady-state equilibrium position in which the whole model is moving with the given velocity. The static position is therefore then referred to as a *steady-state position*, and the calculation of this position allows for any drag loads due to differences between the starting velocity and the current or wind velocities.

|  |  |
| --- | --- |
| Note: | The model will start the simulation from the calculated steady state; i.e. with the specified starting velocity. So you should normally ensure that each vessel in the model has its prescribed motion for the first stage set to match the specified starting velocity. Otherwise the simulation will start with a sudden change in vessel velocity, which will cause a "kick" which may take some time to settle down. |

## Line statics step 1 policy

The line statics step 1 policy determines which lines have their [step 1 statics](Linedata,Statics.htm#LineStaticsStep1Method) calculated and which do not, with reference to lines which are connected to other lines. The policies that can be adopted are

* **None**: no lines are included in step 1 statics.
* **Parent lines excluded**: step 1 statics will only be calculated for lines that do not have other lines connected to them as [children](Objectconnections.htm), either directly or indirectly.
* **All lines included**: all lines are included in
  step 1 statics.

## Line statics step 2 policy

The line statics step 2 policy determines which lines have their [step 2 statics](Linedata,Statics.htm#LineStaticsStep2Method) calculated, in particular where lines that are connected to other lines. The available policies are

* **None**: no lines are included in step 2 statics, even if an individual line is set to use [full statics](Linestatics,Fullstatics.htm).
* **Parent lines excluded**: step 2 statics will only be calculated for lines that do not have other lines connected to them as [children](Objectconnections.htm).
* **Solve coupled systems**: OrcaFlex will identify coupled groups of lines and perform the step 2 statics calculation on each of these groups as a whole.

|  |  |
| --- | --- |
| Note: | The default policies will usually be appropriate. The [line statics theory](Linestatics.htm) topic has more details on these policies, and an [article discussing the motivation for them can be found on our website](https://www.orcina.com/news/upcoming-in-orcaflex-10-2-line-statics-policy/). |

## Whole system statics enabled

This option allows you to suppress the [whole system statics](Staticanalysis.htm#WholeSystemStatics) step.

Usually whole system statics should be calculated because it brings the entire system into a static equilibrium. However, there are situations where it may be desirable to skip this step, usually because you wish to start dynamics from a non-equilibrium state, for instance to perform analysis of rate of decay.

|  |  |
| --- | --- |
| Note: | Disabling whole system statics and setting both line statics step 1 and 2 policies to **None** is not equivalent to [disabling statics](Generaldata,Analysis.htm#AnalysisStaticsEnabled) altogether. The former still runs through all the various machinery and checks that a normal static calculation would perform in order to define a static state; it is just that it skips the actual solve (so that the result will not, generally, be a static equilibrium). The latter skips the static step entirely (the option to [run statics](Calculationmenu.htm#MenuCalculationStatics) on the calculation menu is disabled) and [static state results](Results,Staticstatetables.htm) are not available. One way to describe the former option would be **suppressing statics**, whereas the latter option would be **disabling statics**. |

## Whole system statics convergence parameters

When buoys or vessels are included in the [static analysis](Staticanalysis.htm), their equilibrium positions are calculated using an iterative algorithm that is controlled by these convergence parameters. These parameters do not normally need to be altered. However if the statics calculation fails to converge it is sometimes possible to improve the behaviour by adjusting them.

### Max iterations

The calculation is abandoned if convergence has not been achieved after this number of steps. For some difficult cases simply increasing this limit may be enough.

### Tolerance

This controls the accuracy of the solution, for [lines](Linedata,Propertiesreport.htm#LineDataFullStaticsAccuracies), [3D buoys](3Dbuoys,Propertiesreport.htm#3DBuoysStaticsAccuracies), [6D buoys](6Dbuoys,Propertiesreport.htm#6DBuoysStaticsAccuracies), [vessels](Vesseldata,Propertiesreport.htm#VesselStaticsAccuracies) and [constraints](Constraints,Propertiesreport.htm#ConstraintsStaticsAccuracies).

|  |
| --- |
| In addition to the tolerance, the accuracy of the analysis is also affected by the [characteristic lengths and forces](Allobjectsdataform.htm#AllObjectsCharacteristicScales) associated with the objects in the model. |

### Min damping, max damping

For some cases it is necessary to control the convergence process by damping down (i.e. reducing) the step taken at each stage. OrcaFlex includes an automatic damping system that chooses a suitable damping factor for each iteration, but you can set the minimum damping and maximum damping factors to limit this.

Normally the default values will suffice but for difficult cases the default values can be changed. For cases that appear to make the convergence unstable (e.g. displaying implausible line configurations during the simulation), try increasing the **min damping** factor to a value greater than 1, up to, say, 10. You can also try increasing the **max damping** factor, from 10 to a value up to, say, 100.

|  |  |
| --- | --- |
| Note: | Convergence will be slower with larger damping values so you should aim to use the smallest values that result in statics converging. |
