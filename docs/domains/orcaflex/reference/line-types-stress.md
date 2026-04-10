# Line types: Stress data

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

$\newcommand{\Te}{T\_\mathrm{e}}$
$\newcommand{\Tw}{T\_\mathrm{w}}$
$\newcommand{\po}{p\_\mathrm{o}}$
$\newcommand{\peei}{p\_\mathrm{i}}$
$\newcommand{\ao}{a\_\mathrm{o}}$
$\newcommand{\ai}{a\_\mathrm{i}}$

## Stress outer and inner diameter

The stress diameters are the inside and outside diameters of the load-bearing cylinder. They are used in the [wall tension](Linetheory,Calculationstage1tensionforces.htm) and [stress results](Linetheory,Pipestresscalculation.htm) calculations, which are based on the assumption that the loads in the line are taken by a simple homogeneous cylinder. For simple cases, the stress diameters can be set to '~', in which case they will be taken to be the same as the pipe diameters. For more complex cases, for example where the pipe outside diameter allows for added buoyancy modules that are not load bearing, the stress diameters can be set explicitly.

## Allowable stress

The maximum allowable stress for this type of line. This value is *only* used to draw a limit curve on stress [range graphs](Results,Rangegraphs.htm); it does *not* limit the stress achieved in the line. A value of ~ here tells OrcaFlex to not draw any limit curve.

## Stress loading factors

The stress loading factors define the proportion of each load (wall tension, bend moment, shear and torque) to be used in calculating [stress results](Linetheory,Pipestresscalculation.htm) and [code check results](Codechecks.htm). Each of wall tension, bend moment, shear force and torque is multiplied by the corresponding stress loading factor in these results calculations.

The reported [wall tension result](Lineresults,Forces.htm#LineResultsWallTension) is scaled by the tensile stress loading factor.

The stress loading factors also influence the effective tension used in calculating the [code check results](Codechecks.htm). The value of effective tension used in the code checks is calculated from the total, unscaled, wall tension, $\Tw$, as
\begin{equation}
\Te = C\_1 \Tw - (\peei\ai - \po\ao)
\end{equation}
where $C\_1$ is the tensile stress loading factor. See [line pressure effects theory](Linetheory,Linepressureeffects.htm) for more details.

For many cases, such as a simple homogeneous pipe that carries all the loads, these factors should be set to their default value of 1.

In some cases, values less than 1 may be appropriate. For example, consider a line representing a composite structure consisting of a main carrier pipe and an external piggyback pipe. You might estimate that the main pipe takes all of the tensile and torsional loads, but only carries 70% of the bending loads, the other 30% being taken by the piggyback pipe. To obtain stress estimates for the main pipe you would then set the stress outer and inner diameters to ~ and the bending and shear stress loading factors to 0.7.

|  |  |
| --- | --- |
| Note: | The stress loading factors only affect the [wall tension results](Lineresults,Forces.htm#LineResultsWallTension), [stress results](Lineresults,Stressstrain.htm), [fatigue analyses](Fatigueanalysis,Introduction.htm) and [code check results](Codechecks.htm). These results are derived after the simulation has completed: the stress loading factors do not affect the progress of the simulation. This means that, unlike most data items, OrcaFlex allows you to modify these values after a simulation has been run. |

## Additional bending stiffness

If [additional bending stiffness](Linetypes,Structuredata.htm#HomogeneousPipeAdditionalBendStiffness) is non-zero, OrcaFlex will automatically determine what proportion of each load (wall tension, bend moment, shear and torque) should be considered to contribute to the [stress results](Linetheory,Pipestresscalculation.htm) and [code check results](Codechecks.htm), so that the reported loads are those carried only by the original line type structure and exclude the proportion due to the additional stiffness.
