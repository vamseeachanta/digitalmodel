# Data: Environment

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

### Water

The water depth and water density.

|  |  |
| --- | --- |
| Note: | When using a finite water depth, the [Green's function](Theory,Green'sfunction.htm#GreensFunctionFiniteDepthWater) imposes [restrictions](Theory,Green'sfunction.htm#FiniteDepthFrequencyRestrictions) on the range of wave frequencies. |

### Waves are referred to by

The waves can be referred to by period in seconds, by angular frequency in radians/second or by frequency in hertz.

### Wave periods or frequencies

The periods or frequencies of the incident waves according to your preferred convention.

### Wave headings

The direction in which the wave progresses, measured positive anti-clockwise from the global X-axis when viewed from above. So, for example, 0 degrees means a wave travelling in the positive $X$-direction, and 90° means a wave travelling in the positive $Y$-direction.

### QTF crossing angles

The range of crossing angles to be included in the calculation of [second-order loads](Theory,Second-orderequations.htm#SecondOrderLoads), including both full QTFs and mean drift loads (also known as Newman QTFs). Using these values to reduce the number of QTFs calculated, e.g. if you only need unidirectional QTFs, will improve the run time.

The crossing angle between two incident waves is defined as $|\Delta\beta|$. Here $\Delta\beta$ is the difference, expressed in the range $-180\degree \le\ \Delta\beta \le +180\degree$, between the headings, $\beta\_1$ and $\beta\_2$, of the two incident waves.

|  |  |
| --- | --- |
| Tips: | To include second-order loads for **all** combinations of wave headings, set the minimum and maximum crossing angles to $0\degree$ and $180\degree$, respectively. |
|  | To include second-order loads only for **unidirectional** waves, set both the minimum and maximum crossing angles to $0\degree$. |

If you plan to import full QTF results into OrcaFlex, you should consider whether you want to produce unidirectional or bidirectional QTFs.

|  |  |
| --- | --- |
| Notes: | If the OrcaWave results contain any QTFs with $\beta\_1\neq\beta\_2$, they will be treated as [bidirectional QTF data](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Vesseltypes,WavedriftandsumfrequencyQTFs.htm#FullQTFs) by OrcaFlex. Any QTFs that are required by OrcaFlex (to form a complete interpolation grid) and were excluded from the OrcaWave calculation will be populated with zero values on import into OrcaFlex. |
|  | If the OrcaWave results all have $\beta\_1=\beta\_2$, they will be treated as [unidirectional QTF data](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Vesseltypes,WavedriftandsumfrequencyQTFs.htm#FullQTFs) by OrcaFlex. |

### QTF periods or frequencies

The range of second-order periods or frequencies to be included in a full QTF calculation. Using these values to reduce the number of QTFs calculated, e.g. if you only need QTFs at periods close to a natural resonance, will improve the run time.

|  |  |
| --- | --- |
| Notes: | The second-order periods or frequencies are also affected by the choice of **QTF frequency types**. |
|  | The OrcaWave results can be imported into OrcaFlex as [full QTF data](https://www.orcina.com/webhelp/OrcaFlex/Redirector.htm?Vesseltypes,WavedriftandsumfrequencyQTFs.htm#FullQTFs). Any QTFs that are required by OrcaFlex (to form a complete interpolation grid) and were excluded from the OrcaWave calculation will be populated with zero values on import into OrcaFlex. |

### QTF frequency types

The types of [second-order frequencies](Theory,Second-orderequations.htm) (sum frequencies and/or difference frequencies) to be included in a full QTF calculation.

### Include mean drift full QTFs

If you include only *sum frequencies* in your **QTF frequency types**, or if your range of **QTF periods or frequencies** does not include zero frequency (infinite period), the mean drift QTFs would naturally be excluded from the OrcaWave calculation. This option allows you to include the mean drift QTFs anyway, overriding those other data.

|  |  |
| --- | --- |
| Tip: | Mean drift QTFs may, optionally, contribute to determining the static equilibrium position of a vessel in OrcaFlex. It may be desirable to include the mean drift QTFs for this reason, even if they are not relevant for the dynamic analysis you intend to perform. |

### Wave spectrum for drag linearisation

When this option is selected the model has a wave spectrum that describes the sea state and is used to linearise [hydrodynamic drag](Theory,Hydrodynamicdrag.htm). The wave components of the spectrum are used to evaluate expectations in the [stochastic linearisation](Theory,Hydrodynamicdrag.htm#DragLinearisation). The form of the spectrum is specified on the [wave spectrum](Data,Wavespectrum.htm) page.

The underlying diffraction calculation is performed, as usual, for the wave [periods or frequencies](#WavePeriodsOrFrequencies) and [headings](#WaveHeadings) specified above. Typically, the wave components of the spectrum require different periods and/or headings. Results at the periods and headings of the wave components are obtained by 2-dimensional (frequency and heading) interpolation of the diffraction results.

* Interpolation on frequency uses linear interpolation with truncation. OrcaWave will not extrapolate beyond the frequencies in the diffraction results.
* Interpolation over heading uses linear circular interpolation. It treats the headings as being arranged around the full circle of possible directions (modulo 360°), and linearly interpolates all values within that circle.

|  |  |
| --- | --- |
| Note: | Interpolation over heading is only required if the [number of wave directions](Data,Wavespectrum.htm#NumberOfWaveDirections) in the wave spectrum is greater than 1. |

### Drag linearisation parameters

The maximum number of iterations for [drag linearisation](Theory,Hydrodynamicdrag.htm#DragLinearisation). The calculation is abandoned if convergence has not been achieved after this number of iterations.

The tolerance used to determine whether drag linearisation has [converged](Theory,Hydrodynamicdrag.htm#ConvergenceCriteria).
