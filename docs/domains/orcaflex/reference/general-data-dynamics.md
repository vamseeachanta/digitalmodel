# General data: Dynamics

> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)

## Solution method

OrcaFlex offers the following solution methods:

* [explicit time domain](Dynamicanalysis,Timedomainsolution.htm#Explicit) integration
* [implicit time domain](Dynamicanalysis,Timedomainsolution.htm#Implicit) integration
* [frequency domain](Dynamicanalysis,Frequencydomainsolution.htm) solution.

The time domain and frequency domain solution methods are used to solve for the [dynamic response of the system](Dynamicanalysis.htm).

### Explicit time domain integration

The explicit scheme used by OrcaFlex is semi-implicit Euler. Like all explicit schemes, this is conditionally stable. In practice this means that, in order to achieve stability, the [time step](Generaldata,Explicitintegration.htm#GeneralTimeSteps) must be small compared to the shortest natural nodal period. By default OrcaFlex will [automatically set the time step](Generaldata,Explicitintegration.htm#AlwaysUseRecommendedTimeSteps).

### Implicit time domain integration

For implicit integration OrcaFlex uses the [generalised-α](Dynamicanalysis,Timedomainsolution.htm#Implicit) integration scheme, which is unconditionally stable for linear systems. [Constant and variable time step](Generaldata,Implicitintegration.htm#ImplicitIntegrationParameters) options are available. OrcaFlex provides two results, [implicit solver iteration count](Generaldata,Results.htm) and [implicit solver time step](Generaldata,Results.htm), which can be used to track the performance of the implicit integration scheme.

### Limitations of implicit integration

Some of OrcaFlex's features have not been adapted for the implicit scheme. Consequently, implicit integration cannot be used with models that use any of the following features:

* Vessels using calculated (3 DOF) or calculated (6 DOF) [primary motion](Vesseldata,Calculationdata.htm#VesselDataPrimaryMotion) when some [superimposed motion](Vesseldata,Calculationdata.htm#VesselDataSuperimposedMotion) is also applied. Note that implicit integration *can* be used with vessels using calculated primary motion (3 DOF or 6 DOF) when no superimposed motion is applied.
* Tension-controlled detailed [winches](Winches.htm) with non-zero inertia.
* [Time domain VIV models](Timedomainmodels.htm).

### Advantages and disadvantages of time domain schemes

The explicit scheme is extremely robust and flexible. Its main drawback is that the stability requirements can result in very short time steps and correspondingly long computation times. This tends to be more significant for stiff systems, or for systems with fine segmentation. For such systems the implicit scheme can be faster, sometimes by orders of magnitude.

It is essential to consider accuracy as well as computation time. For the explicit scheme, if the simulation is stable then, in our experience, it is rare for the results to be inaccurate. We recommend that you conduct time step sensitivity studies to confirm this.

Implicit schemes, on the other hand, can quite easily achieve stability and yet produce inaccurate results. For rapidly-varying physical phenomena (e.g. snatch loads, impact, sudden [line on line clashing](Linetheory,Clashing.htm) etc.) results accuracy is likely to be an issue. We recommend that time step sensitivity studies are carried out to ensure accuracy of results. Comparisons with the explicit scheme are particularly useful for this purpose.

### Frequency domain solution

In frequency domain analysis, the dynamic response of a [linearised representation of the system](Dynamicanalysis,Frequencydomainsolution.htm#Linearisation) is solved at discrete [user-specified solution frequencies](Generaldata,Frequencydomain.htm#FrequencyDomainSolutionFrequencies).

### Advantages and disadvantages of frequency domain analysis

The advantage of conducting the solution in the frequency domain is that it can be significantly quicker than doing so in the time domain. However, the [theory that underpins](Dynamicanalysis,Frequencydomainsolution.htm) the frequency domain method essentially assumes that both the dynamic loading and the system's response are *linearly* related to some underlying stochastic process (e.g. the wave elevation) and also that they are stationary (i.e. the statistics do not change over time). If the case being studied is well-approximated as a linear stationary system, then the frequency domain solver should offer an accurate and efficient solution method. If there are significant nonlinearities present in the model, then the accuracy of the dynamic solution found by the frequency domain solve will be impaired. Therefore, caution and engineering judgment should be exercised when interpreting frequency domain results.

### Limitations of frequency domain analysis

Some of OrcaFlex's features have not been adapted for the frequency domain solver. These as-yet unsupported features include (but are not limited to):

* vessel [full QTFs](Vesseltypes,WavedriftandsumfrequencyQTFs.htm#FullQTFs)
* [wind loading](Environment,Winddata.htm) on objects other than vessels
* variable drag coefficients (on various model objects)
* updated mean loads and responses in steady current due to drag nonlinearity
* [line contact with containment](Linecontact,Modelling.htm#LineContactContainmentScaling) (line contact without containment *is* supported)
* the [turbine object](Turbines.htm).

If you attempt to use any of these features in a frequency domain analysis, OrcaFlex will report an error. We intend to remove these limitations, where possible, in future releases. Some features are inherently incompatible with the frequency domain and can never be supported. These unsupported features include

* any time-varying or externally-derived variables, e.g. vessel motion prescribed by a time history
* [nonlinear waves](Waves,Nonlinearwavetheories.htm), e.g. Dean stream wave.

Again, if you attempt to use such features in a frequency domain analysis, OrcaFlex reports an error.
