# RAOs


## Types

- Displacement RAOs
- Load RAOs
- Seastate/Distrubance RAOs

### Displacement RAOs

Displacement RAOs are produced using small displacement theory. Non-linearities are not taken into account.

Data Headers:
Direction, period, DOF 1 (Amplitude, Phase), DOF 2 (Amplitude, Phase) etc.

### Load RAOs

For accurately modelling the loads on the vessel. Ex: Simulate vessel motions when high tension/stiffness moorings and risers are connected.

During simulations, the wave load RAOs models take longer time to settle down to a solution

Data Headers:
Direction, period, DOF 1 (Amplitude, Phase), DOF 2 (Amplitude, Phase) etc.

### Seastate/Distrubance RAOs

Presence of each of these objects will modify the undisturbed sea due to interactions between the objects and the passing waves (e.g. wave radiation and diffraction). Relatively small objects, such as small buoys, will cause only minimal disturbance to the sea state, but large objects such as vessels can significantly disturb the sea state that is experienced by other objects nearby.

[Seastatedisturbance RAOs](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,Seastatedisturbance.htm)

Data Headers:
Direction, Period, Vessel Position (x, y, Z), Velocity Potential Disturbance RAOs (Amplification Factor, Phase)

## RAO Checks

Displacement and Loads follow same patterns

How about seastate RAOs potential? extremes should be 1 and highest velocity/acceleration should follow vessel?

https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,RAOqualitychecks.htm