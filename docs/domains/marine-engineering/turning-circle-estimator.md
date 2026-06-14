# Turning Circle Estimator

The turning-circle estimator is a preliminary first-order Nomoto workflow for
constant-speed, constant-rudder sensitivity studies. It integrates
`r_dot = (K * delta - r) / T`, then reports advance and transfer from the
interpolated 90 degree heading crossing and tactical diameter from the
interpolated 180 degree heading crossing.

This is not MMG and not a full MMG maneuvering model. The packaged typical-ship
YAML uses assumed Nomoto `K` and `T` values; it is not calibrated from sea-trial
or telemetry evidence.

The output may reference IMO turning-circle terminology and ABS/class context
for reader orientation, but it is not an IMO maneuvering compliance assessment
and it makes no ABS or class compliance conclusion.

Primary artifacts:

- `turning_circle_time_history.csv`
- `turning_circle_metrics.csv`
- `turning_circle_results.json`
- `turning_circle_provenance.json`
- chart files for trajectory, yaw rate, heading, and metrics sensitivity
