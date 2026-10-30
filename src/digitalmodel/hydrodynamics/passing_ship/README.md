# Passing-ship package report

The [module analysis](../../../../docs/reports/modules/passing_ship/report.html)
documents public entry points and independently checks the shipped normalized
section helpers using synthetic boundary conditions. Its
[report index](../../../../docs/reports/modules/passing_ship/README.md)
contains the derivation, rebuild command and preserved standalone example.

The checked quantities are dimensionless section shape and slope in `m^-1`.
Physical forces, force coefficients, separation/depth validity and full
force-calculator qualification remain **NOT_EVALUATED**. The parent issue
remains open for broader numerical qualification. Configuration, batch and
benchmark interfaces are described by source inspection only.
