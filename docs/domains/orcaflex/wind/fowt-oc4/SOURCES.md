# Public reference data: NREL 5 MW on the OC4-DeepCwind semi-submersible

`basis.yml` and `generator_data.yml` in this folder are written by
`scripts/fowt/extract_oc4_generator_data.py`. Every value carries its unit and
source. No figure is digitised and no source document is redistributed.

## Sources

1. Robertson, A., Jonkman, J., Masciola, M., Song, H., Goupee, A., Coulling, A.,
   and Luan, C. (2014). *Definition of the Semisubmersible Floating System for
   Phase II of OC4*. NREL/TP-5000-60601.
   <https://docs.nlr.gov/docs/fy14osti/60601.pdf>, retrieved 2026-11-12, HTTP 200,
   SHA-256 `ccb0f7cdc001cfc41f3a32abc858c5ab0fca2564152b0ed81a0f56781913a1a0`.
   The following values are transcribed with table and printed page:
   - Table 2-2: tower mass and centre of mass;
   - Table 3-1 and 3-2: platform geometry;
   - Table 3-3: platform mass, centre of mass and inertia, including ballast;
   - Table 4-6: water density and depth, displacement, centre of buoyancy,
     buoyancy, hydrostatic restoring and additional quadratic drag;
   - Table 5-1: mooring properties;
   - Eq. (5-14) and (5-15): mooring vertical preload and linearised stiffness.
2. Robertson, A., et al. (2014). *Offshore Code Comparison Collaboration
   Continuation Within IEA Wind Task 30: Phase II Results Regarding a Floating
   Semisubmersible Wind System*. NREL/CP-5000-61154.
   <https://docs.nlr.gov/docs/fy14osti/61154.pdf>, retrieved 2026-11-12, HTTP 200,
   SHA-256 `09e2e26b87f2a36e188388973d715d90387720b8bf07b53dbccb523a1b86b0b7`.
   The paper quotes the surge, pitch and heave natural frequencies in its text
   (0.01, 0.04 and 0.058 Hz). They are used only as rounded comparators.
3. OpenFAST/r-test at revision `dd5feaaaa500ba7283140107806300d551cff0a7`,
   Apache License 2.0 (`LICENSE` at that revision). Files under
   `glue-codes/openfast/`, retrieved 2026-11-12 from
   `https://raw.githubusercontent.com/OpenFAST/r-test/<revision>/<path>`, HTTP 200.
   Their SHA-256 digests are recorded in `generator_data.yml`:
   - `5MW_OC4Semi_Linear/NRELOffshrBsline5MW_OC4DeepCwindSemi_ElastoDyn.dat`,
     `..._ElastoDyn_Tower.dat`, `NRELOffshrBsline5MW_Blade.dat`: rotor, nacelle,
     blade and tower properties;
   - `5MW_Baseline/HydroData/marin_semi.hst` and `marin_semi.1`: WAMIT
     hydrostatics, added mass and radiation damping;
   - `5MW_OC4Semi_Linear/..._HydroDyn.dat` and `..._MAP.dat`: WAMIT length scale,
     displaced volume and mooring azimuths;
   - `5MW_OC4Semi_Linear/5MW_OC4Semi_Linear.1.lin`: the OpenFAST linearisation.
     The extraction takes its state-matrix eigenvalues as comparator natural
     periods.

## Notes

- The r-test ElastoDyn platform mass (3,852 t) is the steel only. HydroDyn adds
  the 9,621 t water ballast through flooded members. The rigid model uses the
  report's Table 3-3 platform properties, which include ballast.
- The r-test mooring input uses an unstretched length of 835.35 m; the report
  gives 835.5 m, which the model uses. The shorter length shortens the surge
  period by 0.6 s in the licence-free checks.
- Nacelle roll and pitch inertia are not published. They are set equal to the
  nacelle yaw inertia about its own centre, 1.74e6 kg m², which is 0.02 % of the
  system pitch inertia.
- The rotor's transverse inertia is taken as half its polar inertia. That
  relation holds for three equally spaced blades. The tilt rotation of the rotor
  tensor is neglected.
- The reference design publishes no mooring minimum breaking load. No
  utilisation is computed.
