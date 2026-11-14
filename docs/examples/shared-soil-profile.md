# Shared soil profiles

`digitalmodel.geotechnical.SoilLayer` is the shared frozen record;
`digitalmodel.geotechnical.spudcan.SoilLayer` remains the native spudcan record.
The version-1 text codec uses `load_soil_profile(text)` and `dump_soil_profile(profile)`.
Files use UTF-8/LF. YAML requires `schema_version: 1`, `units: SI`, and `layers`.
Each layer requires `kind` (`clay`, `sand`, `unknown`), `top_m`, and `bottom_m`.
Optional fields are `su_pa`, `cohesion_pa`, `friction_deg`, `bulk_weight_n_m3`,
and `submerged_weight_n_m3`. Depths are metres below mudline; strengths are Pa,
weights N/m³ and angles degrees. Layers are contiguous from zero. Null means
missing; zero is supplied. Undrained and drained strengths cannot be combined.
Optional `cpt` uses existing `CPTData` fields: `depth_m`, `qc_pa`, `fs_pa`,
`u2_pa` (nullable entries), and required `area_ratio`; omitted `u2_pa` remains missing.
Duplicate/unknown keys, object tags, invalid units and nonfinite values are rejected.

`soil_profile_adapters.from_cpt(data, layers)` attaches the existing CPT object;
`to_cpt(profile)` returns it without classification or inferred strengths.
`to_spudcan_layers(profile)` returns native tuples with Pa and N/m³ divided by 1000;
submerged weight and the kind's strength are required. Sand is cohesionless surface
sand over clay. Caller-selected method settings and geometry checks remain necessary.
`to_slope_profile(profile, x_extent_m=(-10, 30), mudline_elevation_m=100,
water_unit_weight_n_m3=9810, circles=[dict(x_m=10, y_m=105, radius_m=20)])`
requires separate bulk weight and explicit Su or cohesion/friction. Elevations are
mudline elevation minus depth. Only horizontal constant layers are supported;
varying boundaries, gradients and saturated weight are omitted. No solver is imported.
These records do not implement correlations, cyclic curves, GEF or AGS4 ingestion.
