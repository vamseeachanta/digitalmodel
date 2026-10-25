# Authored riser campaign basis

Version: screening-basis-v1; date: 2026-10-25; terms: repository MIT licence.

## Assumed screening basis and parameter ranges

**Every selected engineering quantity is ASSUMED**, including zeros, all RAOs,
all material properties and inherited builder defaults. Group-level `basis` and
`source_id` fields apply to every number in that group. Public sources bound
concept dimensions only. Scaled dimensions and synthetic motions do not describe
an actual floater, production system or site. BSEE records are not used, and no
claim about operating populations or water-depth distributions is made.

| Floater | Small → large particulars (m) | Range derivation and public reference |
| --- | --- | --- |
| FPSO | Length 175 → 350; beam 32 → 64; moulded depth 16.5 → 33; draft 10 → 20 | ASSUMED factors 0.5 and 1.0 on public hull length, beam and moulded depth; draft is independently ASSUMED. [Hull technical description, Key Figures](https://www.modec.com/business/service/floater/fpso/hull/m350.html) |
| Semi-submersible | Envelope length/beam 74 → 88.8; draft 20 → 24; depth 32 → 38.4; column diameter 12 → 14.4 | ASSUMED factors 1.0 and 1.2; square envelope is column spacing plus base diameter, not an exact hull plan. [Report 60601, section 3, Table 3-1](https://docs.nlr.gov/docs/fy14osti/60601.pdf) |
| Spar | Length/beam/column diameter 9.4 → 11.28; draft 120 → 144; depth 130 → 156 | ASSUMED factors 1.0 and 1.2 on the lower diameter, draft and draft plus freeboard. Length and beam denote cylindrical envelope. [Report 47535, section 3, Table 3-1](https://docs.nlr.gov/docs/fy10osti/47535.pdf) |
| TLP | Length/beam/column diameter 18 → 21.6; draft 47.89 → 57.468; depth 57.89 → 69.468 | ASSUMED factors 1.0 and 1.2 on the cylinder diameter and draft; freeboard 10 m is ASSUMED before scaling. [Report 45891, section 3.2, Table 2](https://docs.nlr.gov/docs/fy10osti/45891.pdf) |

Table 2. Public reference concepts bound ASSUMED dimensions; wind-platform reference dimensions do not establish production-platform motions.

Public dimension references are identified by document and section above;
response digests and retrieval evidence are recorded in the campaign manifest.
The references supply dimension facts only; no source document, image, measured
RAO, client input or licensed standard is redistributed by this campaign.

Other ASSUMED inputs and criteria are:

- Small/large pipe outer diameters: 0.2731/0.3239 m; wall: 25.400 mm, without corrosion allowance or coating.
- Steel density: 7850 kg/m³; contents density: 800 kg/m³; seawater: 1025 kg/m³; Young's modulus: 207 GPa; Poisson ratio: 0.3 (dimensionless). No material grade or yield limit is assigned.
- Gravity: 9.80665 m/s²; hang-off: 10 degrees from vertical at mean sea level; grounded tail: one third of water depth. Anchor centreline is one bare-pipe radius above the seabed; catenary height subtracts that radius from water depth.
- Lazy-wave sag/hog elevations: 0.1/0.4 of centreline vertical span; buoyancy factor: 2.0 (dimensionless); module density: 400 kg/m³. The factor is uplift divided by bare submerged weight; buoyed net weight is negative bare weight. Equivalent annular module volume, OD and dry mass are calculated to preserve that weight, while pipe EI, EA and stress diameters remain unchanged.
- Current: surface speed 0.5 m/s, direction 0 degrees; depth fractions 0, 0.5, 1.0 have speed factors 1.0, 0.5, 0.1. Depths are measured positively downward from mean sea level. Wind is zero.
- Normal and shear seabed stiffness: 100 kN/m³; friction: zero; normal drag coefficient: 1.2; normal added mass coefficient: 1.0; axial coefficients: zero. These are uncalibrated defaults, not qualified soil or hydrodynamic data.
- Target mesh length: 5 m; stage durations: 20 s build-up and 200 s response; implicit constant step: 0.1 s. Mesh, duration, time-step and seed convergence remain unestablished.
- Geometry closure and installed-length tolerances: 0.000001 m; hang-off must be between 0 and 45 degrees; grounded tail and hog clearance must exceed zero. These are geometry-screen criteria, not strength criteria.

The eight RAO sets are **ASSUMED synthetic tables**, one per floater/size pair.
DOF order is surge, sway, heave, roll, pitch, yaw. Translation is m/m; rotation
is deg/m, both per unit wave amplitude; phases are degrees lagging a wave crest
at the reference origin. Axes are forward, port and up; positive rotations lower
the starboard side, lower the bow and turn the bow to port. The reference and
phase origins are at (0, 0, 0) m at mean sea level. Only heading 0 degrees is
supported; unsupported headings and wave periods outside the table are rejected.

Base amplitudes for the small FPSO, semi-submersible, spar and TLP are,
respectively, `(0.3, 0, 0.8, 0, 0.8, 0)`, `(0.2, 0, 0.4, 0, 0.3, 0)`,
`(0.1, 0, 0.2, 0, 0.2, 0)` and `(0.2, 0, 0.05, 0, 0.05, 0)` in the DOF units
above. These amplitudes are multiplied by ASSUMED factors
`(0.05, 0.3, 0.7, 0.9, 1.0, 1.0)` at periods `(5, 8, 12, 16, 20, 25)` s.
Large-class amplitudes are multiplied by another factor of 0.8. All phases are
ASSUMED zero degrees. These tables are an explicit sensitivity exercise; they
are not diffraction results or physically qualified long-period limits.

The vessel receives prescribed RAO motion; hull hydrostatics, mooring dynamics
and TLP tendons are not calculated. Principal particulars identify the assumed
size class; vessel length is carried into the native model, with equal reference
and instance lengths to avoid additional length scaling. Beam, draft and depth
are descriptive manifest data, not inputs to a coupled hull model. Near/far
`x_m` denotes a change in radial span: vessel surge is its negative, while the
anchor remains at its nominal global position. The native line starts at the
vessel origin; the top connection is rotationally free.
