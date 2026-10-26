# Riser results exchange

The exporter writes `strength.json` and, for a fatigue simulation,
`fatigue-<sea_state_id>.json` from completed simulation results and a frozen case
row. Extraction is read-only. Synthetic fixtures exercise the exchange without a
licensed binding. Native engineering qualification remains outstanding.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-26 | Introduce extraction and validation for consistent downstream results. |

## Run from the simulation loop

Run the command after a completed calculation, inside the loop's existing
interactive desktop process with the licensed Python environment available:

```bash
python -m digitalmodel.riser_results export --sim completed.sim --case R10-W1-SCR --out results/R10-W1-SCR/
```

An operating-system service process or remote launch can fail licence acquisition;
the command does not create an interactive desktop or acquire a different licence.
The equivalent in-process call is `export_results(sim, input_file, case_row,
extraction, out, adapter)`. The loop can inject `NativeAdapter(binding)` using its
already loaded binding. The module's explicit `load_adapter()` factory is the only
place that imports that binding. Importing the package or asking for command help
does not load it. Exporting never invokes a solver calculation.

Use `--manifest extraction.json` to select metadata explicitly. Otherwise the
command reads `completed.export.json` beside `completed.sim`. Relative input
references resolve from the manifest directory. The sidecar contains the frozen
matrix row, the actual simulation input file and the extraction settings. The
following abbreviated row uses frozen matrix values; retain the complete frozen
row in operational sidecars:

```json
{
  "case_row": {
    "case_id": "R10-W1-SCR",
    "outside_diameter_mm": "273.100",
    "wall_thickness_mm": "25.600",
    "checked_wall_mm": "19.4000",
    "fatigue_stationary_seconds": "3600",
    "fatigue_seas": "F03;F04;F05"
  },
  "input_file": "completed.dat",
  "extraction": {
    "line": "riser",
    "analysis_type": "dynamic",
    "load_condition": "extreme",
    "smys_MPa": 450,
    "design_case_factor": 1.2,
    "load_case": {
      "id": "nominal-fatigue-F03",
      "wave": {"toward_deg": 180},
      "current": {"speed_m_s": 0},
      "offset": 0
    },
    "period_s": [150, 3750],
    "contact_tolerance_m": 0,
    "fibres_deg": [0, 90, 180, 270],
    "scf": 1.2,
    "sea_state": {
      "id": "F03",
      "Hs_m": 2.112264,
      "Tp_s": 6.59,
      "direction_deg": 180,
      "occurrence": 0.015
    },
    "variables": {
      "effective_tension": "Effective tension",
      "bending_moment": "Bend moment",
      "von_mises_stress": "Max von Mises stress",
      "top_angle": "Declination",
      "longitudinal_stress": "ZZ stress",
      "clearance": "Seabed clearance"
    }
  }
}
```

The variable names are native result identifiers requiring readback against the
installed version, line formulation and sign convention. `contact_tolerance_m`
and circumferential stations are extraction choices: zero contact tolerance and
four fibres above are examples, not demonstrated convergence. Resolve centreline
versus outer-surface clearance before selecting a contact tolerance. Preserve the
configured stationary interval, seed, wave/current definitions and offset in
`load_case`. A strength simulation omits `sea_state`; a functional static result
sets `analysis_type` to `static`, `load_condition` to `functional` and
`design_case_factor` to 1. Static results carry null dynamic quantities and no
fatigue file.

Each output directory represents **one simulation**, including its load case,
sea and seed. Select distinct directories for different simulations, for example
`results/R10-W1-SCR/nominal-fatigue-F03/`. All files retain `case_id` and
`load_case` identity. A digest-matched completed export is reused without another
native read. A different existing result or partial export is rejected; preserve
it and select a fresh directory. The exporter neither combines strength envelopes
across simulations nor selects a governing simulation for the study.

## Engineering basis and units

SI native outputs are read as m, kN, kN.m, kPa and degrees; other native unit
systems are rejected. The adapter boundary also supports explicit N, N.m, Pa,
MPa, mm and radians, with dimensional conversions tested independently. Range
graph grids must agree exactly; missing curves are not interpolated.

`von_mises_utilisation` uses the larger of static and dynamic maximum equivalent
stress at each station. The allowable is `C_f * (2/3) * SMYS`, labelled
**API RP 2RD legacy WSD**, as carried by
`src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py`,
`ApiRp2rdStrategy`. The supplied study basis sets SMYS to 450 MPa,
functional `C_f=1` and extreme `C_f=1.2`; the resulting extreme allowable is
360 MPa. This reuses the repository label and code basis without claiming a
current standard edition or a complete code check. No licensed standard is used.

The adapter requires explicit native `StressOD` and `StressID` on every line
type. Their checked wall must match `checked_wall_mm` from the frozen row; nominal
wall remains distinct and no further corrosion or tolerance deduction is applied.
Native stress readback must confirm membrane, pressure, true/effective wall force,
biaxial bending and governing fibres. Independent load envelopes are not combined
to invent simultaneous equivalent stress. `envelopes` retains static values and
dynamic minima/maxima; the primary tension and bending arrays are dynamic maxima
(or static values for a static result). Compression remains visible in minima.

Final touchdown is the first station of the contiguous contact tail connected to
the grounded end. Earlier contacts are retained separately. For each sampled
instant, the adapter finds that station from clearance. Signed longitudinal stress
histories are extracted at every fixed material station swept by the final
contact and at hang-off, for every configured outer-surface fibre. Touchdown range
is the largest signed history range, with its physical station and fibre. A
moving spatial maximum and scalar equivalent-stress extrema are never rainflow
inputs. All stations of the contact zone are required; omitted or duplicate
station/fibre histories cause failure. Spatial contact and circumferential
resolution require later convergence checks.

Rainflow reuses the existing stack kernel extracted from
`src/digitalmodel/structural/fatigue_apps/rainflow_counter.py` into
`src/digitalmodel/structural/rainflow_cycles.py`. The earlier public wrapper still
delegates to this same kernel. Full counts remain 1 and residual half counts 0.5;
plateaux are reduced to reversals before counting. The source module cites
ASTM E1049-85; no standard document is opened or reproduced.

Fatigue uses the supplied [public FAT80 comparison](https://strathprints.strath.ac.uk/52862/),
section 3.2.1 and Figure 16: range 80 MPa at 2 million cycles, slope 3. The study
extends `N = 2000000 * (80 / range_MPa)^3` with one slope and no endurance cutoff.
Configured SCF multiplies ranges once. The supplied benchmark assumes SCF 1.2,
no mean-stress benefit and unity thickness correction, with weld and environment
treatment unqualified. Exact range/count histograms avoid damage error from
bin-centre substitution. Annual damage is history damage times
`8760 * 3600 / duration_s * occurrence`; occurrences are fractions without
renormalisation. The recorded interval must cover the frozen stationary duration.
The three selected seas cover 3.8% of a year in the supplied basis.

Every station/fibre retains its histogram and annual contribution. The document's
`damage_per_year` is the maximum station/fibre contribution for **this sea**.
Consumers must sum sea contributions at the **same station and fibre first**,
then take the spatial maximum; adding independent per-sea maxima is invalid.
`life_years` is only reciprocal benchmark annual damage and is null at zero damage.
This is **air-equivalent benchmark damage**, with no service-life qualification:
**screening sensitivity: three sea states, not a full fatigue analysis**.

## Shared validation and provenance

Downstream consumers import `read_document` and `validate_document` from
`digitalmodel.riser_results`. Call `validate_document(document, "strength")` or
`validate_document(document, "fatigue")` after reading. These checks enforce
required schema fields, finite numbers, aligned increasing arcs, extrema,
allowable/method labels, timestamps, digests, occurrence fractions, both hotspot
regions, unique material points and reciprocal-life consistency. JSON with
nonfinite constants or duplicate keys is rejected. Additional evidence fields
are permitted so consumers can retain provenance.

`run` records SHA-256 of the input and result bytes, result basename, UTC timestamp,
frozen matrix-row digest and extraction-settings digest. Input and result bytes
are checked again after extraction to detect concurrent changes. No absolute
source location is written into `run`. Files are validated before output creation
and opened exclusively to avoid replacing existing data. The two-file write is
not a filesystem transaction; an interruption may leave a partial export, which
is detected on a retry.

## Checks and remaining qualification

```bash
PYTHONPATH=src python -m pytest tests/riser_results -q -p no:cacheprovider
```

Fixtures are labelled synthetic. Tests cover hand-calculated utilisation,
constant-amplitude damage, full/half rainflow counts, dimensional conversions,
contact selection, static nulls, malformed schemas, fixed material histories,
provenance, retries, the command interface and import-time binding isolation.

Actual native identifiers, stress definitions, stress-section pressure-force
reconciliation, contact conventions, licence access, mesh/step/duration/seed
convergence and independent engineering review remain unverified. Top angle is
absolute from vertical; flex-joint socket excursion and hardware capacity are not
evaluated. Additional fatigue regions at buoyancy transitions and hog/sag remain
outside the two-region exchange. No output represents qualified fatigue adequacy
or complete code compliance.
