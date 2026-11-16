# RAO convention conversion

`rao_conventions.py` implements the convention equations stated in a legacy
engineering workbook. It converts one normalized displacement-RAO sample at a
time. It uses the existing `diffraction_units` functions for frequency and angle
units. It does not infer conventions from a solver name.

`RAOInput` is a frozen, validated SI record:

| Field | Units and component order |
|---|---|
| `angular_frequency_rad_s` | rad/s; finite and positive |
| `translation_m_per_m` | m response per m wave amplitude; surge, sway, heave |
| `rotation_rad_per_m` | rad response per m wave amplitude; roll, pitch, yaw |
| `phase_rad` | rad; surge, sway, heave, roll, pitch, yaw |

Table 1. Input units and ordering. Amplitudes are finite and nonnegative; phases
are finite and may include full turns. Input sequences are copied to tuples.

`RAOInput.from_degrees` accepts rotations in deg/m and phases in degrees and
converts both using pi/180. `convert_lag_rao_to_lead` returns another SI record.
`export_rao_degrees` returns a frozen `RAOExport` with frequency in Hz, period in
seconds, translations in m/m, rotations in deg/m, and phases in degrees. Field
names carry their units. Component order is preserved at each boundary.
Unrepresentable frequency, period, or exported angle values are rejected.

The source defines a lag by `x = a R cos(omega t - phi)` and the destination
uses `x = a R cos(omega t + epsilon)`. Therefore `epsilon = -phi`. Phases are
not wrapped: full turns remain full turns. With `mirrored_about_x=True`, the
source's longitudinal symmetry rule adds pi to the converted phases of sway,
roll, and yaw only; magnitudes are unchanged. This assumes physical port/starboard
symmetry and represents reflection in the longitudinal vertical plane, rather
than rotation of a coordinate system.

`wave_frequency_hz` computes `f = omega/(2*pi)` and `wave_period_s` computes
`T = 2*pi/omega`. `opposite_axis_heading_rad` implements the source's
`heading_destination = heading_source - pi` for a finite source angle in
`[0, 2*pi]`, with the same sense of angular measurement on both axes. Output is
in `(-pi, pi]`; source zero is represented by +pi, equivalent to the equation's
-pi endpoint. This helper does not mirror headings automatically.

The method does not preserve erroneous spreadsheet addressing. Each call uses
the sample supplied by its caller. A fixed-row lookup in a legacy engineering
workbook can repeat an amplitude across frequencies; an exporter shall select
the intended frequency sample explicitly. Spreadsheet row/column helpers,
metadata, charts, pasted results and copy-to-clipboard macros are outside this
module.

## Standards and clauses

No named engineering standard, edition or clause is stated in the source's
method material. The source's references section is empty. The equations above
are convention definitions and mathematical unit identities; no standards
compliance is established. Embedded long-period reference tables have no
bibliographic attribution and are not implemented here.

## Input gaps and limits

- Repeated source tables have inconsistent amplitude labels (normalized RAOs
  versus displacement/rotation). The caller must establish normalization per
  wave amplitude before constructing a sample. Wave height and wave slope
  normalization are unsupported.
- No reference-point locations or transfer equation are supplied. Lever-arm
  transfer is outside this revision; rotational amplitudes are not applied to
  another point.
- No spatial wave-reference offset, wave number or reference-phase propagation
  equation is supplied. Only lag/lead temporal sign conversion is implemented;
  wave-reference translation and sine/cosine reference changes are unsupported.
- The heading helper changes only the opposite reference axis. Arbitrary axis
  permutations, handedness changes, global/body rotations and automatic heading
  expansion require a separately specified mapping.
- The symmetry rule requires a symmetric vessel and consistent motion axes.
  This module does not establish either from data.
- Standard titles, editions, clauses and the attribution of theoretical
  long-period tables remain missing source evidence.

A next revision may add reference-point transfer, explicit coordinate transforms
and normalization variants after those definitions and independent verification
criteria are supplied. No source RAO dataset or private cached result is included.

The tests use illustrative examples and independent trigonometric identities,
unit formulas, limiting cases and validation checks. Run them from the repository
root with `python -m pytest tests/hydrodynamics/rao_conventions` in an environment
with the repository dependencies installed.
