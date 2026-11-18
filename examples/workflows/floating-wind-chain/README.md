# Floating wind sizing analysis example

## Summary

This synthetic barge example demonstrates a shared screening input and the
mass/CG mapping into diffraction and coupled-model specifications. It produces
structured inputs and a standard report. It supplies no physical turbine,
controller, mooring, panel mesh or solver result. All solver-dependent checks
remain `NOT_EVALUATED`.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 00 | 2026-10-20 | Initial arithmetic example to demonstrate the shared-input mapping and remaining evidence needs. |
| 01 | 2026-10-22 | Require explicit density and ballast position, retain spectral sea states, publish validated UTF-8 inputs and record the hull-only equilibrium gap. |

## Inputs and basis

[input.yml](input.yml) is the only driving input. All geometry, mass, radii,
sea states and screening coefficients are **assumed, based on previous
experience**, chosen for transparent arithmetic. They are required inputs,
not design recommendations. Water density and ballast CG above keel are explicit
inputs. The inherited screening model supports only its existing density and
ballast-position convention; incompatible inputs are rejected. This example's
density is an assumption consistent with the NOAA World Ocean Atlas 2013 FAQ,
question 8; its suitability for a particular site is not established.

| Quantity | Input | Unit |
| --- | ---: | --- |
| Length × beam × draft | 20.0 × 10.0 × 2.0 | m |
| Block and waterplane coefficients | 1.0 | dimensionless |
| Steel mass | 100.0 | t |
| RNA / tower masses | 10.0 / 20.0 | t |
| Hub / tower-base height above still water | 10.0 / 2.0 | m |
| Hull roll / pitch / yaw radii | 3.0 / 5.0 / 5.0 | m |
| Water depth | 50.0 | m |
| Water density | 1025.0 | kg/m³ |
| Ballast CG above keel | 0.1 | m |

## Method and independent answer

Using the explicit assumed density of 1025.0 kg/m³:

```text
Volume = 20*10*2 = 400.0 m³
Displacement = 400*1025/1000 = 410.0 t
Ballast = 410 - 100 - 10 - 20 = 280.0 t
Hull-only mass = 410 - 10 - 20 = 380.0 t
```

The steel CG is at half the draft, 1.0 m above keel, from the explicit input.
The explicit ballast CG is 0.1 m above keel. This value matches the inherited
screening assumption of 0.05 times draft; a different position is rejected
because the unchanged screening model cannot represent it. The position is a
synthetic assumption, not an independently established design recommendation.
The RNA CG is at 12.0 m above keel; the uniform tower CG is at 8.0 m.
The first moment of the full system is
`100*1 + 280*0.1 + 10*12 + 20*8 = 408.0 t.m`.
Removing the topside gives hull first moment `128.0 t.m` above keel.
Consequently hull CG relative to still water is `128/380 - 2`, or **−1.7 m**
when reported to one decimal place. The tests evaluate this expression before
rounding. The hull inertia diagonal is **3420.0, 9500.0, 9500.0 t.m²**;
for example roll inertia is `380*3² = 3420.0 t.m²`.

These are independent hand calculations, not expected values obtained by
calling the implementation. The mass first-moment equation is from OpenStax,
*University Physics volume 1* (2016), section 9.6; inertia is from section 10.5.
The exact conversion `1 t = 1000 kg` is from BIPM, *SI Brochure*, ninth edition
(2019), Table 8. [The workflow documentation](../../../docs/domains/orcaflex/wind/sizing-to-solver-chain.md)
links each opened source and explains the datum and limitations.
Standard gravity follows BIPM, Declaration 2 of the 3rd CGPM (1901),
"conventional value of standard gravity"; it is not a site measurement.

The template uses JONSWAP: the shared significant height and spectral peak
period remain `WaveHs` and `WaveTp`. Regular-wave types are rejected because
no design-wave height or period conversion is supplied. Nonzero steady loads
are rejected because native applied-force mapping is not implemented.

## Reproduce

Run in an environment with the repository's Python dependencies:

```bash
PYTHONPATH=src python examples/workflows/floating-wind-chain/run.py --out "$OUTPUT_DIR"
```

Set `OUTPUT_DIR` to an empty output directory outside the checkout. The script
writes `diffraction.yml`, the sea-state coupling spec, generated modular inputs,
`manifest.json`, `analysis.json` and `analysis.html`. No browser or solver starts.
[The recorded example report](analysis.html)
is built from these inputs with no solver results.
All adapter and report text I/O declares UTF-8 and LF. Generation and mandatory
validation run in a temporary sibling directory before publication; an error
leaves the requested output empty or absent so that the input can be corrected
and retried. The manifest uses forward slashes and rejects links and junctions
in generated output.

For a future stored-results comparison, pass `--results records.json`. Each row
is a `ResultRecord` as documented in the workflow guide. Use the digest in the
new manifest, the actual solver version, and a traceable source. The unit is
seconds; the supported metrics are heave and pitch natural periods. A simple
synthetic comparison of a 10.0 s screen with a 12.0 s stored value gives a
2.0 s difference and 20.0 %; it is tested as arithmetic and never presented as
a solved response.

## Conclusion and way forward

The example closes the mass mapping without counting RNA and tower twice.
It demonstrates structural input validation, not turbine design validation.
The diffraction body is 380.0 t against 410.0 t displacement at the specified
draft: the separate 30.0 t topside is absent. Static equilibrium and bare-hull
RAOs remain `NOT_EVALUATED` until the load balance is defined. The manifest and
report retain these masses and the equilibrium criterion.
Replace the skeletal generic template with the reviewed turbine/controller
arrangement, account for its separate mass and inertia, supply and check the
mesh, import fresh diffraction data, and validate native loading before the
recorded static, decay and time-domain analyses described in #2435.
