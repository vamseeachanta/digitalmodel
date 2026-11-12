# Generator data sources (IEA 15 MW / VolturnUS-S)

This file records the sources of `generator_data.yml`. The sources of
`basis.yml` are recorded in [`SOURCES.md`](SOURCES.md). That file is kept
unchanged because the quasi-static mooring report
(`docs/reports/fowt-volturnus-mooring-analysis.manifest.json`) records its
SHA-256 digest as a report input.

`generator_data.yml` holds the values the OrcaFlex modular generator's floating
wind model type reads in addition to `basis.yml`. It is extracted by
`scripts/fowt/extract_iea15_generator_data.py` from four files of the
IEA-15-240-RWT repository at the same pinned revision
(`e4993d63de10f165389534461dd544006750fe60`), retrieved on 2026-11-12 with HTTP 200
from `https://raw.githubusercontent.com/IEAWindTask37/IEA-15-240-RWT/<revision>/<path>`:

- `Documentation/IEA-15-240-RWT_tabular.xlsx`: Overview (rotor and hub
  geometry, shaft tilt, precone, blade mass), Nacelle Mass Properties (rotor
  and nacelle mass and centre) and Rotor Performance (steady thrust curve).
- `OpenFAST/IEA-15-240-RWT-UMaineSemi/HydroData/IEA-15-240-RWT-UMaineSemi.hst`:
  WAMIT hydrostatic stiffness, with the centre of gravity placed at the origin
  so that the gravity moment-arm term is excluded.
- `OpenFAST/IEA-15-240-RWT-UMaineSemi/HydroData/IEA-15-240-RWT-UMaineSemi.1`:
  WAMIT added mass and radiation damping at zero, infinite and 100 finite
  frequencies.
- `OpenFAST/IEA-15-240-RWT-UMaineSemi/IEA-15-240-RWT-UMaineSemi_HydroDyn.dat`:
  `WAMITULEN`, `PtfmVol0` and the additional quadratic damping matrix `AddBQuad`.

The finite-frequency radiation table is a diagnostic subset: the source supplies
18 matrix entries per period, while the export retains only entries 11, 33, 55,
15 and 66 for each of added mass and damping. Sway, roll and other coupling
entries are omitted. Omitted entries must not be treated as zero; complete
matrices must be read from the pinned source before use in a full model.

The extractor verifies all four inputs against expected SHA-256 digests at the
pinned revision before parsing any values. A mismatch rejects extraction.
The raw files are not committed; their SHA-256 digests are recorded in
`generator_data.yml`, and the extraction test reproduces the committed file
byte for byte when the raw folder is supplied through `DIGITALMODEL_IEA15_RAW`.

The workbook's RNA row centre of mass is not the mass-weighted combination of
its own nacelle, blade and hub rows: the component rows place it 0.533 m further
downwind and 0.056 m lower. The difference is recorded in the data file; the
generator adopts the RNA row, as `basis.yml` does. The source discrepancy is not
resolved.

The repository at the pinned revision holds no OrcaFlex input files. Orcina's
public K03 example, held under `docs/domains/orcaflex/examples/`, models the same
design and is used only as a comparator.
