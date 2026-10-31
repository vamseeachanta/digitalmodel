# Parametric atlas dataset publishing

The existing atlas publisher validates the committed grids, freezes their bytes,
and uploads the dataset card and all configurations in one commit. Downloaded
files are checked against those bytes at the returned revision. Viewer readiness
is reported only after the `/rows` response agrees with the local sample and total
row count. An upload can succeed while viewer readiness remains unestablished.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-19 | Document the publication gate, reproducible example and remaining qualification limits. |
| 2 | 2026-10-24 | Reject boolean validation numbers and malformed viewer rows; cover publication guards and maintain card history here to avoid stale generated revisions. |

The generated dataset card refers to this history rather than repeating a fixed
revision number and publication date.

## Reproduce

Run from the repository root with the launch environment's `UV_CACHE_DIR`,
`PRE_COMMIT_HOME`, and `TMPDIR` exported. Set `HF_HUB_CACHE` and `HF_XET_CACHE` to
subdirectories of that approved cache location when publishing. Authentication
uses the existing credential store; tokens shall not be printed or committed.

```sh
uv run --no-project --with pandas --with pyarrow --with pyyaml \
  python scripts/hf/publish_atlas_explorer.py --dry-run
uv run --no-project --with pytest --with pandas --with pyarrow --with pyyaml \
  --with huggingface_hub python -m pytest tests/scripts/test_publish_atlas_explorer.py -q -o addopts='' \
  -p no:cacheprovider --basetemp "$TMPDIR/hf-tests"
```

The dry run performs no network writes. Remove `--dry-run` and include
`--with huggingface_hub` to publish the existing projection. Temporary readback
files use `TMPDIR`; parquet and card payloads are otherwise held in memory.
A concurrent dataset update is rejected through the commit's expected parent.

## Worked example: integrity gate

Inputs are invented arithmetic test data, not a design curve or material data.
Define an allowable cycle count `N = 100` and applied cycle counts `n = 10, 20`.
For the illustrative single-bin relation `D = n / N`, hand division gives
`D = 0.1, 0.2`. This arithmetic is written independently in the test fixture's
docstring. No engineering coefficient or acceptance limit is inferred from it.

The declared grid contains both cycle counts. The publication method checks
that every declared point occurs exactly once and that the response is finite
and nonnegative. Both rows pass. Replacing one damage value with infinity,
removing a point, or duplicating a point blocks publication. A viewer response
with the wrong damage, total count or sample length blocks readiness.
Missing or malformed row records follow the same bounded retry path and end with
`viewer NOT_READY` if the response remains invalid.

Interpolation acceptance uses the error and threshold already recorded by the
source manifest. This demonstrates preservation of the source validation claim;
it does not independently reproduce the underlying physical calculation.
Boolean error or threshold values are rejected rather than interpreted as numbers.

## Configurations and registry

The existing dataset contains `mooring_fatigue`,
`synthetic_rope_mooring_fatigue`, and `code_check`, each with a `train` split.
The website's `config/capabilities.yaml` already registers these configurations
under `atlas-explorer`, with `mooring_fatigue` as the primary configuration.
No duplicate registry or dataset is introduced. A website build remains a
separate acceptance check in the website repository.

The card includes the source `grid.parquet` and `manifest.yaml` SHA-256 digests;
the grid bytes are uploaded under the viewer filename `train.parquet`.
Any `provenance.citations` sidecars are carried into the card unchanged.
Edition-only standards references are retained, but absent complete sidecars
are labelled **NOT_EVALUATED**. Clause-level qualification, units absent from
source field names, and independent engineering validation remain required
before these demonstration grids can serve as design inputs.

## Sources and limits

- [HF manual configuration](https://huggingface.co/docs/hub/datasets-manual-configuration)
  defines the dataset card `configs`, `data_files` and split layout.
- [HF rows API](https://huggingface.co/docs/dataset-viewer/rows) defines the viewer
  readback surface and total-row metadata.
- [Atlas response implementations](../src/digitalmodel/parametric/generate.py)
  identify the existing calculation methods; this change introduces no engineering
  equation, coefficient, standard limit or replacement material data.
- The source manifests identify DNV-RP-C203 (2021-09), *Fatigue design of offshore
  steel structures*; DNV-OS-E301 (2018-07), *Position mooring*; and API RP 2RD (2013),
  *Design of Risers for Floating Production Systems and Tension-Leg Platforms*.
  Clause identifiers and complete Citation sidecars are not established by those
  manifests; the card shall not imply otherwise.

A viewer HTTP error is an application/indexing failure and does not establish
that authentication or the network is unavailable. The command fails after
bounded retries and leaves the uploaded revision available for diagnosis.
