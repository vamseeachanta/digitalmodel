# Hull-girder report pack

The report separates calculated still-water demand from unqualified
strength inputs. Eight strength entries remain unsourced: four signed allowable
curve groups, deck and keel section moduli, material yield strength and permissible
bending stress. No strength utilisation or permitted loading condition is issued.

The load geometry and weight form the synthetic textbook box-barge analytical
fixture recorded in `screening_input.yml`. The published IACS
catalogue establishes publication metadata. Its reproduction terms do not
qualify numerical rule extraction, loading-manual curves or material properties.
No licensed numerical value is copied into the new report or evidence register.

Build from the repository root:

```sh
PYTHONPATH=src uv run --no-sources python examples/workflows/hull-girder-report-pack/build_report.py --output-dir docs/domains/ship-design/hull-girder-report
```

The output contains a self-contained `report.html`, unrounded `results.json` and
`manifest.json` with input and output SHA-256 digests. The builder requires an
explicit destination, rejects changes supplied under canonical input identities,
and rejects output-file symlinks before writing. Existing source inputs and the
shared report renderer are unchanged.

Verify the saved bytes:

```sh
PYTHONPATH=src uv run --no-sources python examples/workflows/hull-girder-report-pack/build_report.py --output-dir docs/domains/ship-design/hull-girder-report --verify
```

`allowable-basis.json` records each strength input, its missing evidence and the
opened source metadata. `revisions.json` supplies the revision history directly
after the executive summary. A subsequent issue adds a history row; the newest
row identifies the declared revision. The report uses the ten-section reporting
sequence and a separate appendix for the evidence required before acceptance.

The numerical check compares the central-block still-water solution with the
existing analytical fixture and its existing tolerances. It establishes numerical
agreement for the synthetic fixture loading geometry. No independent finite-element
comparison, wave combination, section stress, ultimate strength, local buckling
or class compliance is established.

The original `input.yml`, `screening_input.yml`, `make_demo_data.py` and `data/`
remain legacy synthetic regression fixtures. Their illustrative strength ratios
and document metadata are not the current report issued by the new
builder. Replacement of the eight strength entries remains incomplete pending
the qualified evidence listed in the report.

The `tests-infrastructure-other` hosted gate includes `tests/report_pack/` and
`tests/postprocess/`.

The builder currently accepts only `not established` strength-input labels. A
qualified input will require a code change and tests of its evidence qualification
before a different register label can be accepted.
