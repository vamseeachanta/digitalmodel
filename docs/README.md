# digitalmodel Documentation

This is the canonical documentation entry point for digitalmodel issue routing.
Use it with the repo entry surfaces before changing source, tests, docs, or
routing registries.

## Required Routing Surfaces

| Surface | Role |
|---|---|
| [AGENTS.md](../AGENTS.md) | Worker entry point, commands, and repo constraints |
| [README.md](../README.md) | Human overview and high-level module summary |
| [docs/domains/README.md](domains/README.md) | Domain documentation layout and domain-doc caveats |
| [docs/maps/digitalmodel-operator-map.md](maps/digitalmodel-operator-map.md) | Repo-wide code/tests/docs operator map |
| [docs/registry/module-routing.yaml](registry/module-routing.yaml) | Canonical machine-readable module routing registry |
| [Engineering analysis criteria](standards/analysis-parametric-lookup-criteria.html) | Required criteria for new or materially revised analysis deliverables, parametric studies and lookup datasets |
| [Engineering analysis workflow](standards/engineering-analysis-workflow.html) | Stage actions, evidence handoffs and exit conditions; [ANSYS](domains/ansys/analysis-workflow.html) and [AQWA](domains/aqwa/analysis-workflow.html) domain mappings |

## Route By Issue Type

New and materially revised engineering analysis deliverables shall follow the [parametric-analysis and lookup-dataset criteria](standards/analysis-parametric-lookup-criteria.html): explicit acceptance criteria, justified range coverage, independently verified responses and revision-bound lookup outputs. Existing shared workflow and source-data contracts retain authority; adoption shall be recorded per analysis, and this guide does not certify historical analyses or install cross-repository enforcement.

| Issue type | Start with | Tests | Docs |
|---|---|---|---|
| Engineering domain calculation | `src/digitalmodel/<domain>/` from the operator map | `tests/<domain>/` when present | `docs/domains/<domain>/`; [analysis criteria](standards/analysis-parametric-lookup-criteria.html) and [common workflow](standards/engineering-analysis-workflow.html) |
| Solver integration | `src/digitalmodel/solvers/`, `src/digitalmodel/orcaflex/`, `src/digitalmodel/orcawave/`, and bridge rows in the operator map | `tests/solvers/`, `tests/orcaflex/`, `tests/orcawave/`, and integration tests | solver/domain docs plus the operator map; [analysis criteria](standards/analysis-parametric-lookup-criteria.html) |
| Validation, traceability, or standards work | domain source row plus `src/digitalmodel/citations/` when citation metadata is involved | matching domain tests, `tests/citations/`, and `tests/engineering_validation/` | domain docs and registry entry; [analysis criteria](standards/analysis-parametric-lookup-criteria.html) |
| Infrastructure, configs, or workflow routing | `src/digitalmodel/infrastructure/`, `src/digitalmodel/workflows/`, or `src/digitalmodel/data_systems/` | matching tests plus integration tests | domain docs and this routing entry point |
| Documentation/indexing maintenance | `docs/README.md`, `docs/domains/README.md`, operator map, and registry | `tests/docs/test_digitalmodel_routing_contract.py` | this file |

## Curated Routing Surfaces

Curated routing surfaces are authoritative for issue work because they are small,
reviewed, and intended for repeat use:

- `AGENTS.md`
- `README.md`
- `docs/README.md`
- `docs/domains/README.md`
- `docs/maps/digitalmodel-operator-map.md`
- `docs/registry/module-routing.yaml`
- [docs/standards/analysis-parametric-lookup-criteria.html](standards/analysis-parametric-lookup-criteria.html)
- [Common analysis workflow](standards/engineering-analysis-workflow.html)
- [ANSYS analysis workflow](domains/ansys/analysis-workflow.html)
- [AQWA analysis workflow](domains/aqwa/analysis-workflow.html)
- focused tests that validate these surfaces

The criteria and workflow definitions are documentation surfaces; the module routing registry remains unchanged. The existing documentation-routing tests remain applicable to their original contracts and do not establish criteria adoption or automated workflow enforcement.

Program workflow mappings are indexed at this entry point and in the common workflow. The domain-layout README describes directory organization; it is not a duplicate workflow index.

Raw inventory surfaces are discovery aids only. Broad generated indexes, historic
reports, logs, cached outputs, and large extracted inventories can help locate
candidate files, but they do not replace the curated routing surfaces above.

## Repo-vs-Bulk-Artifact-Store

The universal placement rule is repo-vs-bulk-artifact-store. Keep source, tests,
small curated docs, operator maps, and routing registries in the repo. Put large,
generated, binary, cache, raw crawl, or fast-growing artifacts in a bulk artifact
store instead. `/mnt/ace/data` is the current workspace-hub implementation example
for that bulk store, not a path that should be hard-coded into portable repo logic.
