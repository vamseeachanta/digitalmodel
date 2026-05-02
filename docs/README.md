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

## Route By Issue Type

| Issue type | Start with | Tests | Docs |
|---|---|---|---|
| Engineering domain calculation | `src/digitalmodel/<domain>/` from the operator map | `tests/<domain>/` when present | `docs/domains/<domain>/` when present |
| Solver integration | `src/digitalmodel/solvers/`, `src/digitalmodel/orcaflex/`, `src/digitalmodel/orcawave/`, and bridge rows in the operator map | `tests/solvers/`, `tests/orcaflex/`, `tests/orcawave/`, and integration tests | solver/domain docs plus the operator map |
| Validation, traceability, or standards work | domain source row plus `src/digitalmodel/citations/` when citation metadata is involved | matching domain tests, `tests/citations/`, and `tests/engineering_validation/` | domain docs and registry entry |
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
- focused tests that validate these surfaces

Raw inventory surfaces are discovery aids only. Broad generated indexes, historic
reports, logs, cached outputs, and large extracted inventories can help locate
candidate files, but they do not replace the curated routing surfaces above.

## Repo-vs-Bulk-Artifact-Store

The universal placement rule is repo-vs-bulk-artifact-store. Keep source, tests,
small curated docs, operator maps, and routing registries in the repo. Put large,
generated, binary, cache, raw crawl, or fast-growing artifacts in a bulk artifact
store instead. `/mnt/ace/data` is the current workspace-hub implementation example
for that bulk store, not a path that should be hard-coded into portable repo logic.
