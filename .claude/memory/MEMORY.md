# digitalmodel — repo-scoped memory

Session context for THIS repo only (epic #3084 context-flow backbone).
Keep entries scoped to digitalmodel; cross-repo facts stay in workspace-hub memory.

## Project
- (repo mission summary — see config/mission/mission-map.yaml in workspace-hub)

## Feedback
- (none yet — repo-scoped feedback accrues here instead of the workspace-hub blob)

## Project (cfd)
- Master mesh store adopted 2026-09-04: `scripts/cfd/mesh_store.sh` (host runtime) + `digitalmodel.solvers.openfoam.mesh_store` (library), identity = meshing dicts + STLs; only the SERIAL polyMesh is shared (parallel decomposition is not reproducible on gpu-claw). Chain scripts flow repo -> lane via `scripts/cfd/deploy_lane.sh`; see docs/domains/openfoam/mesh_store_case_layout.md. Chain lives on feat/1173-calm-water-hull-resistance, not main.
