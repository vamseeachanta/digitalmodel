# OrcaWave — specifications for unbuilt capability

Seven analysis capabilities documented in the AceEngineer marine-offshore
skill corpus against an API that does not exist in `digitalmodel`.

Discovered when a real engagement followed one of these skills and found the
import missing (`aceengineer-strategy#262`, then `#267`). A sweep showed 134
documented paths across 28 skills did not resolve; most were stale after the
`solvers/` and `hydrodynamics/` reorganisation and were repointed. These seven
were not drift — the capability was never built.

| Spec | Snippets | Absent paths |
|---|---|---|
| [OrcaWave analysis](analysis.md) | 2 | 8 |
| [OrcaWave aqwa benchmark](aqwa-benchmark.md) | 2 | 6 |
| [OrcaWave damping sweep](damping-sweep.md) | 3 | 7 |
| [OrcaWave mesh generation](mesh-generation.md) | 2 | 7 |
| [OrcaWave multi body](multi-body.md) | 2 | 6 |
| [OrcaWave qtf analysis](qtf-analysis.md) | 2 | 6 |
| [OrcaWave to orcaflex](to-orcaflex.md) | 2 | 9 |

## Why these are here and not in the skill corpus

A skill that documents a non-existent API is worse than no skill: it reads as
an asset, survives review, and fails at the moment an agent depends on it.
Moving the specification to the repository that would implement it turns a
trap into a build target.

The skills keep their engineering content and now carry an explicit warning
plus an `ace:known-missing` marker, so the absence is declared rather than
silently carried.
