These checks need the open-source solver chain, which is not installed on this workstation (Python here has no numpy, pyNastran or MYSTRAN). They are pending the execution environment, which acts as the benchmark execution for this route.

| gate item | check | reason it is pending |
|---|---|---|
| FE0 | pyNastran reads each deck with cross-referencing: read errors 0, unresolved references 0, unsupported cards 0 | pyNastran not available here |
| FE1 | MYSTRAN completes its input checks: fatal messages 0, warnings dispositioned | MYSTRAN not available here |
| FE10 | LE1 on three meshes and the patch test, then compare.py | needs the execution environment runs |

The offline checks in Table 3 establish the decks' content (references, geometry, constraints, load resultant), not the solver's reading of them.
