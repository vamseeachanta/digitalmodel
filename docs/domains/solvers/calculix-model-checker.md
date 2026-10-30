# CalculiX pre-run model checker

The checker reads a complete structural INP deck and its includes before
`FEMChain.solve` discovers or starts CalculiX. Errors block execution; warnings
pass. Every finding contains a severity, entity identifiers and a fix hint.
The solver result retains the report under `qa_gate`. The checker itself never
starts CalculiX or gmsh.

## Use

```sh
calculix-check analysis.inp --tolerance 1e-6
python -m digitalmodel.solvers.calculix.cli analysis.inp --json
```

The CLI returns 0 for a passing gate, 1 for model errors and 2 for invalid
arguments. `--tolerance` is an absolute Euclidean distance in the deck's length
unit, with a default of `1e-6`. Supply a finite positive tolerance appropriate
to the model's units and intended clearances.

```python
from digitalmodel.solvers.calculix import check_deck, validate_model

report = check_deck("analysis.inp", tolerance=1e-6)
evidence = report.to_dict()
validate_model("analysis.inp")  # raises ModelCheckError on errors
```

`ModelCheckError.report` retains the findings. A full run uses the same gate
through `FEMChain.solve(node_tolerance=1e-6)`. Every solve rereads the current
deck, so edits after setup are checked.

## Checks

| Finding | Severity | Basis and correction |
| --- | --- | --- |
| `unconstrained_part` | Error | A connected group of structural elements has no boundary condition or tie/contact path to a supported group in an analysis step. Add restraints or correct the connection. |
| `floating_mass` | Error | A MASS or ROTARYI element has no path to structural elements. Attach its node to the structure. A boundary condition on an isolated mass is insufficient. |
| `unmerged_nodes` | Error | Distinct nodes lie within the stated tolerance. Merge them or validate an intentional tie. Only opposite sides of an explicit tie are exempt; contact alone supplies no exemption. |
| `missing_reference` | Error | A referenced node, element, set, material, surface, amplitude or contact interaction is undefined. Correct the name or define the entity. |
| `missing_section` | Error | An element lacks a section or mass property assignment. Complete its analysis definition. |
| `unused_definition`, `unused_section` | Warning | A definition is unreferenced or a section selects no elements. Confirm its purpose or remove it. |
| `contact_path` | Warning | A contact pair supplies a possible support path. Confirm gaps, normals and engagement independently. |
| `parse_error`, `unsupported_topology`, `empty_mesh`, `empty_surface` | Error | The reader cannot establish a usable model. Correct the input or extend and test the supported reader. |

## Input scope and limits

The reader supports case-insensitive keywords and names, relative includes,
comments, continued element connectivity, generated sets, nested set unions,
and forward references. Supported element widths are explicit in
`_deck.ELEMENT_NODES`; they cover common solid, plane, axisymmetric, shell,
beam, truss, two-node spring/dashpot and point-mass elements. Element surfaces
support tetrahedron, hexahedron and wedge faces and shell SPOS/SNEG faces.
Node surfaces resolve node IDs or node sets.

gmsh INP exports contain only a mesh. Add material, section and boundary
definitions before using them as analysis decks. Mixed export blocks retain
unique element IDs across the file.

Support is a connectivity check. A path to one restrained degree of freedom
does not prove that all rigid-body modes are restrained. The gate does not
compute stiffness rank, assess mesh quality or qualify an analysis. Contact
paths assume the declared pairs can engage. A tie joins its declared surfaces;
its interpolation and effective tolerance require independent review.

Boundary conditions persist between steps; `OP=NEW` resets the active
restraints. Each step is checked independently. Rotational restraints alone
do not support solid-only nodes. The reader blocks unsupported element types,
coordinate transforms, mesh-generation keywords, assemblies, composite
sections, equations, MPCs and rigid-body constraints. Other solver physics and
keyword data are outside the connectivity and reference checks.

Synthetic tests in `tests/solvers/calculix/test_model_checker.py` cover the
findings, parser failures, gmsh export compatibility, CLI exit codes and the
solver launch boundary. Native solver qualification remains separate.
