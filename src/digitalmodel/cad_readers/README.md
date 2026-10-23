# CAD readers

Read CAD files into typed geometry records that carry mandatory length units.
All examples and tests use synthetic geometry. No drawing is a qualified
structural model merely because it can be read.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Add format adapters and a shared units contract for geometry ingestion. |

## Formats and dependencies

| Format | Dependency | Returned geometry | Coordinate units |
| --- | --- | --- | --- |
| DXF | `ezdxf>=1.3,<2` (`drawings` extra) | Complete `ezdxf.document.Drawing` | Modelspace `$INSUNITS`, or caller declaration |
| STEP, STP | `cadquery-ocp>=7.7,<8` (`cad` extra) | OpenCASCADE `TopoDS_Shape` | Millimetres; original unit declarations retained |
| IGES, IGS | `cadquery-ocp>=7.7,<8` (`cad` extra) | OpenCASCADE `TopoDS_Shape`, including faces | Millimetres; original unit declaration retained |
| STL, OBJ | `meshio>=5.3,<6` (`mesh` extra) | `meshio.Mesh` points and connectivity | Caller declaration required |
| DWG | Explicit external converter plus `ezdxf` | Converted DXF document | Converted DXF units, or caller declaration |

Dependencies are imported only by the reader that uses them. The package can
be imported without optional CAD libraries. OpenCASCADE 7.9 is exercised in
the tests; version 8 bindings are outside the supported dependency range.

```python
from digitalmodel.cad_readers import read_cad

model = read_cad("component.step")
shape = model.geometry.shape
factor_to_metres = model.units.metres_per_unit

mesh_model = read_cad("surface.stl", units="mm")
points_m = mesh_model.geometry.mesh.points * mesh_model.units.metres_per_unit
```

## Units contract

Supported declarations are `m`, `mm`, `cm`, `in` and `ft`. Missing units raise
`UnitsError` unless `units=` is provided. A caller declaration conflicting
with file metadata raises `UnitsError`; it does not rescale or override the
file. Unsupported and user-defined file units are refused even if the caller
supplies a declaration. Unitless STL/OBJ comments are not trusted as metadata.

`UnitInfo.coordinate_unit` describes the returned geometry, and
`metres_per_unit` converts its coordinates to metres. `declared_units` retains
the distinct original file units; it is empty when units came from the caller.
`source` is `file` or `caller`. Mixed supported STEP representation units are
translated by OpenCASCADE into the same output unit; a single caller declaration
must agree with every declaration in that file.

For a length `L`, the conversion is `L_m = L * metres_per_unit`. For derived
areas and volumes the factor is squared or cubed by dimensional multiplication.
The exact definitions are from [NIST SP 811 (2008), Appendix B.8](https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8):
inch = 0.0254 metre and foot = 0.3048 metre; milli and centi are SI prefixes.
These are unit definitions, not engineering acceptance limits.

## DWG call form

```python
from digitalmodel.cad_readers import Converter, read_cad

# The adapter accepts this call form; configure arguments for the chosen tool.
converter = Converter(
    ("dwg-to-dxf", "{input}", "{output}"),
    timeout_seconds=conversion_budget_seconds,
)
model = read_cad(
    "drawing.dwg", converter=converter, converted_path="converted.dxf"
)
```

The placeholders must be standalone arguments. Execution uses an argument
vector without a shell. The caller provides the executable, arguments, timeout
and a new output path. Existing output is never overwritten; generated output
is retained even after a read failure. A nonzero exit, timeout, missing output,
malformed DXF or unresolved units raises `CadReadError` (or `UnitsError`).
Converter stdout and stderr are discarded because they can contain drawing
content. No converter is installed, discovered or automatically invoked by
another format. Tests execute a small Python fake; real converter compatibility
remains unqualified.

## Worked example

Run from a source checkout with `ezdxf` installed, directing output to a new
directory chosen by the caller:

```sh
PYTHONPATH=src python examples/cad_readers/read_line.py --output-directory "$EXAMPLE_OUTPUT"
```

The input is one line from `(0, 0, 0)` to `(2000, 0, 0)` with DXF modelspace
units set to millimetres. The reader retains those endpoints and units. The
method subtracts endpoints, takes the Euclidean length, and multiplies by
the millimetre-to-metre factor. Independent hand calculation:
`2000 mm * (1 m / 1000 mm) = 2 m`. Output:

```json
{
  "input_length": 2000.0,
  "input_unit": "mm",
  "length_m": 2.0,
  "units_source": "file"
}
```

## Limits and failure behavior

- Native DXF entities, blocks, layers and paperspace remain in the document.
  The units contract applies to modelspace; nested block unit conversion and
  flattening are downstream operations. Empty modelspace is refused. Proxy
  entities and unresolved external references are retained, not reconstructed.
- STEP and IGES translation uses the kernel's transfer process and its healing
  behavior. It is not a shape-validity, watertightness or structural check.
  Null, empty or incomplete root transfers are refused. Per-entity diagnostics, assemblies,
  names, colours and PMI are not exposed by this adapter.
- OpenCASCADE translation settings are restored after each call. Calls through
  this package are serialized; other code changing kernel settings concurrently
  must coordinate externally.
- Meshes are read without topology repair. Empty geometry and nonfinite points
  and out-of-range connectivity are refused. Mesh quality and physical applicability
  are not evaluated.
- File extensions select the reader. Unit inference from dimensions, filenames
  or appearance is not performed. No licensed solver is used.

## Sources and tests

- [ezdxf Units, stable documentation](https://ezdxf.readthedocs.io/en/stable/concepts/units.html): `$INSUNITS`, unitless drawings and conversion semantics.
- [OpenCASCADE STEP user guide](https://dev.opencascade.org/doc/overview/html/occt_user_guides__step.html): loading, units and translation.
- [OpenCASCADE IGES user guide](https://dev.opencascade.org/doc/overview/html/occt_user_guides__iges.html): surfaces, output units and translator settings.
- [OpenCASCADE 7.9 IGES unit flags](https://github.com/Open-Cascade-SAS/OCCT/blob/V7_9_0/src/IGESData/IGESData_BasicEditor.cxx): named unit flags and conversion definitions.
- [meshio package documentation](https://pypi.org/project/meshio/): STL/OBJ readers and mesh representation.

`tests/cad_readers/test_readers.py` writes the fixtures during the tests. Expected
dimensions are independent hand calculations in the test docstrings. Kernel
comparisons use `Precision::Confusion`, the kernel numerical precision, rather
than a guessed design acceptance limit.
