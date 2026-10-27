# Headless CAD inspection pilot

The `digitalmodel.cad_io` package inspects DWG, DXF and STEP inputs with optional
tools. It has no dependency on a desktop application, a display server or a
commercial converter. Synthetic tests generate three DXF drawings and two STEP
solids; no historical input or derived output is distributed with the package.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 0 | 2026-10-22 | Add conversion and solid checks with synthetic inputs to assess a headless CAD workflow. |
| 1 | 2026-10-25 | Reject input/report/checkpoint aliases before writes to preserve source bytes; document mesh and converter settings so their selection basis is explicit. |
| 2 | 2026-10-27 | Extend alias checks to every artifact and converter log, including standalone conversions; constrain optional CAD dependencies to versions compatible with runtime requirements. |

## Placement and interfaces

This package follows the repository's top-level utility packages: a small public
`__init__.py`, a `__main__.py` command, separate drawing and solid operations,
and acceptance tests under `tests/specialized/cad_io/`, covered by the existing
specialized test shard. Dependencies are imported inside
operations. The optional `cad-io` extra installs the drawing and solid readers
on platforms with compatible kernel wheels. `cad-io-mesh` adds gmsh on platforms
for which it is available.

The CAD extra uses build123d 0.7.0 because later releases require NumPy 2,
whereas the runtime requires NumPy 1.x. Its kernel is constrained to the 7.7
API, which provides `Shape.HashCode`; the SVG adapter excludes releases
that import a newer kernel through a separate distribution. The 3MF binding
is pinned to the release that provides `py_lib3mf`. These constraints avoid
the webcolors and NumPy conflicts introduced by build123d 0.13.0 without
changing existing runtime requirements. String filenames are passed to the
older kernel interfaces.

- `read_dwg_version(path)` returns `tag` and `release`. The six-byte header
  identifies a format family; it does not establish file validity.
- `dwg_to_dxf(path, out_dir)` returns the converted DXF path. LibreDWG subprocesses
  have a 120-second timeout and write separate logs. Nonzero exits are failures.
- `dxf_to_json(path)` returns layers, text, block attributes, dimension values,
  drawing units and entity counts. Counts include entities in every layout and
  block definition once, plus attached attributes; block instances are not
  expanded. Normal reading is attempted first. If ezdxf recovery is needed,
  the result reports its use and audit error/fix counts.
  LibreDWG 0.14 can emit a null `ENDBLK` entity handle that even ezdxf recovery
  rejects. Only that invalid handle is omitted in memory before recovery;
  `null_endblk_handles_removed` records the count. Source bytes, geometry and
  ownership/reference tags are retained.
- `dxf_to_svg(path, out)` renders model space through ezdxf's SVG drawing backend.
- `roundtrip_check(dwg, out_dir=None)` compares counts after DWG → DXF → DWG → DXF.
  The return DWG uses LibreDWG's R2000 writer. If `dxf2dwg` is absent, the result
  explicitly identifies DWG → DXF → DXF through ezdxf. An installed encoder that
  fails is reported as a failure. Agreement is 100 times the sum of per-type
  minimum counts divided by the sum of per-type maximum counts. Two empty
  drawings agree at 100.0 %. This metric does not establish geometry fidelity.
- `step_checks(path, out_dir=None, mesh=True)` reports kernel validity, a bounding
  box and solid volume in imported millimetres and cubic millimetres. It attempts
  binary glTF (`part.glb`), STL (`part.stl`) and a coarse first-order tetrahedral
  mesh exported as Abaqus/CalculiX INP (`part.inp`). Mesh counts cover nodes and
  volume elements. The mesh is a format smoke check; load cases, materials,
  mesh convergence and solver validation are outside this pilot.

When `out_dir` is omitted, the two inspection functions retain their artifacts
in a new directory under `TMPDIR`. Use an explicit directory for reproducible
artifact locations. Conversion and surface exports refuse existing targets.

## Pilot settings and selection basis

These values are pilot operating choices, rather than values prescribed by a
standard or calibrated against engineering results:

| Setting | Selection basis | Limit |
| --- | --- | --- |
| Maximum mesh size `span / 4` | Select four characteristic lengths across the largest bounding-box extent for a coarse export trial with few elements. | A target size, not a guaranteed element count or an accuracy criterion. |
| Minimum mesh size `span / 8` | Allow local sizing down to half the maximum target size while bounding the requested refinement range. | Small features may require finer sizing; no convergence study supports this value. |
| Converter timeout `120` seconds | Bound each converter invocation to two minutes so an unresponsive tool cannot hold the sequential batch indefinitely. | An operating budget, not a measured upper bound for valid drawings. Large inputs may time out; a round trip invokes multiple converters, each with its own budget. |

`span` is the largest imported bounding-box extent in model length units. The
divisors are dimensionless and scale with that extent. These choices establish
neither mesh quality nor engineering accuracy. A format smoke check only proves
that an export can be produced; it does not validate these sizing choices.

## Rebuild from a clean checkout

Run from the repository root with Python 3.12, uv, curl, a C compiler, make and
the usual C development headers already available. Set `TMPDIR` and
`UV_CACHE_DIR` to writable external directories first. No administrative access
is required. LibreDWG is a separate GPL-licensed executable; the package invokes
it without linking against it. The release source also contains the format-tag
table used by the header reader.

```sh
export PYTHONDONTWRITEBYTECODE=1
export UV_CACHE_DIR="${UV_CACHE_DIR:?set a writable uv cache directory}"
export CAD_WORK="${TMPDIR:?set a writable external scratch directory}/cad-pilot"
mkdir -p "$CAD_WORK/sources" "$CAD_WORK/tools"
curl --fail --location https://ftp.gnu.org/gnu/libredwg/libredwg-0.14.tar.xz \
  --output "$CAD_WORK/sources/libredwg-0.14.tar.xz"
tar -xf "$CAD_WORK/sources/libredwg-0.14.tar.xz" -C "$CAD_WORK/sources"
(
  cd "$CAD_WORK/sources/libredwg-0.14"
  ./configure --prefix="$CAD_WORK/tools" --disable-bindings \
    --disable-docs --disable-shared
  make -j4
  make install
)
uv venv "$CAD_WORK/venv" --python python3.12
uv pip install --python "$CAD_WORK/venv/bin/python" --only-binary=:all: \
  ezdxf==1.4.5 build123d==0.7.0 'cadquery-ocp>=7.7,<7.8' \
  'ocpsvg<0.6' py-lib3mf==2.3.1 pytest==9.1.1 pytest-asyncio==1.4.0
export VIRTUAL_ENV="$CAD_WORK/venv"
export PATH="$VIRTUAL_ENV/bin:$CAD_WORK/tools/bin:$PATH"
export PYTHONPATH=src
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1
dwg2dxf --version
dxf2dwg --version
time uv run --no-project --with pytest python -m pytest tests/specialized/cad_io \
  -q -rA -p no:cacheprovider --basetemp="$CAD_WORK/test-first"
```

For the volume-mesh integration check, install the optional gmsh wheel before
running the tests:

```sh
uv pip install --python "$CAD_WORK/venv/bin/python" --only-binary=:all: \
  'gmsh>=4.13,<5'
```

The available index has no matching Arm Linux Python 3.12 wheel for the 7.7
kernel binding. On that platform, install ezdxf, the runtime drawing
dependencies (Matplotlib and Pillow with NumPy 1.x), and the test dependencies
for a drawing-only run. Solid and mesh tests skip with reasons;
the CLI records unavailable solid or mesh operations as failed. The gmsh check
requires its module and shared libraries. Shared
libraries required by wheels also have to be available. No source build or
alternative mesher is attempted for unavailable wheels.

Earlier synthetic suite time: **3.5 seconds** wall time, with 22 tests passed
and one gmsh integration test skipped. Python 3.12.3, LibreDWG 0.14, ezdxf 1.4.5
and build123d 0.13.0 were used in an isolated environment. That measurement
predates the compatible CAD extra and expanded alias regressions; it does not
validate the currently declared solid toolchain. Dependency download and
LibreDWG compilation are excluded. The acceptance runtime target is about one
minute or less. Use a new `--basetemp` directory for another test run.

## Command and results

```sh
python -m digitalmodel.cad_io synthetic-inputs --out "$CAD_WORK/results"
```

Replace `synthetic-inputs` with a file or folder. Folder discovery is recursive
and selects `.dwg`, `.dxf`, `.step` and `.stp`, case-insensitively. Keep the output
outside an input folder. Files receive deterministic sorted codes such as `F01`,
with independent artifact folders and `results.json` checkpoints. The result
contains each code, format, SHA-256 input digest, status, step results and reason.
Step failures do not stop the remaining files or independent checks.
Before creating output directories or writing files, the command rejects aliases
between any input and any reserved output, and between outputs. Reserved outputs
include reports, checkpoints, drawing metadata and all pending JSON writes;
SVG, converted drawings, round-trip artifacts, solid exports and converter logs
are included too. Resolved paths detect symlinks; filesystem identities detect
existing hardlinks. Rejection returns exit code 2 and preserves the input and
existing artifacts. Standalone converters and round trips also validate their
destinations before writing. This preflight assumes paths remain unchanged
during the run; concurrent filesystem changes are outside its scope.

The printed converted fraction counts readable drawing metadata or successful
STEP surface exports. A converted file can still have failed checks; the summary
reports that count separately. `ok` means all applicable checks completed,
`partial` means conversion succeeded but another check failed, and `failed`
means conversion or input processing failed. An explicitly supplied unsupported
file is `skipped`. Exit codes are 0 for no failed checks, 1 for file/check
failures, and 2 for invalid input/output arguments.
An `ok` round-trip step means that comparison completed; inspect its `equal`,
`delta` and `agreement_percent` fields for differences in entity counts.

Completed per-file checkpoints are reused when input bytes, format and schema
match. Failed outcomes are reused too, so a resumed run does not repeat a long
conversion. Use a fresh output directory after changing tools, repairing an
input or requesting a retry. A changed input digest in an existing output
directory is reported as a failure. Checkpoints are local run records, rather
than an artifact-integrity or tool-version cache.

Historical-input trials remain local. Public reporting contains only file
codes, tried/converted/failed counts, DWG version tags and entity-count agreement
percentages. Recovery audits and unrendered layouts require review before the
outputs are used for engineering decisions.
