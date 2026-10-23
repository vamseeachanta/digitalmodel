# Content qualification and retrieval

The qualification helper reports why each candidate must be retained or needs
review. It reads the declared consumers without running them. It neither removes
payloads nor changes the manifest. Retrieval restores the manifest bytes after
size and SHA-256 checks.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Documents dependency inspection and retrieval edge cases so unresolved content remains retained. |

## Qualification method

Run from the repository root, repeating each option for additional paths:

```sh
python scripts/data/qualify.py --candidate docs/example.csv --consumer scripts/example.py
```

The paths must exist within the checkout. Executable extensions, executable mode
bits and script headers take precedence over a documentation directory.
Configuration formats, including GIS projects, also remain protected. Consumers
may name an exact file, an extensionless model, a parameterized name or a directory.
Quoted paths may contain spaces. Relative references are checked against both the
consumer directory and the repository root; ambiguous matches retain all matches.

Python inspection follows literal assignments, aliases, path concatenation and
formatted strings to calls. Recognized CSV reads establish input edges; recognized
CSV writes establish generator edges. A later read takes precedence over a write.
Control flow and unresolved arguments remain explicitly unresolved. This is a
bounded static analysis and cannot prove arbitrary program data flow.

Compressed GIS projects are read as ZIP containers of QGS XML without extracting
members. Datasources are resolved relative to the containing project. A shapefile
reference retains all same-stem dataset components among the candidates,
including compound metadata sidecars. Unsupported providers and unresolved
dependencies prevent a removal decision.

| Disposition | Meaning |
| --- | --- |
| `RETAIN_REQUIRED` | Executable/configuration content or an identified consumer needs this candidate. |
| `RETAIN_UNRESOLVED` | No confirmed consumer, but missing dependencies, unsupported flow or an empty consumer scope prevent qualification. |
| `REVIEW_REQUIRED` | No required consumer was identified within the declared scope; further review is required before any removal. |

Each JSON row carries its path, content kind, input/output role, consumers,
generators and unresolved reasons. An output-only CSV receives the role
`generated-output`; that label does not establish that current exporters reproduce
its historical bytes. The consumer list is an explicit inspection scope, not a
claim of complete repository coverage.

## Worked example

Inputs: a generator reads `raw.csv` through an alias and writes `out.csv` through
another alias. The following example creates disposable files using the configured
temporary directory. It never imports or executes the generator.

```python
from pathlib import Path
from tempfile import TemporaryDirectory
from runpy import run_path

qualify = run_path("scripts/data/qualify.py")["qualify"]
with TemporaryDirectory() as folder:
    root = Path(folder)
    (root / "raw.csv").write_text("value\nexample\n")
    (root / "out.csv").write_text("value\nexample\n")
    (root / "generate.py").write_text(
        'source = "raw.csv"\ntarget = "out.csv"\n'
        'frame = pd.read_csv(source)\nframe.to_csv(target)\n'
    )
    for row in qualify(root, ["raw.csv", "out.csv"], ["generate.py"]):
        print(row["path"], row["role"], row["disposition"])
```

Method: resolve `source` and `target`, then identify the read and write calls.
The independently expected answer follows directly from those literal edges:

```text
raw.csv input RETAIN_REQUIRED
out.csv generated-output REVIEW_REQUIRED
```

The regression suite executes this documentation example and checks these literal
answers. Additional disposable fixtures cover extensionless models, directory
consumers, parameterized paths, compressed GIS projects, sidecars, unknown
dependencies and a generated output subsequently read by another consumer.

## Retrieval behavior

`scripts/data/fetch.py` first validates the destination and checks existing bytes.
A file already matching both manifest size and SHA-256 is reused, even when no
store is configured. A mismatching file is replaced only after staged verification.
Symlink destinations and Git metadata paths are rejected before traversal.

Creator read/write access survives restrictive POSIX creation masks. New parent
directories retain creator traversal access; existing parent permissions are
unchanged. Restored payloads have no executable bits, and masked group/other
permissions remain restricted.

The connection timeout remains separate from transfer duration. There is no
default total transfer limit; `--transfer-timeout` accepts a finite positive
duration in seconds when a caller needs one. A total-duration expiry is a transfer
failure and does not become a store-unavailable skip. The transfer subprocess uses
the C locale so connection diagnostics do not depend on the caller's locale.
Remote transfer tests use disposable mocks; live transfer is not established.

Legacy generated CSV layouts are retained by manifest digest. They are restored
as recorded, rather than regenerated by current exporters. A generator's presence
alone establishes neither schema compatibility nor byte reproduction.

## Sources and limits

- Issue #2554 defines the retention policy and the edge cases; the literal test
  fixtures independently define the expected dependency graphs.
- [Python 3.11 AST reference](https://docs.python.org/3.11/library/ast.html),
  node classes for assignments, calls, constants and formatted strings.
- [QGIS 3.40 User Guide, section 26.1.1](https://docs.qgis.org/3.40/en/docs/user_manual/appendices/qgis_file_formats.html#qgs-qgz-the-qgis-project-file-format),
  compressed project containers and external layer references.
- [Python 3.11 stat flags](https://docs.python.org/3.11/library/stat.html),
  creator permission bits and file mode semantics.
- [Git repository layout](https://git-scm.com/docs/gitrepository-layout),
  Git directories and gitfiles.
- [OpenBSD scp manual](https://man.openbsd.org/scp), connection options and exit
  status. A nonzero exit alone does not establish a connection failure.
- `pyproject.toml`, the project test extra, supplies focused CI version constraints.

No engineering equations, coefficients or acceptance limits apply. General
interprocedural analysis, exhaustive consumer discovery, compressed formats other
than QGZ, live transfer and complete repository-suite validation remain outside
the established evidence. No removal is authorized by this helper.
