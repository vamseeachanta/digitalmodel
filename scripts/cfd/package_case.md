# Package an OpenFOAM case

Use the packager from the repository root after case writers have stopped:

```sh
python scripts/cfd/package_case.py cases/example archives/example.tar.gz
python -m tarfile --list archives/example.tar.gz
```

The archive parent directory must already exist. Use a new archive path outside
of the case. Existing files are never overwritten. The source case is unchanged;
no solver or case script is executed.

A digit-prefix tar exclusion such as `--exclude='[0-9]*'` also matches
`0.orig`. The packager instead applies the complete numeric-time name grammar
from `scripts/cfd/prune_case.sh` to root directories only:

```text
[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?
```

For example, `0/`, `12/`, `0.25/` and `1e-05/` are omitted; `0.orig/`,
`1.0.orig/`, `2notes/`, numeric regular files, and numeric directories beneath
`constant/` or `postProcessing/` are retained. These examples are filename
classification examples, not engineering values.

The initial `0/` directory is omitted by this policy. Cases that store their
only initial fields there require a different packaging policy. Processor trees,
meshes, logs and other nested results remain included; this command does not
produce a minimal input-only bundle.

Symbolic links are stored without following their targets. Links can reference
locations outside the archive; inspect them before extracting elsewhere. Archive
account metadata is cleared. The archive contains relative case contents without
an enclosing case directory. It is not a snapshot of a changing case or a
byte-reproducible archive. An I/O failure may leave an incomplete output; retry
with a fresh destination after checking the error.

The archive behavior uses the Python 3.11 Library Reference sections for
[`tarfile.open`, `TarFile.add` and `TarFile.dereference`](https://docs.python.org/3.11/library/tarfile.html).
The regression is tracked in [issue 2057](https://github.com/vamseeachanta/digitalmodel/issues/2057).
