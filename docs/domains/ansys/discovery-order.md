# ANSYS executable discovery order

`ANSYSRunner` selects the first available executable in this order:

1. An explicit `ANSYSRunConfig.executable_path`. If it is not a file, discovery
   returns no executable and does not fall back to another source.
2. `ANSYS_MAPDL_PATH`, `MAPDL_PATH`, then `ANSYS_EXECUTABLE`. Empty values and
   paths that are not files are ignored.
3. Standard Windows installations under `C:\Program Files\ANSYS Inc`.
   Release directories must match `v` followed by ASCII digits only. Releases
   sort by their numeric value in descending order: `v261`, `v100`, `v99`.
   Equal numeric values sort by directory name in descending order, so `v99`
   precedes `v099`. Only existing files at `ansys/bin/winx64/MAPDL.exe` are
   candidates. Other platforms return no standard-installation candidates.
4. PATH lookup for `mapdl`, `ansys`, `MAPDL.exe`, then `ansys.exe`.

Explicit deployment choices precede ambient discovery. Standard installations
precede PATH so an older generic launcher on PATH cannot override the newest
recognized installation. Environment-variable precedence is retained for
compatibility with configured deployments. Relative explicit paths resolve
before execution changes the working directory.

Discovery selects a candidate; it does not establish executable authenticity,
a detected release, licence availability or successful solver completion.
Versioned launcher names, redacted provenance, unknown-release refusal,
normal-termination evidence and stream classification remain work under #1524.

The offline contracts are in `tests/ansys/test_runner_discovery_order.py`.
They use synthetic files and mocked platform, environment and PATH responses;
the process boundary raises if reached. No installed solver is inspected or run.
