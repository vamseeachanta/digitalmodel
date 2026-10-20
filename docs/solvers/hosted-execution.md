# Hosted execution contracts

The opt-in `digitalmodel.workflows.hosted_execution` module provides resource,
destination and executable checks for new hosted executors. Existing executors
remain unchanged; adoption requires the follow-up work below. No engineering
solution or licensed solver result is produced by these infrastructure checks.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Add reusable execution contracts to address pathname replacement, scalar coercion and nested checkout boundaries. |
| 1 | 2026-10-19 | Enforce iterator ceilings and bounded requests, clarify executable memory-file failures, and skip incompatible utility aliases in tests. |

## Inputs, method and guarantees

- `resource_count(value, name=..., ceilings=...)` accepts positive built-in
  integers only, including each ceiling. The invariant is
  `0 < value <= min(ceilings)` when ceilings are supplied; an empty ceiling set
  checks positivity only. This is the policy in
  [issue 1596](https://github.com/vamseeachanta/digitalmodel/issues/1596), not an
  engineering acceptance limit. Booleans, numeric strings and floats are refused.
- `result_locator(value)` accepts slash-separated ASCII identifiers starting
  with an alphanumeric character. No normalization can hide a traversal,
  absolute path, drive prefix, control character or empty component. Locators
  identify results relative to the operator-provided root; they are not host paths.
- `destination_fd(root, locator)` walks every existing component from filesystem
  root through the effective destination with `dir_fd`, `O_DIRECTORY` and
  `O_NOFOLLOW`. A `.git` object of any kind (including a linked-worktree file or
  dangling link) rejects the ancestry. The root and effective destination must
  already exist. No directory is created during validation. The returned
  descriptor stays open for descriptor-relative operations inside the context.
- `witnessed_argv(argv, executable_digests)` copies each declared executable to
  an anonymous memory file, seals it against writes/resizing, then checks its
  SHA-256 against the caller-supplied digest. Every declared executable position
  becomes an inherited descriptor locator. Position zero is mandatory. The
  kernel launches that sealed snapshot rather than reopening its source pathname.
  Source replacement and in-place modification cannot change the snapshot.
- `validated_launch(...)` checks resource counts, destination ancestry and all
  executable digests before invoking the mutation callback. Its callback receives
  `(destination_fd, count, relative_locator, witnessed_plan)`. `validated_request`
  provides the corresponding count/destination contract without launching tools.
  Both require at least one ceiling, including when ceilings are supplied as an
  iterator; an empty iterable is rejected before destination access or mutation.
- `run_witnessed(plan, timeout=..., cwd_fd=...)` executes only while the witness
  context remains open, with an empty environment, captured output and optional
  descriptor-bound working directory. The required positive finite timeout
  bounds the direct child only. A nonzero exit is returned, not converted into
  success. The caller is responsible for process-tree containment and bounded log
  retention. Execution permission denial raises `NotImplementedError` with a
  runtime or kernel policy diagnostic.

## Worked example: resource request

Inputs are synthetic: requested ranks `2`, visible capacity `4`, dispatcher
capacity `3`. Hand calculation: `min(4, 3) = 3`; `0 < 2 <= 3`, so the answer is
`2`. Changing the request to `4` gives `4 > 3`, so it is rejected. The values
are illustrative inputs, not recommended hardware settings.

```python
from digitalmodel.workflows.hosted_execution import resource_count, result_locator

assert resource_count(2, name="ranks", ceilings=(4, 3)) == 2
assert result_locator("batch/case/result.json") == "batch/case/result.json"
```

A caller with a precreated destination and independently approved executable
SHA-256 can compose validation and execution as follows. All paths and capacities
are supplied by the operator; no executable is discovered automatically.

```python
from digitalmodel.workflows.hosted_execution import validated_launch, run_witnessed


def execute(root, namespace, ranks, capacities, executable, approved_digest, timeout):
    def after_validation(destination, count, locator, plan):
        result = run_witnessed(plan, timeout=timeout, cwd_fd=destination)
        return {"locator": locator, "returncode": result.returncode}

    return validated_launch(
        root, namespace, ranks, capacities, [executable],
        {0: approved_digest}, after_validation,
    )
```

For a local launcher with a nested executable in a later argument, the caller
must supply the digest for that argument index as well. The contract tests use
public utility binaries and show a launcher actually executing the nested sealed
binary. No licensed executable is imported, queried or started by the tests.

## Trust boundary and limitations

Linux, procfs descriptor access, Python `os.memfd_create` and file sealing support
are required. Unsupported platforms or runtimes fail closed; no weaker pathname
fallback is provided. Only ELF executables are accepted; scripts would add an
unwitnessed interpreter. Copying must match the approved digest, even if the
source is modified during the copy.
When the runtime defines `MFD_EXEC`, snapshots request it explicitly. Otherwise,
execution remains subject to the kernel's default memory-file execution policy.

The caller must identify **every** nested executable position and use a local
launcher that preserves these inherited descriptors. Remote MPI, launchers that
close descriptors and Windows/API workers are **NOT_EVALUATED** by this module.
Executable-relative resource discovery may differ for memory-backed binaries.
The OS, dynamic loader, shared libraries and Python caller are trusted. An empty
child environment removes inherited loader overrides, but is not an OS sandbox.

The operator must exclusively control the destination directory tree throughout
validation and use. Descriptor-relative opens prevent symlink substitution from
redirecting an operation. They do not prevent a privileged actor or another writer
from reparenting an already-open directory or adding a Git marker after the check.
No claim of atomic filesystem-policy enforcement against such an actor is made.
The callback must use the descriptor, perform its workflow-specific pure checks
before mutation, and serialize only validated relative locators. Raw OS errors,
subprocess arguments and tool output are internal diagnostics; the caller must
sanitize them before external publication. Callback failures are not rolled back.

## Adoption inventory

A source search of `src/digitalmodel/{solvers,workflows,hydrodynamics}` for
subprocess launches and hosted/root configuration identifies these adoption
candidates. Source presence does not establish deployment or runtime coverage.

| Execution path | Current gap | Separately planned adoption |
| --- | --- | --- |
| OpenFOAM batch configuration, layout and execution | Scalar coercion, pathname hash witnesses, namespace ancestry, mutation order and external locators need migration | [3039](https://github.com/vamseeachanta/digitalmodel/issues/3039) |
| OrcaFlex batch and reproduction workflows | API worker resources/results and interpreter/DLL identity need a platform-specific contract | [3040](https://github.com/vamseeachanta/digitalmodel/issues/3040) |
| CalculiX, MYSTRAN, BEMRosetta, AQWA and OrcaWave subprocess adapters | Direct subprocess execution needs assessment before hosted enrollment | [3040](https://github.com/vamseeachanta/digitalmodel/issues/3040) |
| OpenFOAM runner, mesh preparation, smoke and warm-start helpers; solver benchmark and probe utilities | Auxiliary launches need caller/interface inventory during adoption | [3039](https://github.com/vamseeachanta/digitalmodel/issues/3039), [3040](https://github.com/vamseeachanta/digitalmodel/issues/3040) |

No host setup, queue work, solver execution or automatic migration is included.

## Sources and tests

- Python 3.11, [os file and directory operations](https://docs.python.org/3.11/library/os.html#files-and-directories): descriptor-relative operations and `memfd_create`.
- Python 3.11, [subprocess.Popen](https://docs.python.org/3.11/library/subprocess.html#subprocess.Popen): executable selection and descriptor inheritance.
- Linux man-pages 6.19 (2026), [memfd_create(2)](https://man7.org/linux/man-pages/man2/memfd_create.2.html), sealing example: immutable anonymous file lifetime and seal flags.
- Linux man-pages 6.19 (2026), [elf(5)](https://man7.org/linux/man-pages/man5/elf.5.html), `e_ident`: ELF identification bytes.
- Linux man-pages 6.19 (2026), [inode(7)](https://man7.org/linux/man-pages/man7/inode.7.html), file mode: executable permission bits.

`tests/workflows/test_hosted_execution.py` supplies synthetic scalar hand
calculations, invalid locators, nested Git markers, symlink rejection, descriptor
lifetime, no mutation on invalid requests, digest mismatch, pathname replacement,
sealed-write refusal and actual nested descriptor execution. Test watchdog
values are synthetic runtime inputs, not engineering criteria. Cache ancestry
above the isolated fixture root is excluded in tests because an approved cache
can itself reside inside a checkout; root and nested checks inside the fixture
use real filesystem descriptors. Production checks have no such exclusion.
