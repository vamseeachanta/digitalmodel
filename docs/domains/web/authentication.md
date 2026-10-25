# Web authentication

Protected web examples compare live environment credentials as UTF-8 bytes.
Matching input is accepted; unequal, empty or malformed input is refused.
Flask-HTTPAuth **4.8.1** was measured with real Flask test-client requests.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| A | 2026-10-22 | Replace deprecated callbacks with a shared verifier to handle Unicode passwords and reject malformed wire encodings. |
| B | 2026-10-25 | Record local test counts and the hosted aggregate cancellation evidence to distinguish measured authentication results from pending domain coverage. |

## Inputs and method

Application modules read `DIGITALTWINFEED_BASIC_AUTH_USER` and
`DIGITALTWINFEED_BASIC_AUTH_PASSWORD`. The independent template reads
`TEMPLATE_CODE_BASIC_AUTH_USER` and `TEMPLATE_CODE_BASIC_AUTH_PASSWORD`.
Settings are read on each request. Missing or whitespace-only settings close
authentication. Nonblank values retain their whitespace and Unicode spelling.

Let `U` and `P` be the configured strings and `u` and `p` the presented strings.
After checking for nonblank configuration and nonempty presented fields:

```text
accept = compare_digest(UTF8(u), UTF8(U)) AND compare_digest(UTF8(p), UTF8(P))
```

Both comparisons run before their boolean results are combined. The method
uses Python's [`hmac.compare_digest`](https://docs.python.org/3.11/library/hmac.html#hmac.compare_digest)
(Python 3.11 Library Reference), which requires ASCII-only strings or bytes.
UTF-8 bytes support non-ASCII input without the string-comparison exception.
There is no normalization, case folding or trimming of nonblank credentials.
The comparison count is bounded; request processing time can depend on input
length and is not claimed to be constant.

The [`verify_password` callback](https://flask-httpauth.readthedocs.io/en/latest/#flask_httpauth.HTTPBasicAuth.verify_password)
receives the user and password and returns a boolean. Inspection of the
installed **4.8.1** API confirms that it bypasses `get_password` and
`hash_password`. The shared verifier lives in
`src/digitalmodel/web/authentication.py`; both environment providers delegate
to it. Legacy password-reader functions remain for compatibility, with no
active `get_password` registration.

`UTF8BasicAuth` validates Base64 and UTF-8 before invoking the library parser.
This rejects ignored non-alphabet characters and the library's Latin-1 fallback:
[`RFC 4648`](https://www.rfc-editor.org/rfc/rfc4648), section 3.3 (2006),
and [`RFC 7617`](https://www.rfc-editor.org/rfc/rfc7617), sections 2 and 2.1
(2015). Clients must send UTF-8. Existing protected routes retain their success
and refusal status contracts. The parser adapter depends on the pinned library
API; library upgrades require re-running the request matrix.

## Worked request

Run the committed example with the declared web dependencies available:

```sh
PYTHONPATH=src python scripts/web/authentication_example.py
```

The script generates a random ASCII user and a password consisting of U+00E9,
U+5BC6 and U+1F512 followed by random ASCII text. These code points come from
the existing `tests/web/test_web_unicode_auth.py` fixture; they are test inputs,
not password policy. Generated credentials are not printed or persisted.
The request is constructed as follows:

```text
GET /protected/
Authorization: Basic Base64(UTF-8(generated_user + ":" + generated_password))
```

The script registers the old reader with the installed library to reproduce
the old result, then requests the updated application and template routes.
Measured results for each provider:

| Input | Deprecated callback | Shared verifier |
| --- | --- | --- |
| Exact generated Unicode password | `TypeError: comparing strings with non-ASCII characters is not supported` | HTTP 200, accepted |
| Generated password followed by `x` | The same string-comparison defect applies | HTTP 403, refused |

The independent calculation is equality: a string and itself have identical
UTF-8 bytes; appending `x` changes those bytes. The route handlers supply HTTP
200 and HTTP 403. `tests/web/test_authentication_example.py` re-runs the example;
the request matrix separately measures incorrect Unicode passwords through
each provider. It also checks ASCII credentials, Unicode user names, live
setting changes, missing/blank settings, empty passwords, malformed encodings
and a long-header probe. The probe size is derived from generated input; it
does not set or establish a supported header-size limit.

## Related test triage

The following outcomes were measured against the clean starting tree, equal
to `origin/main`, and the updated tree. No dependency, workflow or legal-gate
change was required for the six tests reported in #3191.

| Reported test | Before | After | Classification and action |
| --- | --- | --- | --- |
| `tests/legal/test_check_identifiers.py::TestTheExistingBacklog::test_the_backlog_has_not_grown` | Not found | Not found | Stale issue reference. Main already replaced the class with `TestManualAudit`; its tests pass. No count or gate changed. |
| `tests/legal/test_check_identifiers_c11.py::TestCiRunsTheGate::test_the_quality_workflow_scans_the_whole_tree` | Not found | Not found | Stale issue reference. Main's `TestCiRetirementContract` passes under the documented manual-audit policy. No workflow changed. |
| `tests/marine_ops/fowt_quasi_static/test_dependencies.py::test_solver_extra_declares_the_executed_versions` | Pass | Pass | Main already declares the retained optional dependency versions. |
| `tests/marine_ops/fowt_quasi_static/test_dependencies.py::test_workflow_import_and_non_solver_functions_with_solvers_absent` | Pass with declared runtime dependencies | Pass | Minimal test environment initially lacked `xarray`; supplying the already-declared dependency resolves it. |
| `tests/capabilities/test_generated_html_freshness.py::test_every_registered_page_is_byte_identical_to_its_generator` | Pass with declared runtime dependencies | Pass | Environment preparation required. `pylife` is already a runtime dependency; no skip or dependency change is needed. |
| `tests/capabilities/test_generated_html_freshness.py::test_regenerated_pages_inherit_core_brand_tokens` | Pass with declared runtime dependencies | Pass | Same environment requirement; every registered generator ran. |

The requested isolated command, `PYTHONPATH=src python -I -m pytest`, cannot
collect the freshness module because `-I` ignores `PYTHONPATH` and its
`scripts` import needs the repository root. The repository's normal invocation,
`PYTHONPATH=src:. python -m pytest`, measures the freshness outcomes above.
The dependency tests also passed with `-I`. The first literal selection of all
six nodes could not run because the retired nodes do not exist.

Initial environment failures included missing `seaborn` and a NumPy/Arrow
incompatibility. Using the existing project constraints (`numpy<2`, `pandas<3`,
`pyarrow==14.0.1` from `pyproject.toml`) and declared dependencies resolved the
freshness setup errors. Tests were not changed to conceal missing packages.

## Limits and NOT_EVALUATED

Basic authentication requires transport protection; Base64 provides no
confidentiality (RFC 7617, section 4). These examples provide no rate limiting.
Credentials are supplied through the environment. Whole-request timing,
deployment transport settings, proxy header limits, brute-force resistance,
Unicode normalization policy and production suitability are **NOT_EVALUATED**.
Unprotected business routes are unchanged; testing an authentication object
does not establish that every business route requires authentication.

The commented callback in
`src/digitalmodel/web/digitaltwinfeed/PlotlyPlot/PlotlyPlot.py` remains inactive.
Whether those public routes should require authentication is **NOT_EVALUATED**.
The compatibility readers are retained; calling them from a newly registered
deprecated callback would reintroduce the original defect.

The broader domain suites have unrelated baseline and environment failures,
recorded with commands in the change description. In particular, Capytaine
3.0.0 has no usable wheel in this environment; its dependent tests could not
complete. No solver installation was built from source and no licensed solver
was started or imported. The passing dependency tests do not establish that
the optional solvers execute here.

## Validation evidence

On 2026-10-25, the complete web suite passed locally: **524 passed**, including
**384 tests in the four changed test files**, with no failures or skips. The
command was `PYTHONPATH=src:. python -m pytest tests/web/ -q -rA -p no:cacheprovider`
with temporary output outside the repository. The environment used Python 3.12.15,
pytest 8.4.2, Flask 3.1.3, Flask-HTTPAuth 4.8.1 and Flask-RESTful 0.3.10.
These counts establish web-suite results only; the broader-suite figures in
the change description were not rerun.

The hosted aggregate log for run `39830604607`, job `120057461154`,
reports `CI harness detector tests did not succeed: cancelled`. The run's job
metadata records 23 cancelled jobs, all with no executed steps, and one failed
aggregate. The cancellation annotations for the harness and the web-containing
`tests-misc` job confirm explicit cancellation. Across all checks on the same
head, eight succeeded. This is evidence of incomplete hosted execution;
it provides no domain-test result and establishes no authentication defect.
The aggregate requires the harness to succeed, as does the current main
workflow. A completed hosted run remains necessary before drawing conclusions
about domain coverage. No workflow or gate change is indicated by this evidence.
