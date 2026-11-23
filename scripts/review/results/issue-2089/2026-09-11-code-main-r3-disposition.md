# Main-session code review disposition

Scope: approved milestone 1 of https://github.com/vamseeachanta/digitalmodel/issues/2089.
Actual reviewer verdicts remain preserved; this is not a claim of provider consensus or Claude approval.

- Codex r1 REQUEST_CHANGES (MINOR): work/temp dot aliases bypassed distinct-directory validation. Fixed tests-first with normalized comparison and lexical path rejection; 9 regression tests were added.
- Claude r1: UNAVAILABLE after timeout; no verdict.
- Claude r2: MAJOR, five findings. Main-session r3 disposition follows.
  1. Valid: text stripping preceded the length bound. Fixed tests-first; profiling demonstrates oversized strings reject before strip.
  2. Trailing separators on distinct directories are valid Windows syntax. Equality normalizes paths; packet hashes bind literal metadata. No requirement for one canonical spelling was approved. Documentation clarifies the limit.
  3. Windows reparse checks are observational and do not provide atomic protection against a hostile writer. This remains a deferred live-execution requirement, not a solved containment capability. Stage and capture now explicitly return containment_qualified=False; regression tests and the guide enforce this boundary. The live entry point always raises.
  4. Height 4 m is declared geometry; all tested immersions are below the top. Immersed-box hydrostatics do not independently measure full height. Documentation now makes that assumption explicit.
  5. byte_count+1 bounds coarse LF slots including a final empty slot, not authenticated native lines. No precise line-content verification is claimed. Native locator qualification remains gated separately.
- Integration discovery: all four test modules lacked domain ownership. Added the GHS test root to the existing solver-smoke domain and verified each module is visited. Generalizable findings were promoted to issue 2096.

Validation: final Linux and Windows evidence records 86 passing tests on each platform with zero skips. Main r3 RED/GREEN artifacts preserve the two regression failures and fixes. The independent synthetic rehearsal compares 30 quantities and remains comparison_passed_unreviewed, licensed_execution_verified=False, containment_qualified=False. No licensed solver or native parser was run.

Final production source SHA256:
- __init__.py: 92694625a8108a6bfd920e8842cebfc1805f0b018eba06943aea11f79cabb355
- _canonical.py: 4ca7a266bdede03d6c47b8ee1079bfb2421e0cce281db9291fe267b8b98456f1
- comparison.py: b193f80ea8476815a81499ff9372902d7a3721e540cbf4b8346ae7cc020e0676
- contracts.py: 9e68b82cd9db608b088327f8f1451c1e46d4b690f279a57d12176917973b8d0e
- runner.py: 364c67969ad59d4b19d81f3c07f3d416bb2b516c2c9161a52e381ef20d75981b
