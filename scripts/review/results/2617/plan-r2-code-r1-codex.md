VERDICT: MINOR

Main-session Codex adversarial review of the revised plan and final code packet.
Target: plan-r2-code-r1.packet.md; raw-byte hashes: plan-r2-code-r1.manifest.json.

Verified checks:
- The actual pre-change function was run before production edits; the independently generated constant-QTF time histories reproduce the reported discrepancy.
- Pair normalization follows the dimensional wave-amplitude convention. Unique Hermitian pairs contribute amplitude squared divided by two; diagonal terms are constant.
- Sparse real/complex pairs, a one-component sea, diagonal-only QTFs, and signed frequency-varying Newman coefficients exercise the corrected public APIs.
- Full bin weighting handles the final occupied difference-frequency bin. Newman/full equality is checked at 1e-12 relative tolerance.
- FFT force formation is independent of the production variance sum. Nyquist exceeds the largest difference frequency, and sampling confidence uses independent record variances.
- The oscillator prediction calls the repository function with transfer-magnitude weighting. Time-domain lsim provides the reference; transient removal and whole-cycle variance are consistent with the stated single-beat case.
- Locally verified all six direct call sites, the package re-export, pytest slow-marker registration, 357 passing tests, and the seven intended red failures. The frozen packet includes the code, plan, tests and numeric report.

Residual MINOR limitations, also stated in the report: malformed/non-Hermitian/triangular QTF validation and nonuniform quadrature remain outside scope; the oscillator check does not qualify broadband or nonlinear mooring response. No unresolved blocking defect applies to the documented complete-Hermitian, uniform-grid, fixed-amplitude cases.
