# Pipelay screening evidence manifest

## Summary

An issued screening summary now has a received-config snapshot and a digest
manifest beside it. Readback verification detects changed or missing bytes and
refuses unsupported manifests. This is evidence consistency, not physical
qualification, design approval or an authenticated signature.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-22 | Added the issued evidence contract and a reproducible synthetic example to make byte changes detectable. |

Table 1. Initial issue of this note.

## Inputs and retained artefacts

`router(cfg)` saves the entire config as received, before defaults and returned
results mutate it: `json.dumps(cfg, sort_keys=True, allow_nan=False)`, encoded as
UTF-8. This is a JSON snapshot of the received mapping; it is not the original
YAML file, its comments, or the bytes before engine preprocessing. An unserialisable
or nonfinite config is refused before a summary is written.

For a summary called `screening.json`, the siblings are
`screening.json.input.json` and `screening.json.manifest.json`. The latter records
the schema version, hash algorithm, distribution metadata version as
`tool_revision`, and the filenames and digests of the input and output.
Unavailable distribution metadata is `NOT_EVALUATED`; a source commit is not
inferred from a package version. The manifest hashes the saved input and summary
by reading their bytes from disk. Existing summary keys and criteria are unchanged.

All geometric values below are synthetic. Ancillary pipe properties and strain
criteria are inherited template assumptions, assumed, based on previous
experience; they are not standard allowables or qualified material data. All
inputs, including the output locator, are shown:

```json
{
  "basename": "pipelay",
  "pipelay": {
    "config": {
      "allowable_overbend_strain": 0.002,
      "allowable_reeling_strain": 0.02,
      "reel_core_radius_m": 10.0,
      "residual_out_of_straightness_span_m": 8.0,
      "straightener_efficiency": 0.8
    },
    "method": "Reel-Lay",
    "outputs": {
      "directory": "issued",
      "summary_json": "screening.json"
    },
    "pipe": {
      "coating_density": 0.0,
      "coating_thickness": 0.0,
      "outer_diameter": 0.2,
      "seawater_density": 1025.0,
      "smys": "448.0e6",
      "steel_density": 7850.0,
      "wall_thickness": 0.0191,
      "youngs_modulus": "207.0e9"
    }
  }
}
```

## Method and worked answer

The local derivation is [screening-basis.md](screening-basis.md), equations E5-E6:
outer-fibre strain is half the diameter times curvature;
residual curvature is the unrecovered fraction of reel curvature. Integrating a
constant small-slope curvature over a symmetric span gives the sagitta expression.
For this synthetic set the arithmetic is:

```text
reeling strain = D / (2 R) = 0.010000
residual curvature = (1 - eta) / R = 0.02000000 /m
residual radius = 1 / curvature = 50.0 m
residual strain = D curvature / 2 = 0.002000
sagitta = curvature L^2 / 8 = 0.16000 m
```

The answer below is read from the issued summary, including the screen result
against the supplied illustrative criterion:

```json
{
  "allowable_reeling_strain": 0.02,
  "applied_defaults": {
    "pipe.poissons_ratio": 0.3
  },
  "method": "Reel-Lay",
  "min_stinger_radius_m": 50.0,
  "pipe_outer_diameter_m": 0.2,
  "pipe_submerged_weight_N_per_m": 519.839,
  "reel_core_radius_m": 10.0,
  "reeling_bending_strain": 0.01,
  "reeling_strain_utilisation": 0.5,
  "residual_bending_strain": 0.002,
  "residual_curvature_1_per_m": 0.02,
  "residual_out_of_straightness_m": 0.16,
  "residual_out_of_straightness_span_m": 8.0,
  "residual_radius_m": 50.0,
  "residual_radius_status": "finite",
  "screening_status": "pass",
  "straightener_efficiency": 0.8
}
```

The issued manifest is read back without modifying its fields:

```json
{
  "hash_algorithm": "sha256",
  "input": {
    "file": "screening.json.input.json",
    "sha256": "2bf921e06a2a0828f281092282e58207b73ff747890f60e2de374415e580f0af"
  },
  "output": {
    "file": "screening.json",
    "sha256": "10da38fe5c1ed88f847e2c587e22cecaa2b5bb972050e8981d77b5190c8efc97"
  },
  "schema_version": 1,
  "tool_revision": "NOT_EVALUATED"
}
```

Intact verification result:

```json
{
  "failures": [],
  "ok": true
}
```

Changing the initial opening brace of the saved input to an opening square bracket
changes exactly one byte. The verifier compares bytes, without trusting or parsing
the modified input. Its refusal is:

```json
{
  "failures": [
    {
      "artefact": "input",
      "reason": "digest_mismatch"
    }
  ],
  "ok": false
}
```

## Verification contract and limits

Call `verify_evidence_manifest(Path(...))`. `ok` is true only when `failures` is
empty. Failures name `input`, `output` or `manifest` with a reason:
`digest_mismatch`, `missing_file`, `unreadable_file`, `invalid_json`,
`invalid_schema`, `unsupported_schema_version`, `unsupported_hash_algorithm` or
`unknown_keys`. Duplicate keys are invalid JSON for this contract. Artefact names
must match the expected siblings. Missing fields, malformed digests and unknown
nested keys fail closed. Both changed artefacts can be reported together.

Retain the whole bundle and use a fresh output destination for each issue. The
existing destination overwrite behaviour is unchanged. Issuance is not a
transactional archive: an interrupted write can leave an incomplete bundle,
which verification refuses. A test-only hash in a test file is not an issued
manifest. The unsigned manifest cannot detect coordinated replacement of the
manifest and its artefacts. No signer identity, timestamp, original source
revision or protected chain of custody is established.

The manifest does not establish physical validation of the flexible-line
approximation, straightener efficiency, cyclic plasticity or ovalisation. It
does not establish dynamic installation response or suitability of a strain
criterion. No native solver is executed. The catenary arithmetic checks are
consistency checks of a static, inextensible, uniformly weighted flexible line.

## Sources and NOT_EVALUATED

- [Bai and Bai, Subsea Pipeline Design, Analysis and Installation](https://www.sciencedirect.com/book/9780123868886/subsea-pipeline-design-analysis-and-installation)
  (2014), chapters 5 and 6: inherited equation-form background in
  `workflow.py`. Publisher access returned HTTP 403; chapter text and page
  confirmation are NOT_EVALUATED. No inaccessible allowable is used.
- [Classical Mechanics, The Catenary](https://phys.libretexts.org/Bookshelves/Classical_Mechanics/Classical_Mechanics_(Tatum)/18%3A_The_Catenary), sections 18.2
  and 18.3 (public text, last modified 2022): force
  resolution, catenary parameter, arc length and tension identities were opened.
  These support independent arithmetic closure tests, not physical validation.
- NOT_EVALUATED: rights-cleared public measured or experimentally qualified
  J-lay and reel-lay benchmark datasets and their versioned physical tolerances.
  None was supplied or established by the issue or opened references. No licensed,
  private or inaccessible dataset is substituted for independent evidence.

## Reproduction

Generated by `scripts/pipelay/generate_evidence_note.py`. Run with `PYTHONPATH=src`,
`--work-dir` set to a fresh scratch directory outside the repository and `--output`
set to this note's path. The script retains the intact bundle and tampered copy
in scratch. Its regression test regenerates this entire note, including every
computed quantity, both artefact digests and the refusal. Only the displayed
distribution metadata version is normalized when comparing environments;
no digest or result is normalized. Bibliographic identifiers and revision
metadata are rendered by the same script.
