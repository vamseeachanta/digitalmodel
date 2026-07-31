Here is the adversarial review of the Task 1 parent contract implementation.

### 1. MAJOR: Fail-Closed Composition Flaw in `existing_envelope_extensions`
**File:Line:** `src/digitalmodel/contract_validation/riser_parent_contract.py:165-166`
```python
        if isinstance(extension, dict):
            envelopes[name].update(copy.deepcopy(extension))
```
**Vulnerability:** The contract design directive specifies `"extend_effective_minimum_bindings_or_discriminated_union"`. However, the implementation applies a blind `.update()` to the existing envelope dictionary. This allows a malicious `host_motion_component.yaml` to overwrite arbitrary security-critical envelope properties (like `producer`, `consumers`, or `covers`) rather than strictly appending bindings.
**Counterexample:** An attacker provides the following in their host component:
```yaml
existing_envelope_extensions:
  normalized_case:
    producer: "attacker_controlled_owner"
```
Because `_validate_envelopes` only checks that the producer is *some* valid owner, the attacker successfully hijacks the producer of the `normalized_case` envelope, bypassing fail-closed composition constraints.

### 2. MAJOR: Fail-Closed Composition Flaw via Blind Assurance Overwrite
**File:Line:** `src/digitalmodel/contract_validation/riser_parent_contract.py:307-313`
```python
    for key, value in assurance.items():
        if key not in skip and key != "planning_validation":
            result[key] = copy.deepcopy(value)
```
**Vulnerability:** `_compose_assurance` iterates over the assurance dictionary and blindly overwrites any keys in `result` that are not explicitly skipped. Crucial top-level keys like `owners`, `interface_envelopes`, `milestone_DAG`, and `identity_chain` are missing from the `skip` list. This completely destroys the careful merging done earlier in `_compose` and allows the assurance component to hijack the entire contract definition.
**Counterexample:**
```yaml
# in assurance_component.yaml
owners:
  drilling_workflow:
    issue: "https://github.com/attacker/malicious/issues/1"
    role: "hijacked_role"
milestone_DAG:
  edges: [] # Maliciously drop all blocking base dependencies
```
The exact semantic checks in `_validate_effective_declarations` (e.g., `set(composed["owners"]) != expected_owners`) only verify dictionary **keys**, meaning the malicious overwriting of the values inside `owners` or `milestone_DAG` edges will pass validation completely unnoticed.

### 3. SECURITY / TOCTOU: Payload Cache Poisoning via Aliased Manifest Paths
**File:Line:** `src/digitalmodel/contract_validation/riser_parent_contract.py:118-125`
```python
        # ...
        payloads[declaration["path"]] = data
```
**Vulnerability:** The `_read_bound_files` function caches file data using the raw string `declaration["path"]` as the key. The paths of non-component declarations (like `review_candidate_plan`) are evaluated *after* `base` and `components`, and crucially, their paths are never verified for uniqueness by `validate_manifest_declarations`. An attacker can alias the `base` path in `review_candidate_plan` and use a TOCTOU race condition to bypass the cryptographic binding of the base contract.
**Counterexample:**
```yaml
# in manifest.yaml
composition:
  base:
    path: "issue-1602-riser-analysis-contract-v1.yaml"
    sha256: "<hash_of_legitimate_base>"
# ...
review_candidate_plan:
  path: "issue-1602-riser-analysis-contract-v1.yaml" # Exact same path!
  sha256: "<hash_of_malicious_base>"
```
By concurrently swapping the `issue-1602-riser-analysis-contract-v1.yaml` file on disk between the first read (for `base`) and the second read (for `review_candidate_plan`), both hash checks will pass. However, the `payloads` dictionary will be overwritten with the malicious file's contents, causing `_load_components` to blindly parse and compose using the malicious base data.

### 4. Test Gaps
**File:** `tests/validation/test_riser_parent_contract_adversarial.py`
The adversarial tests completely fail to assert structural integrity within composed dictionaries. They assert that `section in composed` or that simple scalars match, but they lack deep assertions to ensure that base keys/values within `owners`, `interface_envelopes`, or base DAG edges haven't been mutated or dropped. They also fail to test overlapping/aliased component paths in the manifest.

### Verdict
MAJOR
