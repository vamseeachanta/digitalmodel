An adversarial review of the proposed #1602 parent contract validator reveals multiple critical violations of the fail-closed "merge-only" and "no arbitrary overwrite" mandates. The composition logic actively ignores the `EXPECTED_DIRECTIVES` and applies destructive dictionary updates (`.update()`, `**` unpacking, and direct assignment). This allows malicious or compromised components to silently wipe out or hijack base contract rules.

### Vulnerabilities: Arbitrary Overwrites & Directive Violations

**1. `closeout_rules` Eviction**
* **File/Line**: `src/digitalmodel/contract_validation/riser_parent_contract.py:214-215`
* **Vulnerability**: The `_compose_assurance` method loops over `assurance.items()` and assigns `result[key] = copy.deepcopy(value)` for any key not in the `skip` set. Because `closeout_rules` (and `invariants`) are missing from `skip`, this unconditionally overwrites `base["closeout_rules"]`.
* **Counterexample**: The assurance component can provide `"closeout_rules": []`, which completely erases the base closeout obligations. This directly violates the `"append_strengthening_rules"` directive.

**2. `milestone_DAG` Guard Wiping**
* **File/Line**: `src/digitalmodel/contract_validation/riser_parent_contract.py:169-176`
* **Vulnerability**: The `_compose_collections` method updates `dag` via `dag.update(...)` for all extension keys other than `edges` and `milestone_owners`. This violates `"append_unique_edges_and_guards"`.
* **Counterexample**: A host component declares `"deterministic_bundle_readiness_guards": {}`. The `.update()` call wipes out the base readiness guards. The downstream validation in `_validate_dag_guards` still passes because an empty dictionary trivially satisfies the `set(references) <= nodes` subset check.

**3. `owners` Hijacking**
* **File/Line**: `src/digitalmodel/contract_validation/riser_parent_contract.py:155`
* **Vulnerability**: The `_merge_owners` method uses dictionary unpacking: `owners[key] = {**owners.get(key, {}), **copy.deepcopy(value)}`. This guarantees that overlapping properties provided by the extension overwrite base properties.
* **Counterexample**: A host component lists a base owner (e.g., `website`) under `owners['amend']` in the manifest and supplies `{"website": {"role": "malicious"}}`. The dictionary unpacking overwrites the original role rather than enforcing `"merge_by_owner_key_and_append_strengthening_amendment"`.

**4. `envelope_commitment_rules` Redefinition**
* **File/Line**: `src/digitalmodel/contract_validation/riser_parent_contract.py:141-144`
* **Vulnerability**: Dictionary unpacking is used to merge commitment rules (`**base, **host`). The directive explicitly demands `"union_new_keys_only"`, but this unpack enables the host to redefine and weaken existing base rules.
* **Counterexample**: The host redefines a base envelope's `commitment_field` to point to a weaker or attacker-controlled binding.

### Test Adequacy & Security Posture
The test suite in `tests/validation/test_riser_parent_contract_adversarial.py` completely lacks regression coverage for these base-preservation invariants. While it tests `assurance` surface limits, it fails to assert that `host` cannot clobber properties like DAG guards and envelope rules, and completely misses that `assurance` clobbers unskipped structures. (Input/Output handling logic correctly detects aliases via `dev`/`ino` and canonical paths, meaning path uniqueness handles symbolic links properly—but this defensive layer protects nothing if the semantic contract constraints are destructively overwritten in memory).

VERDICT: MAJOR
