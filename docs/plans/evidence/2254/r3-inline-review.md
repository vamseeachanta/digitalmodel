# Final inline delta review — 2026-10-09

The Claude r2 packet (SHA-256 80158fa9f61bdd1b212ce6a1123596b141494bff36d23f673be80e98ade67850) was verified unchanged before its findings were acted on. Its CHANGES-REQUIRED verdict applies to the pre-r3 bytes. No Claude approval is claimed for the final commit.

F1 was reproduced: dataclasses.asdict(HydrostaticsResult) failed with TypeError for mappingproxy. Defensive native-dict reads replace mapping proxies, preserving asdict/astuple behavior without exposing retained dictionaries. The mapping regression now asserts unchanged retained V=120 m³ after assignment to the returned copy. Constructor dictionaries are copied; conventions are deep-copied.

F2 was reproduced: vars() exposed array metadata. Frozen slotted ClippedHull, Quantity and HydrostaticsResult remove instance __dict__ storage exposure. Fresh ndarray views still protect shape/dtype metadata; immutable backing protects bytes and every base-chain ndarray. Explicit private/object.__setattr__/native-pointer bypasses remain outside the contract.

Both added regressions failed before the correction. The final integrity suite passes 14 tests, including copy/pickle, dataclass APIs and storage introspection. Codex final inline defect review covers the delta, construction/serialization interfaces, Mapping callers, Quantity dataclass methods and the original 120 → 240 m³ case. No remaining defect is identified within this public-API scope. Per the r3 inline routing rule, another cross-provider dispatch is not performed. Claude shall review the final draft before owner merge.

Gemini review is UNAVAILABLE: the callable CLI rejected its existing OAuth credential with invalid_grant, then awaited interactive retry. No credential/config changes are made. The fallback is the Codex/Claude T2 review, with the r2 findings corrected and reviewed inline.

Generalizable metadata/copy/pickle findings are promoted to https://github.com/vamseeachanta/digitalmodel/issues/2313 for a bounded inventory of other provenance containers. That discovery issue does not authorize unrelated implementation.
