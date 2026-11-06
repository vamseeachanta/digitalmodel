# Corrosion knowledge pack

The corrosion documentation slice records the implemented public API and its
qualification limits. The pack, registry pointer and consistency tests form one
bounded change; runtime behavior and engineering criteria remain unchanged.

## Plan-lite

1. Maintain the ten-section [knowledge pack](../../src/digitalmodel/corrosion/docs/agent-pack.md)
   and its `corrosion` entry in [the routing registry](../registry/module-routing.yaml).
   Cover public definitions, inputs, units, call routes, source locators and
   failure modes. Other packages remain outside this slice.
2. Reuse [the existing AST checker](../../scripts/docs/check_agent_pack_functions.py)
   without modifying it. Compare every public definition with section 2 of the
   pack; use copied synthetic packages to show that added and stale symbols
   fail in the expected direction without changing real sources.
3. Derive the example independently from the synthetic interval calculation in
   section 4 of the pack. Execute the documented code and compare its categories
   with literal expectations from that calculation. These inputs establish
   software classification behavior only.
4. Run `tests/corrosion`, `tests/docs/test_corrosion_agent_pack.py`,
   `tests/docs/test_agent_pack_functions.py` and `tests/test_main_module_routing.py`
   before and after the documentation change. Record collected, passed, failed
   and skipped counts. Check the standalone inventory, every source-locator
   bound, engineering register, repository structure, whitespace and publication
   restrictions. Inspect hosted failures against the available main reference;
   retain unrelated failures as explicit limits rather than changing their tests.

Acceptance requires agreement between the public API inventory and the pack,
rejection of both synthetic drift cases, and exact agreement with the independent
example categories. Default material data, environmental criteria, corrosion
rates and physical applicability remain NOT_EVALUATED. Passing consistency tests
establishes neither engineering acceptance nor source qualification.

Maintenance rule: preserve the independent arithmetic and explicit qualification
limits whenever public definitions or the worked example change.
