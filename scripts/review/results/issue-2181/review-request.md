Adversarial code review for digitalmodel issue 2181. Default NON-APPROVE. Hunt correctness, source-qualification, offline workflow and API compatibility defects. Return APPROVE/MINOR/MAJOR with concrete findings. Review only the inline packet; no tools/edits/network. Do not repeat scope-expansion requests for colony interaction, combined loading, signed/zero demand, UT mill tolerance or lifetime prediction: this bounded example rejects unsupported inputs explicitly.

Scope: one UT thickness grid through B31G/Modified B31G/RSTRENG/RSTRENG-2D/DNV-F101/circumferential membrane screen; typed Applicability on all six; minimum allowable-to-demand ratio via generic ffs_decision.decide with explicit zero monitor/derate-floor/life bands (ACCEPT >=1; DERATE below1; ESCALATE on flags); any flags force ESCALATE. Factors are caller supplied. Inch/psi contract. Four existing validation links. Registry/example/base-config/engine/harness/catalog additions only. No routes, licensed tables or measured data.

Plan review disposition: R1/R2 Claude MAJOR; Codex inline R3 has addressed real defects: explicit minimum rule, explicit per-method factors including axial design factor, generic shared decide call (not legacy L1/L2), null life, stress-specific derating wording and suppressed pressure rerating field for stress, typed compatibility tests, six numerical anchors/identities from existing records, synthetic provenance. Failed Level-1 extent screen does not invalidate the net-section calculation; only through-wall circumferential loss is flagged. Unsupported above-nominal readings intentionally reject instead of silently clamping; caller width is a separate measurement, not inferred from track count without spacing. MAX projection delegates to the 1D engine; identical results are disclosed, not claimed as independent validation. New constants are arithmetic ratios only; standard constants remain owned by existing engines. The 1334 psi golden belongs to DNV-F101 per consolidated record; issue shorthand is corrected in PR/issue summary. Existing method numeric outputs and signatures are preserved.

The attached full new/modified adapter, method surfaces, examples and tests plus the shared-file delta constitute the review scope. The raw SHA-256 map below pins the complete shared files; the delta excludes unrelated existing rows. Final source readback will verify these hashes alongside the packet manifest before publication. Remaining code-stage gate: Claude, Gemini and Codex review, then exact tests and public bundle review. Claude review before owner merge will remain required on the draft PR.

Code R2 disposition: R1 Claude MAJOR addressed: margin wording is allowable/demand and legacy RSF keys are removed; explicit capacity-screen bands eliminate unsupported REPLACE/MONITOR severity inferences; both breached demands list their distinct allowable limits with axial-specific actions; no derating is emitted on ESCALATE. Output embeds a deep copy of normalized inputs. Report filenames use the validated input stem so examples cannot overwrite each other. Extent-screen result is exposed; Applicability notes align with flags; report date is UTC; UTF-8 readback is explicit. Generic FFSReport footer qualification is partitioned out with a fail-closed structure check, rather than brittle text replacement. Circumferential catalog remains validated deliberately: scoped plan names only the other three catalog entries and combined-loading qualification remains absent. New tests pin the actual reference governing method/verdict and all corrected contracts.

Code R3 inline closure (no third dispatched round): Claude R2 MINOR found no numeric/API defect. Boundary-loss/window assumption is now retained in result limitations and report; projected-profile Applicability docstring and area-weighted compatibility test are explicit; DERATE wording compares allowables; catalog notes bound exercised entry points; missing/mistyped grid fields raise ValueError; report criterion is composed locally without inherited RSF text; factor provenance is caller-supplied; both committed examples have engine-level tests. Repeated aggregate depth flags deliberately retain one result per method; the table identifies each method. UTC dates remain in ignored generated reports. Existing generic FFS header is qualified by the workflow title, limitations and fail-closed footer. Generalizable defects are tracked in https://github.com/vamseeachanta/digitalmodel/issues/2331 and https://github.com/vamseeachanta/digitalmodel/issues/2335. Gemini UNAVAILABLE: fresh CLI credential refresh failed with invalid_grant; no callable Gemini review tool was found. Codex inline final review closes the bounded adapter findings against final frozen sources and tests. Claude still reviews the draft PR before owner merge.

Shared-file SHA-256:
```json
{
  "src/digitalmodel/engine.py": "e731df37d5a183333184eba9c60b890bd8cee30736f92bd1de549b6dd25e54e1",
  "docs/registry/workflows.yaml": "6395a2f87c1ee0c234920f593e5c292ab70e00d8a4ecaad8e72de269c6a05a76",
  "tests/workflows/test_durable_workflows.py": "b2cd66a9d9392c5a6ecfda413196597b6c0c2a333828b227515b4ff97e37d3c8",
  "src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml": "9fd8e6a252a8ce46f370f1ded62306af5b23baa38ef66aa60e6137f08129661e",
  "docs/capability-map/capabilities-added.yml": "69d9fe530575ba2c6f3d27bb50695be133273652554aa8a2ab3ce500e58a0f63"
}
```

Shared-file diff:
```diff
diff --git a/docs/capability-map/capabilities-added.yml b/docs/capability-map/capabilities-added.yml
index 886347c4..35caaa2c 100644
--- a/docs/capability-map/capabilities-added.yml
+++ b/docs/capability-map/capabilities-added.yml
@@ -45,9 +45,9 @@ engines:
   ffs:
     ffs-metal-loss: {status: live, module: asset_integrity.assessment.ffs_coordinator, workflow: ffs-metal-loss, validation: docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md}
     api579-legacy: {status: workflow, module: asset_integrity.API579, workflow: api579-pipe-ffs-b314}
-    corroded-pipe: {status: validated, module: asset_integrity.corroded_pipe, validation: docs/domains/asset-integrity/b31g-validation-2026-06-27.md, issue: 2181}
-    rstreng-2d: {status: validated, module: asset_integrity.rstreng_2d, validation: docs/domains/rstreng-2d-validation-2026-06-29.md, issue: 2181}
-    dnv-f101: {status: validated, module: asset_integrity.dnv_rp_f101, validation: docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md, issue: 2181}
+    corroded-pipe: {status: live, module: asset_integrity.corroded_pipe, workflow: pipeline-corroded-defect-screen, validation: docs/domains/asset-integrity/b31g-validation-2026-06-27.md, issue: 2181}
+    rstreng-2d: {status: live, module: asset_integrity.rstreng_2d, workflow: pipeline-corroded-defect-screen, validation: docs/domains/rstreng-2d-validation-2026-06-29.md, issue: 2181}
+    dnv-f101: {status: live, module: asset_integrity.dnv_rp_f101, workflow: pipeline-corroded-defect-screen, validation: docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md, issue: 2181}
     circumferential: {status: validated, module: asset_integrity.circumferential_defect, validation: docs/domains/circumferential-defect-validation-2026-06-29.md, issue: 2181}
     pitting: {status: engine, module: asset_integrity.assessment.pitting, issue: 2182}
     dents: {status: engine, module: asset_integrity.dent_assessment, issue: 2182}
diff --git a/docs/registry/workflows.yaml b/docs/registry/workflows.yaml
index a38b142c..61e02c21 100644
--- a/docs/registry/workflows.yaml
+++ b/docs/registry/workflows.yaml
@@ -1250,3 +1250,16 @@ workflows:
     result:
       kind: in_memory
       key: mooring_mbl
+
+  - id: pipeline-corroded-defect-screen
+    basename: pipeline_defect_screen
+    title: Six-method pipeline blunt-metal-loss comparison
+    input: examples/workflows/pipeline-corroded-defect-screen/input.yml
+    outputs:
+      - examples/workflows/pipeline-corroded-defect-screen/results/input.yml
+      - examples/workflows/pipeline-corroded-defect-screen/results/input-pipeline-defect-screen.html
+    test: tests/workflows/test_durable_workflows.py::test_workflow_registry[pipeline-corroded-defect-screen]
+    runtime: offline
+    result:
+      kind: in_memory
+      key: pipeline_defect_screen
diff --git a/src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml b/src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml
index 077b7c5e..c2c0a404 100644
--- a/src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml
+++ b/src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml
@@ -85,9 +85,9 @@ codes:
 engines:
   ffs-metal-loss: {module: asset_integrity.assessment.ffs_coordinator, status: live, workflow: ffs-metal-loss, validation: docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md, note: "canonical L1/L2 GML/LML coordinator"}
   api579-legacy: {module: asset_integrity.API579, status: workflow, workflow: api579-pipe-ffs-b314, note: "no validation record; superseded by ffs-metal-loss (#1075), kept as a regression reference"}
-  corroded-pipe: {module: asset_integrity.corroded_pipe, status: validated, validation: docs/domains/asset-integrity/b31g-validation-2026-06-27.md, issue: 2181}
-  rstreng-2d: {module: asset_integrity.rstreng_2d, status: validated, validation: docs/domains/rstreng-2d-validation-2026-06-29.md, issue: 2181}
-  dnv-f101: {module: asset_integrity.dnv_rp_f101, status: validated, validation: docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md, issue: 2181, caveat: "interacting-defect method and PSF tables per #1094"}
+  corroded-pipe: {module: asset_integrity.corroded_pipe, status: live, workflow: pipeline-corroded-defect-screen, validation: docs/domains/asset-integrity/b31g-validation-2026-06-27.md, issue: 2181, note: "workflow covers original/Modified B31G and effective-area pressure methods only"}
+  rstreng-2d: {module: asset_integrity.rstreng_2d, status: live, workflow: pipeline-corroded-defect-screen, validation: docs/domains/rstreng-2d-validation-2026-06-29.md, issue: 2181, note: "workflow covers MAX projection only; area-weighted mode is a sensitivity calculation"}
+  dnv-f101: {module: asset_integrity.dnv_rp_f101, status: live, workflow: pipeline-corroded-defect-screen, validation: docs/domains/asset-integrity/ffs-validation-record-2026-06-27.md, issue: 2181, caveat: "workflow covers single-defect allowable-stress only; interacting-defect method and PSF tables per #1094"}
   circumferential: {module: asset_integrity.circumferential_defect, status: validated, validation: docs/domains/circumferential-defect-validation-2026-06-29.md, issue: 2181}
   pitting: {module: asset_integrity.assessment.pitting, status: engine, issue: 2182, caveat: "Part 6 charts not transcribed; equivalent-LTA bound"}
   dents: {module: asset_integrity.dent_assessment, status: engine, issue: 2182}
diff --git a/src/digitalmodel/engine.py b/src/digitalmodel/engine.py
index 10aa149c..447fde43 100644
--- a/src/digitalmodel/engine.py
+++ b/src/digitalmodel/engine.py
@@ -799,6 +799,10 @@ def engine(
         from digitalmodel.asset_integrity.assessment.ffs_workflow import FFSWorkflow

         cfg_base = FFSWorkflow().router(cfg_base)
+    elif basename == "pipeline_defect_screen":
+        from digitalmodel.asset_integrity.pipeline_defect_screen import router
+
+        cfg_base = router(cfg_base)
     elif basename == "riser_joint_ffs":
         # #1292: drilling-riser joint FFS — Level-1 envelopes, placement, rollup.
         from digitalmodel.asset_integrity.riser_joint_ffs import (
diff --git a/tests/workflows/test_durable_workflows.py b/tests/workflows/test_durable_workflows.py
index bb88c600..c5c98e78 100644
--- a/tests/workflows/test_durable_workflows.py
+++ b/tests/workflows/test_durable_workflows.py
@@ -1624,6 +1624,13 @@ def test_workflow_registry(workflow, monkeypatch):
         assert res["critical_mode"] == 2
         assert res["a_d_ratio"] == pytest.approx(0.9789, abs=1e-3)
         assert res["fatigue_proxy"] > 0.0
+    elif workflow["id"] == "pipeline-corroded-defect-screen":
+        result = cfg["pipeline_defect_screen"]
+        assert len(result["methods"]) == 6
+        assert all("applicability" in row for row in result["methods"])
+        report = (Path(cfg["Analysis"]["result_folder"]) / result["report_file"]).read_text(encoding="utf-8")
+        assert "Method comparison" in report
+        assert all(path in report for path in result["validation_records"])
     else:
         raise AssertionError(f"Missing workflow assertion for {workflow['id']}")


```
