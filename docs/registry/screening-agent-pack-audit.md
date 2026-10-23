# Existing agent pack inventory audit

At code pin `3554b14fde67f44b1c52b81b6c72106282eaff29`, all 92 existing packs were examined without changing them. The strict local table checker passes one pack and rejects 91. The supplemental lexical audit reports 7367 code definitions absent from section 2 and 1906 distinct unresolved or out-of-bounds locators. These are documentation-contract findings, not claims about numerical correctness.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-22 | Record pinned legacy inventory and collection limitations for follow-up. |

## Audit contract

Inventory uses `public_inventory` and `check_pack` from `tests/docs/test_screening_agent_packs.py`: direct Python files only; package-owned public functions, classes, methods and properties. For legacy formats, missing names below mean that at least one class/function name component is absent as a token from section 2. Mention elsewhere in a pack does not satisfy that section. The strict checker additionally requires exact qualified table rows and defining ranges. Empty direct-file inventories do not establish coverage of imported or nested APIs. A legacy-format mismatch needs format reconciliation before reverse-direction function drift can be reliably assessed. No claim is made that all listed definitions are undocumented throughout the pack.

Locators are bracketed `path:Lstart-Lend` references; existence and inclusive file bounds are checked against the pinned tree. Historical source moves can therefore be findings without being defects in current code. Repeated identical invalid locators count once per pack. Every function and locator finding is listed below; none was repaired.

## Per-pack function and locator findings

### 1. `src/digitalmodel/alt_fuel_ship_sizing/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 38.

| Function | Direction |
| --- | --- |
| `lh2_fuel_chain.FuelProperties` | code → pack: absent from section 2 |
| `lh2_fuel_chain.lh2_fuel_properties` | code → pack: absent from section 2 |
| `lh2_fuel_chain.nh3_refrigerated_fuel_properties` | code → pack: absent from section 2 |
| `lh2_fuel_chain.methanol_fuel_properties` | code → pack: absent from section 2 |
| `lh2_fuel_chain.fuel_preset` | code → pack: absent from section 2 |
| `lh2_fuel_chain.TankParameters` | code → pack: absent from section 2 |
| `lh2_fuel_chain.TankParameters.heat_flux_w_per_m2` | code → pack: absent from section 2 |
| `lh2_fuel_chain.TankGeometry` | code → pack: absent from section 2 |
| `lh2_fuel_chain.BoilOffResult` | code → pack: absent from section 2 |
| `lh2_fuel_chain.SeaLeg` | code → pack: absent from section 2 |
| `lh2_fuel_chain.SeaLeg.duration_h` | code → pack: absent from section 2 |
| `lh2_fuel_chain.PortLeg` | code → pack: absent from section 2 |
| `lh2_fuel_chain.VoyageLegResult` | code → pack: absent from section 2 |
| `lh2_fuel_chain.VoyageFuelChainResult` | code → pack: absent from section 2 |
| `lh2_fuel_chain.FuelChainResult` | code → pack: absent from section 2 |
| `lh2_fuel_chain.electrical_power_kw` | code → pack: absent from section 2 |
| `lh2_fuel_chain.fuel_mass_flow_kg_per_h` | code → pack: absent from section 2 |
| `lh2_fuel_chain.cylindrical_tank_geometry` | code → pack: absent from section 2 |
| `lh2_fuel_chain.boil_off` | code → pack: absent from section 2 |
| `lh2_fuel_chain.size_fuel_chain_voyage` | code → pack: absent from section 2 |
| `lh2_fuel_chain.size_fuel_chain` | code → pack: absent from section 2 |
| `tonnage.TonnageResult` | code → pack: absent from section 2 |
| `tonnage.gross_tonnage` | code → pack: absent from section 2 |
| `trade_study.WindAssistInputs` | code → pack: absent from section 2 |
| `trade_study.TradeStudyInputs` | code → pack: absent from section 2 |
| `trade_study.shaft_power_at_speed` | code → pack: absent from section 2 |
| `trade_study.run_trade_study` | code → pack: absent from section 2 |
| `wind_assist.ThrustMatrix` | code → pack: absent from section 2 |
| `wind_assist.ThrustMatrix.from_lists` | code → pack: absent from section 2 |
| `wind_assist.WindRoseBin` | code → pack: absent from section 2 |
| `wind_assist.WindAssistBinResult` | code → pack: absent from section 2 |
| `wind_assist.WindAssistResult` | code → pack: absent from section 2 |
| `wind_assist.fold_twa_deg` | code → pack: absent from section 2 |
| `wind_assist.interpolate_thrust` | code → pack: absent from section 2 |
| `wind_assist.wind_assist_saving` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 2. `src/digitalmodel/ansys/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 452.

| Function | Direction |
| --- | --- |
| `_path_compat.is_junction` | code → pack: absent from section 2 |
| `analysis_evidence.parse_digest` | code → pack: absent from section 2 |
| `analysis_evidence.build_package` | code → pack: absent from section 2 |
| `analysis_evidence.validate_package` | code → pack: absent from section 2 |
| `analysis_evidence.publish_package` | code → pack: absent from section 2 |
| `analysis_evidence.load_package` | code → pack: absent from section 2 |
| `analysis_evidence.response_csv` | code → pack: absent from section 2 |
| `analysis_evidence.read_response_csv` | code → pack: absent from section 2 |
| `analysis_intake.resolve_native` | code → pack: absent from section 2 |
| `analysis_intake.code_fingerprint` | code → pack: absent from section 2 |
| `analysis_intake.build_intake_package` | code → pack: absent from section 2 |
| `analysis_intake.main` | code → pack: absent from section 2 |
| `analysis_lookup.create_ledger` | code → pack: absent from section 2 |
| `analysis_lookup.validate_ledger` | code → pack: absent from section 2 |
| `analysis_lookup.append_withdrawal` | code → pack: absent from section 2 |
| `analysis_matrix_publish.publish_matrix` | code → pack: absent from section 2 |
| `analysis_observed.derive_observed_case` | code → pack: absent from section 2 |
| `analysis_observed.validate_observed_transition` | code → pack: absent from section 2 |
| `analysis_observed.build_observed_package` | code → pack: absent from section 2 |
| `analysis_pending.build_pending_package` | code → pack: absent from section 2 |
| `analysis_pending_inputs.pending_inputs` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.document` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.decode_document` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.equal` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.load_inputs` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.set_to_list` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.validate_execution_sources` | code → pack: absent from section 2 |
| `analysis_pressure_inputs.invocation_start` | code → pack: absent from section 2 |
| `analysis_pressure_observed.intake_code_files` | code → pack: absent from section 2 |
| `analysis_pressure_observed.response_units` | code → pack: absent from section 2 |
| `analysis_pressure_observed.validate_pressure_observed_case` | code → pack: absent from section 2 |
| `analysis_pressure_observed.derive_pressure_observed_case` | code → pack: absent from section 2 |
| `analysis_pressure_observed.validate_pressure_observed_transition` | code → pack: absent from section 2 |
| `analysis_pressure_observed.build_pressure_observed_package` | code → pack: absent from section 2 |
| `analysis_pressure_source.source_inventory` | code → pack: absent from section 2 |
| `analysis_pressure_source.validate_recorded_source` | code → pack: absent from section 2 |
| `analysis_pressure_source.verify_build_source` | code → pack: absent from section 2 |
| `analysis_records.canonical_bytes` | code → pack: absent from section 2 |
| `analysis_records.digest_bytes` | code → pack: absent from section 2 |
| `analysis_records.read_json` | code → pack: absent from section 2 |
| `analysis_records.parse_json` | code → pack: absent from section 2 |
| `analysis_records.decimal_text` | code → pack: absent from section 2 |
| `analysis_records.require_fields` | code → pack: absent from section 2 |
| `analysis_records.nonempty` | code → pack: absent from section 2 |
| `analysis_records.safe_id` | code → pack: absent from section 2 |
| `analysis_records.verify_reference` | code → pack: absent from section 2 |
| `analysis_records.validate_response` | code → pack: absent from section 2 |
| `analysis_records.validate_case` | code → pack: absent from section 2 |
| `analysis_records.validate_attempt_state` | code → pack: absent from section 2 |
| `analysis_records.validate_observed_attempt` | code → pack: absent from section 2 |
| `analysis_records.validate_observed_metadata` | code → pack: absent from section 2 |
| `analysis_records.string_list` | code → pack: absent from section 2 |
| `analysis_records.validate_case_evidence` | code → pack: absent from section 2 |
| `analysis_recovery_note.source_inventory` | code → pack: absent from section 2 |
| `analysis_recovery_note.build_recovery_note` | code → pack: absent from section 2 |
| `analysis_recovery_note.validate_recovery_note_transition` | code → pack: absent from section 2 |
| `analysis_replay.canonical_measurement` | code → pack: absent from section 2 |
| `analysis_replay.make_check_receipt` | code → pack: absent from section 2 |
| `analysis_replay.validate_replay_record` | code → pack: absent from section 2 |
| `analysis_replay.derive_replay_case` | code → pack: absent from section 2 |
| `analysis_replay.validate_replay_transition` | code → pack: absent from section 2 |
| `analysis_replay.build_replay_package` | code → pack: absent from section 2 |
| `analysis_replay_inputs.source_inventory` | code → pack: absent from section 2 |
| `analysis_replay_inputs.opaque_reference` | code → pack: absent from section 2 |
| `analysis_replay_inputs.validate_review` | code → pack: absent from section 2 |
| `analysis_replay_inputs.validate_mapping` | code → pack: absent from section 2 |
| `analysis_replay_inputs.verify_original_artifacts` | code → pack: absent from section 2 |
| `analysis_replay_inputs.observed_reference_hash` | code → pack: absent from section 2 |
| `analysis_replay_inputs.load_replay_inputs` | code → pack: absent from section 2 |
| `apdl_generator.MaterialConfig` | code → pack: absent from section 2 |
| `apdl_generator.ElementTypeConfig` | code → pack: absent from section 2 |
| `apdl_generator.MeshConfig` | code → pack: absent from section 2 |
| `apdl_generator.BoundaryCondition` | code → pack: absent from section 2 |
| `apdl_generator.SolutionConfig` | code → pack: absent from section 2 |
| `apdl_generator.PostProcessConfig` | code → pack: absent from section 2 |
| `apdl_generator.APDLScriptConfig` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_header` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_materials` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_element_types` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_mesh_commands` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_boundary_conditions` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_solution` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_postprocess` | code → pack: absent from section 2 |
| `apdl_generator.APDLGenerator.generate_full_script` | code → pack: absent from section 2 |
| `apdl_reader.APDLReader` | code → pack: absent from section 2 |
| `apdl_reader.APDLReader.parse_materials` | code → pack: absent from section 2 |
| `apdl_reader.APDLReader.parse_sections` | code → pack: absent from section 2 |
| `batch_runner.RunStatus` | code → pack: absent from section 2 |
| `batch_runner.LoadCase` | code → pack: absent from section 2 |
| `batch_runner.BatchLoadConfig` | code → pack: absent from section 2 |
| `batch_runner.BatchResultEntry` | code → pack: absent from section 2 |
| `batch_runner.BatchResults` | code → pack: absent from section 2 |
| `batch_runner.ParameterSweep` | code → pack: absent from section 2 |
| `batch_runner.ParameterSweep.from_range` | code → pack: absent from section 2 |
| `batch_runner.ParameterSweep.from_list` | code → pack: absent from section 2 |
| `batch_runner.BatchConfig` | code → pack: absent from section 2 |
| `batch_runner.BatchRun` | code → pack: absent from section 2 |
| `batch_runner.BatchRunner.generate_batch_script` | code → pack: absent from section 2 |
| `batch_runner.BatchRunner.generate_run_summary_csv` | code → pack: absent from section 2 |
| `batch_runner.BatchRunner.parse_load_cases_yaml` | code → pack: absent from section 2 |
| `batch_runner.BatchRunner.generate_batch_scripts_from_load_cases` | code → pack: absent from section 2 |
| `batch_runner.BatchRunner.collect_results_from_outputs` | code → pack: absent from section 2 |
| `batch_runner.BatchRunner.generate_summary_report` | code → pack: absent from section 2 |
| `bolt_analysis.BoltConfig` | code → pack: absent from section 2 |
| `bolt_analysis.BoltCircleConfig` | code → pack: absent from section 2 |
| `bolt_analysis.FlangeConfig` | code → pack: absent from section 2 |
| `bolt_analysis.GasketConfig` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator.generate_bolt_circle_geometry` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator.generate_bolt_pretension` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator.generate_bolt_material` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator.generate_gasket_elements` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator.generate_flange_model` | code → pack: absent from section 2 |
| `bolt_analysis.BoltAnalysisGenerator.generate_multistep_solve` | code → pack: absent from section 2 |
| `contact_analysis.ContactPairConfig` | code → pack: absent from section 2 |
| `contact_analysis.InterferenceFitConfig` | code → pack: absent from section 2 |
| `contact_analysis.SealConfig` | code → pack: absent from section 2 |
| `contact_analysis.ContactResults` | code → pack: absent from section 2 |
| `contact_analysis.ContactAnalysisGenerator` | code → pack: absent from section 2 |
| `contact_analysis.ContactAnalysisGenerator.generate_contact_pair` | code → pack: absent from section 2 |
| `contact_analysis.ContactAnalysisGenerator.generate_interference_fit` | code → pack: absent from section 2 |
| `contact_analysis.ContactAnalysisGenerator.generate_seal_contact` | code → pack: absent from section 2 |
| `contact_analysis.ContactAnalysisGenerator.generate_contact_results_extraction` | code → pack: absent from section 2 |
| `contact_analysis.ContactAnalysisGenerator.generate_frictional_contact_solution` | code → pack: absent from section 2 |
| `cylinder_absence_collection.CollectionError` | code → pack: absent from section 2 |
| `cylinder_absence_collection.PopulationReadError` | code → pack: absent from section 2 |
| `cylinder_absence_collection.collect_absence_snapshot` | code → pack: absent from section 2 |
| `cylinder_absence_population.PopulationError` | code → pack: absent from section 2 |
| `cylinder_absence_population.reconcile_observed_inventories` | code → pack: absent from section 2 |
| `cylinder_active_names.parent_map_evidence` | code → pack: absent from section 2 |
| `cylinder_active_names.ActiveNameResolution` | code → pack: absent from section 2 |
| `cylinder_active_names.ActiveNameResolution.observe` | code → pack: absent from section 2 |
| `cylinder_active_names.ActiveNameResolution.evidence` | code → pack: absent from section 2 |
| `cylinder_adapter.reference_table` | code → pack: absent from section 2 |
| `cylinder_adapter.validate_runtime_profile` | code → pack: absent from section 2 |
| `cylinder_adapter.assessment_for_records` | code → pack: absent from section 2 |
| `cylinder_adapter.extract_record` | code → pack: absent from section 2 |
| `cylinder_adapter.make_execution_adapters` | code → pack: absent from section 2 |
| `cylinder_agreement.compare_reference` | code → pack: absent from section 2 |
| `cylinder_benchmark.frozen_basis` | code → pack: absent from section 2 |
| `cylinder_benchmark.validate_basis` | code → pack: absent from section 2 |
| `cylinder_benchmark.case_definition` | code → pack: absent from section 2 |
| `cylinder_benchmark.validate_case` | code → pack: absent from section 2 |
| `cylinder_benchmark.build_case` | code → pack: absent from section 2 |
| `cylinder_benchmark.validate_deck` | code → pack: absent from section 2 |
| `cylinder_canary.runtime_sources` | code → pack: absent from section 2 |
| `cylinder_canary.retained_bytes` | code → pack: absent from section 2 |
| `cylinder_canary.run_canary` | code → pack: absent from section 2 |
| `cylinder_cfd_owner_evidence.resolve_owner_evidence` | code → pack: absent from section 2 |
| `cylinder_criteria.evaluate_attempt` | code → pack: absent from section 2 |
| `cylinder_criteria.evaluate_canary` | code → pack: absent from section 2 |
| `cylinder_deck.marked` | code → pack: absent from section 2 |
| `cylinder_deck.preparation` | code → pack: absent from section 2 |
| `cylinder_deck.render_deck` | code → pack: absent from section 2 |
| `cylinder_deck_exports.record` | code → pack: absent from section 2 |
| `cylinder_deck_exports.coordinates` | code → pack: absent from section 2 |
| `cylinder_deck_exports.station_export` | code → pack: absent from section 2 |
| `cylinder_deck_exports.independent_listings` | code → pack: absent from section 2 |
| `cylinder_deck_exports.reaction_export` | code → pack: absent from section 2 |
| `cylinder_deck_exports.metadata_export` | code → pack: absent from section 2 |
| `cylinder_deck_exports.postprocessing` | code → pack: absent from section 2 |
| `cylinder_diagnostic_admission.required_source_inventory` | code → pack: absent from section 2 |
| `cylinder_diagnostic_admission.make_diagnostic_admission` | code → pack: absent from section 2 |
| `cylinder_diagnostic_preflight.ProductionPreflight` | code → pack: absent from section 2 |
| `cylinder_diagnostic_preflight.ProductionPreflight.refresh_absence` | code → pack: absent from section 2 |
| `cylinder_diagnostic_preflight.ProductionPreflight.before_launch` | code → pack: absent from section 2 |
| `cylinder_diagnostic_resources.validate_environment` | code → pack: absent from section 2 |
| `cylinder_diagnostic_resources.validate_capacity` | code → pack: absent from section 2 |
| `cylinder_diagnostic_resources.parse_compatible_license` | code → pack: absent from section 2 |
| `cylinder_diagnostic_snapshot.collect_process_snapshot` | code → pack: absent from section 2 |
| `cylinder_diagnostic_snapshot_v2.collect_v2` | code → pack: absent from section 2 |
| `cylinder_expression.ExpressionError` | code → pack: absent from section 2 |
| `cylinder_expression.evaluate_expression` | code → pack: absent from section 2 |
| `cylinder_expression.parse_checker_output` | code → pack: absent from section 2 |
| `cylinder_expression.evaluate_checker` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.PEReader` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.PEReader.unpack` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.PEReader.offset` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.PEReader.span` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.PEReader.entries` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.PEReader.walk` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.parse_forwarder_pe` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.path_identity` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.DeclaredReads` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.DeclaredReads.read` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.DeclaredReads.verify` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.DeclaredReads.evidence` | code → pack: absent from section 2 |
| `cylinder_forwarder_resources.observe_forwarder` | code → pack: absent from section 2 |
| `cylinder_intermediate_lineage.validate_coarse_predecessor` | code → pack: absent from section 2 |
| `cylinder_intermediate_lineage.replay_coarse_predecessor` | code → pack: absent from section 2 |
| `cylinder_intermediate_lineage.coarse_read_pins` | code → pack: absent from section 2 |
| `cylinder_mesh.mapped_mesh` | code → pack: absent from section 2 |
| `cylinder_mesh.sample_stations` | code → pack: absent from section 2 |
| `cylinder_operational_reservation.ReservationError` | code → pack: absent from section 2 |
| `cylinder_operational_reservation.LocalReservation` | code → pack: absent from section 2 |
| `cylinder_operational_reservation.LocalReservation.evidence` | code → pack: absent from section 2 |
| `cylinder_operational_reservation.LocalReservation.release` | code → pack: absent from section 2 |
| `cylinder_operational_reservation.acquire_local_reservation` | code → pack: absent from section 2 |
| `cylinder_parent_identities.capture_parent_identities` | code → pack: absent from section 2 |
| `cylinder_parent_map.ParentMapChangeError` | code → pack: absent from section 2 |
| `cylinder_parent_map.read_windows_parent_map` | code → pack: absent from section 2 |
| `cylinder_parent_map.parent_of` | code → pack: absent from section 2 |
| `cylinder_parent_map.enumerate_windows_v2` | code → pack: absent from section 2 |
| `cylinder_preparation.PreparationError` | code → pack: absent from section 2 |
| `cylinder_preparation.validate_proposal` | code → pack: absent from section 2 |
| `cylinder_preparation.prepare_inputs` | code → pack: absent from section 2 |
| `cylinder_preparation_history.validate_preparation_history` | code → pack: absent from section 2 |
| `cylinder_preparation_streams.expected_stream_paths` | code → pack: absent from section 2 |
| `cylinder_preparation_streams.verify_streams` | code → pack: absent from section 2 |
| `cylinder_pressure_admission.required_source_inventory` | code → pack: absent from section 2 |
| `cylinder_pressure_admission.verify_source_commit` | code → pack: absent from section 2 |
| `cylinder_pressure_admission.make_pressure_admission` | code → pack: absent from section 2 |
| `cylinder_pressure_capture.inventory_capture` | code → pack: absent from section 2 |
| `cylinder_pressure_capture.capture_pressure` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.owned_path` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.write_exclusive` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.InvocationClock` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.InvocationClock.remaining_seconds` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.InvocationClock.record` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.PressureJournal` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.PressureJournal.consumed` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.PressureJournal.start` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.PressureJournal.claim` | code → pack: absent from section 2 |
| `cylinder_pressure_journal.PressureJournal.terminal` | code → pack: absent from section 2 |
| `cylinder_pressure_lineage.validate_lineage` | code → pack: absent from section 2 |
| `cylinder_pressure_resources.validate_observation_ages` | code → pack: absent from section 2 |
| `cylinder_pressure_resources.production_phase2` | code → pack: absent from section 2 |
| `cylinder_pressure_resources.cumulative_storage` | code → pack: absent from section 2 |
| `cylinder_pressure_resume.execute_pressure_resume` | code → pack: absent from section 2 |
| `cylinder_pressure_scope.pressure_step` | code → pack: absent from section 2 |
| `cylinder_process.WindowsSupervisor` | code → pack: absent from section 2 |
| `cylinder_process.WindowsSupervisor.start` | code → pack: absent from section 2 |
| `cylinder_process.WindowsSupervisor.wait` | code → pack: absent from section 2 |
| `cylinder_process.WindowsSupervisor.active` | code → pack: absent from section 2 |
| `cylinder_process.WindowsSupervisor.terminate` | code → pack: absent from section 2 |
| `cylinder_process.WindowsSupervisor.close` | code → pack: absent from section 2 |
| `cylinder_process.PosixSupervisor` | code → pack: absent from section 2 |
| `cylinder_process.PosixSupervisor.start` | code → pack: absent from section 2 |
| `cylinder_process.PosixSupervisor.wait` | code → pack: absent from section 2 |
| `cylinder_process.PosixSupervisor.active` | code → pack: absent from section 2 |
| `cylinder_process.PosixSupervisor.terminate` | code → pack: absent from section 2 |
| `cylinder_process.PosixSupervisor.close` | code → pack: absent from section 2 |
| `cylinder_process.supervisor_factory` | code → pack: absent from section 2 |
| `cylinder_process_inventory.classify_process_inventory` | code → pack: absent from section 2 |
| `cylinder_process_inventory_v2.classify` | code → pack: absent from section 2 |
| `cylinder_process_windows.BasicLimit` | code → pack: absent from section 2 |
| `cylinder_process_windows.IoCounters` | code → pack: absent from section 2 |
| `cylinder_process_windows.ExtendedLimit` | code → pack: absent from section 2 |
| `cylinder_process_windows.Accounting` | code → pack: absent from section 2 |
| `cylinder_process_windows.WindowsJob` | code → pack: absent from section 2 |
| `cylinder_process_windows.WindowsJob.assign` | code → pack: absent from section 2 |
| `cylinder_process_windows.WindowsJob.active` | code → pack: absent from section 2 |
| `cylinder_process_windows.WindowsJob.terminate` | code → pack: absent from section 2 |
| `cylinder_process_windows.WindowsJob.close` | code → pack: absent from section 2 |
| `cylinder_reference.symbols_for` | code → pack: absent from section 2 |
| `cylinder_reference.reference_exact` | code → pack: absent from section 2 |
| `cylinder_reference.reference_text` | code → pack: absent from section 2 |
| `cylinder_reference_provenance.verify_prepared_reference` | code → pack: absent from section 2 |
| `cylinder_refinement_replay.replay_n4_predecessor` | code → pack: absent from section 2 |
| `cylinder_resource_absence.validate_absence_descriptor` | code → pack: absent from section 2 |
| `cylinder_resource_absence.verify_absence_snapshot` | code → pack: absent from section 2 |
| `cylinder_results.EvidenceError` | code → pack: absent from section 2 |
| `cylinder_results.fixed_decimal_context` | code → pack: absent from section 2 |
| `cylinder_results.decimal_context` | code → pack: absent from section 2 |
| `cylinder_results.decimal_value` | code → pack: absent from section 2 |
| `cylinder_results.dimensional_floor` | code → pack: absent from section 2 |
| `cylinder_results.parse_e24` | code → pack: absent from section 2 |
| `cylinder_results.keyed_records` | code → pack: absent from section 2 |
| `cylinder_results.validated_stations` | code → pack: absent from section 2 |
| `cylinder_results.von_mises` | code → pack: absent from section 2 |
| `cylinder_results.parse_station_values` | code → pack: absent from section 2 |
| `cylinder_results.compare_readback` | code → pack: absent from section 2 |
| `cylinder_results.validate_recovery` | code → pack: absent from section 2 |
| `cylinder_results_audit.verify_load_audit` | code → pack: absent from section 2 |
| `cylinder_results_audit.parse_configuration` | code → pack: absent from section 2 |
| `cylinder_results_audit.load_audit_block` | code → pack: absent from section 2 |
| `cylinder_results_audit.split_load_audit` | code → pack: absent from section 2 |
| `cylinder_results_cdb.parse_model_cdb` | code → pack: absent from section 2 |
| `cylinder_results_cdb.verify_model` | code → pack: absent from section 2 |
| `cylinder_results_diagnostics.classify_diagnostics` | code → pack: absent from section 2 |
| `cylinder_results_listings.extract_block` | code → pack: absent from section 2 |
| `cylinder_results_listings.parse_nodal_listing` | code → pack: absent from section 2 |
| `cylinder_results_listings.parse_contribution_listing` | code → pack: absent from section 2 |
| `cylinder_results_native_listings.normalize_nodal` | code → pack: absent from section 2 |
| `cylinder_results_native_listings.normalize_presol` | code → pack: absent from section 2 |
| `cylinder_results_native_loads.verify_zero_loads` | code → pack: absent from section 2 |
| `cylinder_results_native_loads.verify_constraints` | code → pack: absent from section 2 |
| `cylinder_results_native_pressure.verify_pressure_loads` | code → pack: absent from section 2 |
| `cylinder_results_native_reactions.verify_reactions` | code → pack: absent from section 2 |
| `cylinder_results_native_status.lines` | code → pack: absent from section 2 |
| `cylinder_results_native_status.page_header` | code → pack: absent from section 2 |
| `cylinder_results_native_status.config_section` | code → pack: absent from section 2 |
| `cylinder_results_native_status.format_status` | code → pack: absent from section 2 |
| `cylinder_results_native_status.outres_status` | code → pack: absent from section 2 |
| `cylinder_results_native_status.nerr_status` | code → pack: absent from section 2 |
| `cylinder_results_native_status.verify_case_titles` | code → pack: absent from section 2 |
| `cylinder_results_native_values.summary_fields` | code → pack: absent from section 2 |
| `cylinder_results_state.parse_state_values` | code → pack: absent from section 2 |
| `cylinder_results_state.validate_precision_witness` | code → pack: absent from section 2 |
| `cylinder_results_state.parse_support_reactions` | code → pack: absent from section 2 |
| `cylinder_results_validation.validate_native_evidence` | code → pack: absent from section 2 |
| `cylinder_runner.launch_case` | code → pack: absent from section 2 |
| `cylinder_runner.settle_retained` | code → pack: absent from section 2 |
| `cylinder_runner.BinaryANSYSRunner` | code → pack: absent from section 2 |
| `cylinder_runner.BinaryANSYSRunner.launch_case` | code → pack: absent from section 2 |
| `cylinder_runtime_bundle.prepare_runtime_bundle` | code → pack: absent from section 2 |
| `cylinder_wrapper_consoles.observed_console_pids` | code → pack: absent from section 2 |
| `cylinder_wrapper_consoles.console_pids` | code → pack: absent from section 2 |
| `cylinder_wrapper_consoles.historical_projection` | code → pack: absent from section 2 |
| `cylinder_wrapper_consoles.verify_agreement` | code → pack: absent from section 2 |
| `cylinder_wrapper_consoles.resolve_supplement` | code → pack: absent from section 2 |
| `design_points.DesignPointReader` | code → pack: absent from section 2 |
| `design_points.DesignPointReader.read` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatigueLoadCase` | code → pack: absent from section 2 |
| `fatigue_postprocessor.SNcurve` | code → pack: absent from section 2 |
| `fatigue_postprocessor.SNcurve.from_dnv` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatigueConfig` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatigueResult` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor.calculate_cycles_to_failure` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor.calculate_thickness_correction` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor.calculate_fatigue_damage` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor.generate_stress_range_extraction` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor.generate_sn_curve_apdl` | code → pack: absent from section 2 |
| `fatigue_postprocessor.FatiguePostprocessor.create_damage_summary_df` | code → pack: absent from section 2 |
| `models.APDLMaterial` | code → pack: absent from section 2 |
| `models.APDLSection` | code → pack: absent from section 2 |
| `models.DesignPoint` | code → pack: absent from section 2 |
| `models.ParametricStudy` | code → pack: absent from section 2 |
| `models.WBJNJournal` | code → pack: absent from section 2 |
| `mudmat.generate_mudmat_apdl` | code → pack: absent from section 2 |
| `mudmat.write_mudmat_inp` | code → pack: absent from section 2 |
| `padeye.PadeyeGeometry.resolved_center` | code → pack: absent from section 2 |
| `padeye.generate_padeye_apdl` | code → pack: absent from section 2 |
| `padeye.write_padeye_inp` | code → pack: absent from section 2 |
| `padeye_pressure.generate_pressure_preparation` | code → pack: absent from section 2 |
| `padeye_pressure.prepare_pressure_study` | code → pack: absent from section 2 |
| `padeye_pressure_mesh.pressure_geometry` | code → pack: absent from section 2 |
| `padeye_pressure_mesh.build_pressure_mesh` | code → pack: absent from section 2 |
| `pressure_vessel.NozzleConfig` | code → pack: absent from section 2 |
| `pressure_vessel.NozzleLoad` | code → pack: absent from section 2 |
| `pressure_vessel.WindSeismicLoad` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc.ug27_cylindrical_shell` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc.ug32_ellipsoidal_head` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc.ug32_hemispherical_head` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc.calculate_mawp` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc.check_nozzle_reinforcement` | code → pack: absent from section 2 |
| `pressure_vessel.ASMEThicknessCalc.calculate_hydrotest_pressure` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_vessel_geometry` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_internal_pressure` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_thermal_loading` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_nozzle_loads` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_wind_load` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_seismic_load` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_stress_classification_paths` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_pv_apdl` | code → pack: absent from section 2 |
| `pressure_vessel.PressureVesselGenerator.generate_hydrotest_load` | code → pack: absent from section 2 |
| `pressure_vessel_report.vessel_report` | code → pack: absent from section 2 |
| `report_generator.ModelInfo` | code → pack: absent from section 2 |
| `report_generator.MeshMetrics` | code → pack: absent from section 2 |
| `report_generator.BoundaryConditionSummary` | code → pack: absent from section 2 |
| `report_generator.ResultEntry` | code → pack: absent from section 2 |
| `report_generator.ConvergenceInfo` | code → pack: absent from section 2 |
| `report_generator.ReportConfig` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_model_summary` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_mesh_summary` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_boundary_conditions_table` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_results_summary` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_convergence_section` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_load_case_matrix` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_notes_section` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.generate_full_report` | code → pack: absent from section 2 |
| `report_generator.ReportGenerator.results_to_dataframe` | code → pack: absent from section 2 |
| `results_extractor.NodalResult` | code → pack: absent from section 2 |
| `results_extractor.ElementResult` | code → pack: absent from section 2 |
| `results_extractor.ResultSummary` | code → pack: absent from section 2 |
| `results_extractor.ConvergenceRecord` | code → pack: absent from section 2 |
| `results_extractor.StressSummary` | code → pack: absent from section 2 |
| `results_extractor.DisplacementSummary` | code → pack: absent from section 2 |
| `results_extractor.ComparisonRow` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.extract_summary` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.parse_prnsol_block` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.parse_element_table` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.parse_convergence_history` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.extract_max_values` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.extract_stress_summary` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.extract_displacements` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.compare_load_cases` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.parse_result_digest` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.export_results` | code → pack: absent from section 2 |
| `results_extractor.ResultsExtractor.export_comparison` | code → pack: absent from section 2 |
| `runner.ANSYSRunStatus` | code → pack: absent from section 2 |
| `runner.ANSYSRunConfig` | code → pack: absent from section 2 |
| `runner.ANSYSRunner` | code → pack: absent from section 2 |
| `runner.ANSYSRunner.run` | code → pack: absent from section 2 |
| `runner.ANSYSRunner.prepare` | code → pack: absent from section 2 |
| `runner.ANSYSRunner.execute` | code → pack: absent from section 2 |
| `runner.run_ansys` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalMaterialConfig` | code → pack: absent from section 2 |
| `thermal_analysis.ConvectionBC` | code → pack: absent from section 2 |
| `thermal_analysis.RadiationBC` | code → pack: absent from section 2 |
| `thermal_analysis.HeatFluxBC` | code → pack: absent from section 2 |
| `thermal_analysis.TransientConfig` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisConfig` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_thermal_element_type` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_thermal_materials` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_convection_bcs` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_radiation_bcs` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_heat_flux_bcs` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_steady_state_solution` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_transient_solution` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_thermal_results_extraction` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_thermal_structural_coupling` | code → pack: absent from section 2 |
| `thermal_analysis.ThermalAnalysisGenerator.generate_full_thermal_script` | code → pack: absent from section 2 |
| `wbjn_reader.WBJNReader` | code → pack: absent from section 2 |
| `wbjn_reader.WBJNReader.read_metadata` | code → pack: absent from section 2 |
| `weld_assessment.WeldGeometry` | code → pack: absent from section 2 |
| `weld_assessment.HotspotConfig` | code → pack: absent from section 2 |
| `weld_assessment.StressClassificationLine` | code → pack: absent from section 2 |
| `weld_assessment.LinearizedStress` | code → pack: absent from section 2 |
| `weld_assessment.ASMEStressLimits` | code → pack: absent from section 2 |
| `weld_assessment.WeldAssessmentGenerator` | code → pack: absent from section 2 |
| `weld_assessment.WeldAssessmentGenerator.generate_hotspot_stress_extraction` | code → pack: absent from section 2 |
| `weld_assessment.WeldAssessmentGenerator.generate_stress_linearization` | code → pack: absent from section 2 |
| `weld_assessment.WeldAssessmentGenerator.generate_weld_toe_scf` | code → pack: absent from section 2 |
| `weld_assessment.WeldAssessmentGenerator.generate_asme_stress_check` | code → pack: absent from section 2 |
| `weld_assessment.WeldAssessmentGenerator.calculate_linearized_stresses` | code → pack: absent from section 2 |
| `workflow.AnsysWorkflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L270-L300` | pack → code: file missing or range invalid |

### 3. `src/digitalmodel/asset_integrity/assessment/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 68.

| Function | Direction |
| --- | --- |
| `crack_fad.lr_max` | code → pack: absent from section 2 |
| `crack_fad.fad_curve_option1` | code → pack: absent from section 2 |
| `crack_fad.fad_curve_api579_level2` | code → pack: absent from section 2 |
| `crack_fad.newman_raju_k` | code → pack: absent from section 2 |
| `crack_fad.reference_stress_surface_flaw` | code → pack: absent from section 2 |
| `crack_fad.kmat_from_charpy` | code → pack: absent from section 2 |
| `crack_fad.CrackFlawAssessment` | code → pack: absent from section 2 |
| `crack_fad.critical_flaw_depth_mm` | code → pack: absent from section 2 |
| `ffs_coordinator.FFSAssessmentResult.to_dict` | code → pack: absent from section 2 |
| `ffs_coordinator.assess_component` | code → pack: absent from section 2 |
| `ffs_decision.UnitTagError` | code → pack: absent from section 2 |
| `ffs_decision.Verdict` | code → pack: absent from section 2 |
| `ffs_decision.AssetClass` | code → pack: absent from section 2 |
| `ffs_decision.DecisionBands` | code → pack: absent from section 2 |
| `ffs_decision.AssetClassPolicy` | code → pack: absent from section 2 |
| `ffs_decision.action_for` | code → pack: absent from section 2 |
| `ffs_decision.remaining_life` | code → pack: absent from section 2 |
| `ffs_decision.Derating` | code → pack: absent from section 2 |
| `ffs_decision.scaled_limit` | code → pack: absent from section 2 |
| `ffs_decision.reduced_mawp` | code → pack: absent from section 2 |
| `ffs_decision.Decision` | code → pack: absent from section 2 |
| `ffs_decision.Decision.to_dict` | code → pack: absent from section 2 |
| `ffs_decision.from_traffic_light` | code → pack: absent from section 2 |
| `ffs_decision.FFSDecision` | code → pack: absent from section 2 |
| `ffs_decision.FFSDecision.decide` | code → pack: absent from section 2 |
| `ffs_report.FFSReport.generate_screening_html` | code → pack: absent from section 2 |
| `ffs_report.FFSReport.generate_html` | code → pack: absent from section 2 |
| `ffs_router.FFSRouter` | code → pack: absent from section 2 |
| `ffs_router.FFSRouter.classify` | code → pack: absent from section 2 |
| `ffs_workflow.FFSWorkflow` | code → pack: absent from section 2 |
| `ffs_workflow.FFSWorkflow.router` | code → pack: absent from section 2 |
| `grid_parser.GridParser.min_thickness` | code → pack: absent from section 2 |
| `grid_parser.GridParser.max_thickness` | code → pack: absent from section 2 |
| `grid_parser.GridParser.mean_thickness` | code → pack: absent from section 2 |
| `level1_screener.Level1Screener.t_min` | code → pack: absent from section 2 |
| `level2_engine.Level2Engine.local_loss_projection_domain` | code → pack: absent from section 2 |
| `level3_escalation.Level3Handoff` | code → pack: absent from section 2 |
| `level3_escalation.should_escalate_to_level3` | code → pack: absent from section 2 |
| `level3_escalation.escalate_to_level3` | code → pack: absent from section 2 |
| `measurement_sufficiency.SufficiencyAction` | code → pack: absent from section 2 |
| `measurement_sufficiency.SufficiencyResult` | code → pack: absent from section 2 |
| `measurement_sufficiency.MeasurementSufficiency` | code → pack: absent from section 2 |
| `measurement_sufficiency.MeasurementSufficiency.evaluate` | code → pack: absent from section 2 |
| `pitting.PitFieldCharacterization.to_dict` | code → pack: absent from section 2 |
| `pitting.characterize_pit_field` | code → pack: absent from section 2 |
| `pitting.screen_pitting_level1` | code → pack: absent from section 2 |
| `pitting.assess_pitting_level2_equivalent_lta` | code → pack: absent from section 2 |
| `pitting.assess_pitting` | code → pack: absent from section 2 |
| `pitting.router` | code → pack: absent from section 2 |
| `pitting_workflow.router` | code → pack: absent from section 2 |
| `screening_report.finite_number` | code → pack: absent from section 2 |
| `screening_report.generate_screening_html` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml:L1-L53690` | pack → code: file missing or range invalid |

### 4. `src/digitalmodel/asset_integrity/common/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 326.

| Function | Direction |
| --- | --- |
| `API579_components.API579_components.gml` | code → pack: absent from section 2 |
| `API579_components.API579_components.plot_and_save_gml_results` | code → pack: absent from section 2 |
| `API579_components.API579_components.prepare_contour_plot_gml` | code → pack: absent from section 2 |
| `API579_components.API579_components.lml` | code → pack: absent from section 2 |
| `API579_components.API579_components.b31g` | code → pack: absent from section 2 |
| `API579_components.API579_components.read_gml_grids` | code → pack: absent from section 2 |
| `API579_components.API579_components.read_gml_grids_from_inline` | code → pack: absent from section 2 |
| `API579_components.API579_components.read_gml_grids_from_simulations` | code → pack: absent from section 2 |
| `API579_components.API579_components.simulate_wt_grid` | code → pack: absent from section 2 |
| `API579_components.API579_components.read_gml_grids_from_excel` | code → pack: absent from section 2 |
| `API579_components.API579_components.API579GML` | code → pack: absent from section 2 |
| `API579_components.API579_components.GMLAssessmentLengthEvaluation` | code → pack: absent from section 2 |
| `API579_components.API579_components.GMLAcceptability` | code → pack: absent from section 2 |
| `API579_components.API579_components.get_nearest_vector_index` | code → pack: absent from section 2 |
| `API579_components.API579_components.SaveResults` | code → pack: absent from section 2 |
| `API579_components.API579_components.API579LML` | code → pack: absent from section 2 |
| `API579_components.API579_components.LMLAcceptability` | code → pack: absent from section 2 |
| `API579_components.API579_components.getMAWP` | code → pack: absent from section 2 |
| `API579_components.API579_components.LMLMAWPrEvaluation` | code → pack: absent from section 2 |
| `API579_components.API579_components.LMLLevel2Evaluation` | code → pack: absent from section 2 |
| `ApplicationManager.applicationTimer` | code → pack: absent from section 2 |
| `ApplicationManager.setupApplicationRuns` | code → pack: absent from section 2 |
| `ApplicationManager.decoratorWithArgumentExample` | code → pack: absent from section 2 |
| `ApplicationManager.ApplicationRuns` | code → pack: absent from section 2 |
| `ApplicationManager.ApplicationRuns.configureApplication` | code → pack: absent from section 2 |
| `ApplicationManager.ApplicationRuns.getRuns` | code → pack: absent from section 2 |
| `ApplicationManager.ApplicationRuns.addRunRecordsToDB` | code → pack: absent from section 2 |
| `ApplicationManager.ApplicationRuns.getApplicatonRunsFromDB` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.configure` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.unify_application_and_default_and_custom_yamls` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.get_custom_file` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.generateYMLInput` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.get_application_configuration_parameters` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.configure_overwrite_filenames` | code → pack: absent from section 2 |
| `ApplicationManager.ConfigureApplicationInputs.convert_cfg_to_attribute_dictionary` | code → pack: absent from section 2 |
| `ApplicationManager.SaveApplicationResults` | code → pack: absent from section 2 |
| `ApplicationManager.SaveApplicationResults.saveApplicationResultsToDB` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.init_read_stress_intensity_solution_tables` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.update_cfg_with_library_file` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_critical_allowable_flaw` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_minimum_allowable_initial_flaw` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_K_r_allowable` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.iterate_flaw_length_for_unstable_limits` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.iterate_flaw_length_for_unstable_limits_for_single_c_value` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_flaw_fad_properties` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_annexes` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_finite_width_correction_factor` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_by_annex` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_4_1` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_4_3` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_7_2_2` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_7_2_4` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_7_3_2` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_7_3_4` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_solution_Annex_M_7_3_6` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_solution_by_table_data` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.dataframe_manual_interpolation_using_scipy` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.dataframe_manual_interpolation_using_filters` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.perform_flaw_dimension_acceptance_check` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_by_annex` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Annex_P_9_2` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Annex_P_9_4` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Annex_P_10_2` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Annex_P_10_4` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Annex_P_10_6` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_by_equation` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Equation_P_23` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Equation_P_18` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_reference_stress_Equation_P_11` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_stress_intensity_factor_at_discontinuities` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.init_assign_key_properties` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_critical_stress_intensity_factor` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_plasticity_correction` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_phi_value` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_f_theta_value` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.assign_key_flaw_dimensions_for_calculations` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_approximate_flaw_growth_due_to_histograms` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_flaw_growth_due_to_histograms` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_flaw_growth_for_single_flaw_location` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_flaw_growth_from_one_cycle` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_initial_allowable_flaw_for_life` | code → pack: absent from section 2 |
| `BS7910_critical_flaw_limits.BS7910_2013.get_approximate_flaw_growth_solution_for_single_flaw_location` | code → pack: absent from section 2 |
| `DataFrame_To_Image.render_df_table` | code → pack: absent from section 2 |
| `DataFrame_To_Image.DataFrame_To_Image` | code → pack: absent from section 2 |
| `DataFrame_To_doc.DataFrame_To_doc` | code → pack: absent from section 2 |
| `DataFrame_To_xlsx.DataFrame_To_xlsx_xlsxwriter` | code → pack: absent from section 2 |
| `DataFrame_To_xlsx.DataFrame_To_xlsx_openpyxl` | code → pack: absent from section 2 |
| `DataFrame_To_xlsx.DataFrameArray_To_xlsx_openpyxl` | code → pack: absent from section 2 |
| `data.ReadFromExcel` | code → pack: absent from section 2 |
| `data.ReadData` | code → pack: absent from section 2 |
| `data.ReadData.df_filter_by_column_values` | code → pack: absent from section 2 |
| `data.ReadData.from_df_delete_unwanted_columns` | code → pack: absent from section 2 |
| `data.ReadData.from_xlsx` | code → pack: absent from section 2 |
| `data.ReadData.from_xlsx_get_line_number_containing_keyword` | code → pack: absent from section 2 |
| `data.ReadData.xlsx_to_df_by_keyword_search` | code → pack: absent from section 2 |
| `data.ReadData.superseded_xlsx_to_df_by_keyword_search` | code → pack: absent from section 2 |
| `data.ReadData.from_xlsx_get_WorkSheetRowNumberWithText` | code → pack: absent from section 2 |
| `data.ReadData.read_yml_file` | code → pack: absent from section 2 |
| `data.ReadData.extract_from_dictionary` | code → pack: absent from section 2 |
| `data.ReadData.getFromDict` | code → pack: absent from section 2 |
| `data.ReadData.from_pdf` | code → pack: absent from section 2 |
| `data.ReadData.from_pdf_tabula` | code → pack: absent from section 2 |
| `data.ReadData.from_pdf_camelot` | code → pack: absent from section 2 |
| `data.ReadData.update_extension_of_filename` | code → pack: absent from section 2 |
| `data.ReadData.from_ascii_file_get_line_number_containing_keywords` | code → pack: absent from section 2 |
| `data.ReadData.get_array_rows_containing_keywords` | code → pack: absent from section 2 |
| `data.ReadData.from_ascii_file_get_value` | code → pack: absent from section 2 |
| `data.ReadData.from_ascii_file_get_structured_data_delimited_white_space` | code → pack: absent from section 2 |
| `data.ReadData.from_ascii_file_get_lines_as_string_arrays` | code → pack: absent from section 2 |
| `data.ReadData.get_file_list_from_folder` | code → pack: absent from section 2 |
| `data.GetData` | code → pack: absent from section 2 |
| `data.GetData.download_file_from_url` | code → pack: absent from section 2 |
| `data.FromString` | code → pack: absent from section 2 |
| `data.FromString.using_regex` | code → pack: absent from section 2 |
| `data.FromString.convert_fraction_to_float` | code → pack: absent from section 2 |
| `data.FromString.remove_strings` | code → pack: absent from section 2 |
| `data.FromString.remove_string` | code → pack: absent from section 2 |
| `data.FromString.get_value_by_delimiter` | code → pack: absent from section 2 |
| `data.FromString.remove_next_line_values` | code → pack: absent from section 2 |
| `data.SaveData` | code → pack: absent from section 2 |
| `data.SaveData.saveDataJson` | code → pack: absent from section 2 |
| `data.SaveData.saveDataYaml` | code → pack: absent from section 2 |
| `data.SaveData.saveDataFrame` | code → pack: absent from section 2 |
| `data.SaveData.save_ascii_file_from_array` | code → pack: absent from section 2 |
| `data.SaveData.DataFrame_To_xlsx_xlsxwriter` | code → pack: absent from section 2 |
| `data.SaveData.DataFrame_To_xlsx_openpyxl` | code → pack: absent from section 2 |
| `data.SaveData.DataFrameArray_To_xlsx_openpyxl` | code → pack: absent from section 2 |
| `data.SaveData.df_to_sheet_in_existing_workbook` | code → pack: absent from section 2 |
| `data.SaveData.df_to_table_as_image` | code → pack: absent from section 2 |
| `data.SaveData.df_to_table_as_docx` | code → pack: absent from section 2 |
| `data.SaveData.render_df_table` | code → pack: absent from section 2 |
| `data.SaveData.write_ascii_file_from_text` | code → pack: absent from section 2 |
| `data.SaveData.WriteOrcaFlexYMLFile` | code → pack: absent from section 2 |
| `data.DefineData` | code → pack: absent from section 2 |
| `data.DefineData.empty_data_frame` | code → pack: absent from section 2 |
| `data.DefineData.set_value_in_dictionary` | code → pack: absent from section 2 |
| `data.AttributeDict` | code → pack: absent from section 2 |
| `data.objdict` | code → pack: absent from section 2 |
| `data.DateTimeUtility` | code → pack: absent from section 2 |
| `data.DateTimeUtility.last_day_of_month` | code → pack: absent from section 2 |
| `data.Transform` | code → pack: absent from section 2 |
| `data.Transform.numpy_interp` | code → pack: absent from section 2 |
| `data.Transform.gis_deg_to_distance` | code → pack: absent from section 2 |
| `data.Transform.gis_distance_to_deg` | code → pack: absent from section 2 |
| `data.Transform.get_gis_converted_df_superseded` | code → pack: absent from section 2 |
| `data.Transform.dataframe_to_dataframe` | code → pack: absent from section 2 |
| `data.Transform.dataframe_to_dict` | code → pack: absent from section 2 |
| `data.Transform.df_JSON_strings_values_to_dict` | code → pack: absent from section 2 |
| `data.Transform.dataframe_to_json` | code → pack: absent from section 2 |
| `data.Transform.df_transform_repeat_columns_to_unique_columns` | code → pack: absent from section 2 |
| `data.Transform.transform_list_to_unique_list` | code → pack: absent from section 2 |
| `data.Transform.add_column_to_df` | code → pack: absent from section 2 |
| `data.Transform.transpose_df` | code → pack: absent from section 2 |
| `data.Transform.dataframe_to_html` | code → pack: absent from section 2 |
| `data.Transform.convert_numpy_types_to_native_python_types` | code → pack: absent from section 2 |
| `data.TransformData` | code → pack: absent from section 2 |
| `data.TransformData.get_transformed_data` | code → pack: absent from section 2 |
| `data.TransformData.linear` | code → pack: absent from section 2 |
| `data.PandasChainedAssignent` | code → pack: absent from section 2 |
| `data.transform_df_datetime_to_str` | code → pack: absent from section 2 |
| `database.get_db_connection` | code → pack: absent from section 2 |
| `database.get_db_properties_for_service` | code → pack: absent from section 2 |
| `database.test_saving_service_data_to_db` | code → pack: absent from section 2 |
| `database.Database` | code → pack: absent from section 2 |
| `database.Database.init_assign_db_properties` | code → pack: absent from section 2 |
| `database.Database.db_retry_decorator` | code → pack: absent from section 2 |
| `database.Database.get_input_data` | code → pack: absent from section 2 |
| `database.Database.set_up_db_connection` | code → pack: absent from section 2 |
| `database.Database.enable_connection_and_cursor` | code → pack: absent from section 2 |
| `database.Database.perform_analysis` | code → pack: absent from section 2 |
| `database.Database.get_db_basic_analysis_outputs` | code → pack: absent from section 2 |
| `database.Database.get_df_from_stored_procedure` | code → pack: absent from section 2 |
| `database.Database.get_df_from_query` | code → pack: absent from section 2 |
| `database.Database.get_db_table_analysis_outputs` | code → pack: absent from section 2 |
| `database.Database.get_db_version` | code → pack: absent from section 2 |
| `database.Database.get_db_tables` | code → pack: absent from section 2 |
| `database.Database.get_db_functions` | code → pack: absent from section 2 |
| `database.Database.get_columns_for_all_db_tables` | code → pack: absent from section 2 |
| `database.Database.get_table_basic_statistics` | code → pack: absent from section 2 |
| `database.Database.get_schema_statistics_class` | code → pack: absent from section 2 |
| `database.Database.get_table_statistics` | code → pack: absent from section 2 |
| `database.Database.get_table_timetrace_statistics` | code → pack: absent from section 2 |
| `database.Database.get_timeline_statistics` | code → pack: absent from section 2 |
| `database.Database.get_table_statistics_from_db` | code → pack: absent from section 2 |
| `database.Database.save_application_data` | code → pack: absent from section 2 |
| `database.Database.save_to_db` | code → pack: absent from section 2 |
| `database.Database.get_table_columns` | code → pack: absent from section 2 |
| `database.Database.get_table_column_statistics` | code → pack: absent from section 2 |
| `database.Database.get_table_column_time_traces` | code → pack: absent from section 2 |
| `database.Database.executeScriptsFromFile` | code → pack: absent from section 2 |
| `database.Database.executeQueryWithParameters` | code → pack: absent from section 2 |
| `database.Database.executeNoDataQuery` | code → pack: absent from section 2 |
| `database.Database.get_input_data_from_db` | code → pack: absent from section 2 |
| `database.Database.get_input_data_set` | code → pack: absent from section 2 |
| `database.Database.perform_extreme_event_analysis` | code → pack: absent from section 2 |
| `database.Database.prepare_input_statistics` | code → pack: absent from section 2 |
| `database.Database.format_table_statistics_df` | code → pack: absent from section 2 |
| `database.Database.format_table_statistics_df_for_extreme_events` | code → pack: absent from section 2 |
| `database.Database.get_create_table_sql_code_from_df` | code → pack: absent from section 2 |
| `fad.FAD.get_BS7910_2013_FAD` | code → pack: absent from section 2 |
| `fad.FAD.BS7910_2013_option_1` | code → pack: absent from section 2 |
| `fad.FAD.BS7910_2013_option_2` | code → pack: absent from section 2 |
| `fad.FAD.BS7910_2013_option_3` | code → pack: absent from section 2 |
| `fad.FAD.get_plastic_collapse_load_ratio_limit` | code → pack: absent from section 2 |
| `fad.FAD.init_assign_key_properties` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.evaluate_FAD` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.critical_flaw_limits` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.get_flaw_growth_for_fatigue_loading` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.get_initial_allowable_flaw_for_life` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.get_histogram_loading` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.update_cfg_with_library_file` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.save_result_tables` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_visualizations` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_fatigue_assessment_diagrams` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_unstable_fracture_limits` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_minimum_allowable_flaws` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_flaw_growth_rates` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_min_init_flaw_by_component` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_min_init_flaw_by_service_life` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_min_init_flaw_by_flaw_location` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_min_init_flaw_by_flaw_orientation` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_flaw_growth_by_component` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_flaw_growth_by_service_life` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_flaw_growth_by_flaw_location` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.create_viz_flaw_growth_by_flaw_orientation` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.init_add_pipe_properties_to_cfg` | code → pack: absent from section 2 |
| `fracture_mechanics_components.FractureMechanicsComponents.make_initial_flaw_monotonous` | code → pack: absent from section 2 |
| `math_solvers.Polynomial.solve_by_coefficients` | code → pack: absent from section 2 |
| `math_solvers.Polynomial.solve_using_sympy` | code → pack: absent from section 2 |
| `math_solvers.Scipy_Interpolation` | code → pack: absent from section 2 |
| `math_solvers.Scipy_Interpolation.solve_y_for_X` | code → pack: absent from section 2 |
| `math_solvers.FFT_Methods` | code → pack: absent from section 2 |
| `math_solvers.FFT_Methods.numpy_fft` | code → pack: absent from section 2 |
| `math_solvers.Geometry` | code → pack: absent from section 2 |
| `math_solvers.Geometry.max_distance_for_spatial_sets` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents.get_number_of_workers` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents.write` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents.example_1` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents.example_2` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents.calculation` | code → pack: absent from section 2 |
| `parallel_process_components.ParallelProcessComponents.available_cpu_count` | code → pack: absent from section 2 |
| `pipe_components.PipeComponents` | code → pack: absent from section 2 |
| `pipe_components.PipeComponents.add_custom_folder` | code → pack: absent from section 2 |
| `pipe_components.PipeComponents.get_environments` | code → pack: absent from section 2 |
| `pipe_components.PipeComponents.set_up_db_connection` | code → pack: absent from section 2 |
| `pipe_components.PipeComponents.get_material_data` | code → pack: absent from section 2 |
| `pipe_components.PipeComponents.evaluate_pipe_capacity` | code → pack: absent from section 2 |
| `saveData.saveDataJson` | code → pack: absent from section 2 |
| `saveData.OrderedDumper` | code → pack: absent from section 2 |
| `saveData.dump_ordered_yaml` | code → pack: absent from section 2 |
| `saveData.dump_ordered` | code → pack: absent from section 2 |
| `saveData.saveDataYaml` | code → pack: absent from section 2 |
| `saveData.saveDataFrame` | code → pack: absent from section 2 |
| `saveData.setup_yaml` | code → pack: absent from section 2 |
| `set_logging.set_logging` | code → pack: absent from section 2 |
| `update_deep.AttributeDict` | code → pack: absent from section 2 |
| `update_deep.update_deep_dictionary` | code → pack: absent from section 2 |
| `utilities.get_valid_file_name` | code → pack: absent from section 2 |
| `utilities.get_module_path` | code → pack: absent from section 2 |
| `utilities.is_file_valid_func` | code → pack: absent from section 2 |
| `utilities.add_cwd_to_filename` | code → pack: absent from section 2 |
| `utilities.get_common_name_from_2_filenames` | code → pack: absent from section 2 |
| `utilities.get_colors` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.get_raw_data` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.get_input_data` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.get_environments` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.set_up_db_connection` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.save_raw_data` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.prepare_visualizations` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.prepare_single` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.prepare_plot_input_data` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.get_filtered_df` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.get_scaled_df` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.prepare_multiple` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.prepare_a_multiple_plot` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.plot_a_data_set` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.plotting_for_no_data_object` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.add_fft_peaks_notes` | code → pack: absent from section 2 |
| `visualization_components.VisualizationComponents.run_example` | code → pack: absent from section 2 |
| `visualizations.Visualization` | code → pack: absent from section 2 |
| `visualizations.Visualization.generate_time_line` | code → pack: absent from section 2 |
| `visualizations.Visualization.from_df_array` | code → pack: absent from section 2 |
| `visualizations.Visualization.from_df_columns` | code → pack: absent from section 2 |
| `visualizations.Visualization.set_plot_size_orientation` | code → pack: absent from section 2 |
| `visualizations.Visualization.from_df_get_plot` | code → pack: absent from section 2 |
| `visualizations.Visualization.add_x_y_scale_formats` | code → pack: absent from section 2 |
| `visualizations.Visualization.add_x_y_lim_formats` | code → pack: absent from section 2 |
| `visualizations.Visualization.resolution_adjust` | code → pack: absent from section 2 |
| `visualizations.Visualization.save_and_close` | code → pack: absent from section 2 |
| `visualizations.Visualization.gist1` | code → pack: absent from section 2 |
| `visualizations.Visualization.add_title_and_axis_labels` | code → pack: absent from section 2 |
| `visualizations.Visualization.add_legend` | code → pack: absent from section 2 |
| `visualizations.Visualization.axis_autoformat` | code → pack: absent from section 2 |
| `visualizations.Visualization.add_text_fields` | code → pack: absent from section 2 |
| `visualizations.Visualization.annotate` | code → pack: absent from section 2 |
| `visualizations.Visualization.add_reference_lines_and_spans` | code → pack: absent from section 2 |
| `visualizations.Visualization.multiple_plots` | code → pack: absent from section 2 |
| `visualizations.Visualization.autoselect_current_subplot` | code → pack: absent from section 2 |
| `visualizations.Visualization.plot_subplot` | code → pack: absent from section 2 |
| `visualizations.Visualization.color_lists` | code → pack: absent from section 2 |
| `visualizations.Visualization.orcaflex_range_plot` | code → pack: absent from section 2 |
| `visualizations.Visualization.example_group_bar_plot1_gist` | code → pack: absent from section 2 |
| `visualizations.Visualization.plotCustomFatigue` | code → pack: absent from section 2 |
| `visualizations.Visualization.plotCustomFatigueComparison` | code → pack: absent from section 2 |
| `ymlInput.update_deep` | code → pack: absent from section 2 |
| `yml_utilities.update_deep` | code → pack: absent from section 2 |
| `yml_utilities.yml_read_stream` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.analyze_yaml_keys` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.compare_yaml_root_keys` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.compare_yaml_files_deepdiff` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.compare_yaml_file_contents_deepdiff` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.save_diff_files` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.get_library_yaml_file` | code → pack: absent from section 2 |
| `yml_utilities.WorkingWithYAML.get_library_filename` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 5. `src/digitalmodel/asset_integrity/custom/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 64.

| Function | Direction |
| --- | --- |
| `MaterialProperties.MaterialProperties.evaluate_material_properties` | code → pack: absent from section 2 |
| `MaterialProperties.MaterialProperties.set_up_temperature_derating` | code → pack: absent from section 2 |
| `MaterialProperties.MaterialProperties.temperatureDerating` | code → pack: absent from section 2 |
| `MaterialProperties.MaterialProperties.get_material_properties` | code → pack: absent from section 2 |
| `MaterialProperties.MaterialProperties.evaluate_mass_and_weight_properties` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_pipe_wall` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_load_conditions` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.internal_pressure` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.collapse_propagation` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.external_pressure` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.update_pressure_results` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.internal_pressure_specification_code_based_evaluation` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_burst_pressure_API_STD_2RD` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_burst_pressure_API_RP_1111` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_burst_minimum_thickness_API_RP_1111` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_burst_minimum_thickness_API_STD_2RD` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_burst_pressure_modified_burlow_equation` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_burst_minimum_thickness_modified_burlow_equation` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.external_pressure_specification_code_based_evaluation` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_collapse_pressure_API_STD_2RD` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_collapse_minimum_thickness_API_STD_2RD` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.collapse_propagation_specification_code_based_evaluation` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_collapse_propagation_pressure_API_RP_1111` | code → pack: absent from section 2 |
| `PipeCapacity.PipeCapacity.evaluate_collapse_propagation_minimum_thickness_API_RP_1111` | code → pack: absent from section 2 |
| `PipeCapacity.InternalPressureMethods` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated.ASMEB314InternalPressure` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated.ASMEB314LogitudinalStress` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated.ASMEB314EquivalentStress` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated.ASMEB318InternalPressure` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated.ASMEB318LogitudinalStress` | code → pack: absent from section 2 |
| `PipeCapacity.OtherMethodsTobeIncorporated.ASMEB318EquivalentStress` | code → pack: absent from section 2 |
| `PipeCapacity.API_RP_2RD` | code → pack: absent from section 2 |
| `PipeCapacity.API_RP_16Q` | code → pack: absent from section 2 |
| `PipeCapacity.API_RP_16Q.get_burst_minimum_thickness` | code → pack: absent from section 2 |
| `PipeCapacity.API_RP_16Q.get_burst_pressure` | code → pack: absent from section 2 |
| `PipeCapacity.API_TR_5C3` | code → pack: absent from section 2 |
| `PipeCapacity.API_TR_5C3.init_calculate_required_properties` | code → pack: absent from section 2 |
| `PipeCapacity.API_TR_5C3.get_collapse_pressure` | code → pack: absent from section 2 |
| `PipeCapacity.API_TR_5C3.get_collapse_minimum_thickness` | code → pack: absent from section 2 |
| `PipeCapacity.VonMises_Pipe` | code → pack: absent from section 2 |
| `PipeCapacity.VonMises_Pipe.init_calculate_required_properties` | code → pack: absent from section 2 |
| `PipeCapacity.VonMises_Pipe.get_stress` | code → pack: absent from section 2 |
| `PipeCapacity.VonMises_Pipe.get_burst_minimum_thickness` | code → pack: absent from section 2 |
| `PipeCapacity.VonMises_Pipe.get_burst_pressure` | code → pack: absent from section 2 |
| `PipeCapacity.CFR_30_Part_250` | code → pack: absent from section 2 |
| `PipeCapacity.CFR_30_Part_250.get_burst_minimum_thickness` | code → pack: absent from section 2 |
| `PipeCapacity.CFR_30_Part_250.get_burst_pressure` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.evaluate_pipe_system_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.pipe_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.pipe_section_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.get_fea_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.system_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.pipeProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.steelSectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.insulationSectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.buoyancySectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.sectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.buoyancyFactorAndDiameter` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 6. `src/digitalmodel/asset_integrity/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 173.

| Function | Direction |
| --- | --- |
| `API579.API579` | code → pack: absent from section 2 |
| `applicability.Applicability` | code → pack: absent from section 2 |
| `applicability.Applicability.to_dict` | code → pack: absent from section 2 |
| `applicability.Applicability.note` | code → pack: absent from section 2 |
| `applicability.Applicability.legacy_details` | code → pack: absent from section 2 |
| `applicability.flagged` | code → pack: absent from section 2 |
| `applicability.check_upper_limit` | code → pack: absent from section 2 |
| `applicability.merge` | code → pack: absent from section 2 |
| `applicability.from_details` | code → pack: absent from section 2 |
| `applicability.collect` | code → pack: absent from section 2 |
| `campaign_comparability.t_quantile_95` | code → pack: absent from section 2 |
| `campaign_comparability.GaugeEstimate` | code → pack: absent from section 2 |
| `campaign_comparability.GaugeEstimate.mean_consistent_with_zero` | code → pack: absent from section 2 |
| `campaign_comparability.gauge_from_short_interval` | code → pack: absent from section 2 |
| `campaign_comparability.InvarianceRow` | code → pack: absent from section 2 |
| `campaign_comparability.invariance_family` | code → pack: absent from section 2 |
| `campaign_comparability.reference_set_size` | code → pack: absent from section 2 |
| `campaign_comparability.minimum_detectable_rate` | code → pack: absent from section 2 |
| `campaign_comparability.VarianceBudget` | code → pack: absent from section 2 |
| `campaign_comparability.variance_budget` | code → pack: absent from section 2 |
| `circumferential_defect.Part5LongitudinalRSFResult` | code → pack: absent from section 2 |
| `circumferential_defect.NetSectionCircumferentialResult` | code → pack: absent from section 2 |
| `circumferential_defect.part5_folias_mt` | code → pack: absent from section 2 |
| `circumferential_defect.part5_shell_parameter` | code → pack: absent from section 2 |
| `circumferential_defect.api579_part5_longitudinal_rsf` | code → pack: absent from section 2 |
| `circumferential_defect.api579_part5_circumferential_netsection` | code → pack: absent from section 2 |
| `circumferential_defect.dnv_f101_combined_loading` | code → pack: absent from section 2 |
| `composite_repair.CompositeRepairParameters` | code → pack: absent from section 2 |
| `composite_repair.CompositeRepairParameters.basis_notes` | code → pack: absent from section 2 |
| `composite_repair.RepairContext` | code → pack: absent from section 2 |
| `composite_repair.RepairContext.from_mapping` | code → pack: absent from section 2 |
| `composite_repair.CompositeRepairRecommendation` | code → pack: absent from section 2 |
| `composite_repair.CompositeRepairRecommendation.to_dict` | code → pack: absent from section 2 |
| `composite_repair.recommend_from_ffs_result` | code → pack: absent from section 2 |
| `composite_repair.recommend_composite_repair` | code → pack: absent from section 2 |
| `corroded_pipe.folias_original` | code → pack: absent from section 2 |
| `corroded_pipe.folias_modified` | code → pack: absent from section 2 |
| `corroded_pipe.safety_factor_for_location_class` | code → pack: absent from section 2 |
| `corroded_pipe.b31g_original` | code → pack: absent from section 2 |
| `corroded_pipe.modified_b31g` | code → pack: absent from section 2 |
| `corroded_pipe.rstreng_effective_area` | code → pack: absent from section 2 |
| `corroded_pipe.allowable_flaw_length` | code → pack: absent from section 2 |
| `corroded_pipe.b31g_original_allowable_length` | code → pack: absent from section 2 |
| `dent_assessment.DentStrain` | code → pack: absent from section 2 |
| `dent_assessment.DentAssessment` | code → pack: absent from section 2 |
| `dent_assessment.dent_strain_b31_8` | code → pack: absent from section 2 |
| `dent_assessment.assess_dent` | code → pack: absent from section 2 |
| `dent_assessment.router` | code → pack: absent from section 2 |
| `dent_workflow.router` | code → pack: absent from section 2 |
| `dnv_rp_f101.gamma_m_factor` | code → pack: absent from section 2 |
| `dnv_rp_f101.epsilon_d_fractile` | code → pack: absent from section 2 |
| `dnv_rp_f101.gamma_d_factor` | code → pack: absent from section 2 |
| `dnv_rp_f101.length_correction_factor` | code → pack: absent from section 2 |
| `dnv_rp_f101.dnv_f101_single_defect` | code → pack: absent from section 2 |
| `dnv_rp_f101.dnv_f101_psf` | code → pack: absent from section 2 |
| `dnv_rp_f101.interaction_spacing_limit` | code → pack: absent from section 2 |
| `dnv_rp_f101.dnv_f101_interacting` | code → pack: absent from section 2 |
| `engine.engine` | code → pack: absent from section 2 |
| `engine.validate_arguments_run_methods` | code → pack: absent from section 2 |
| `engine.save_cfg` | code → pack: absent from section 2 |
| `ffs_acceptance_curves.pipe_acceptance_curve` | code → pack: absent from section 2 |
| `ffs_acceptance_curves.pipe_pressure_wall_screen` | code → pack: absent from section 2 |
| `ffs_acceptance_curves.plate_acceptance_curve` | code → pack: absent from section 2 |
| `ffs_acceptance_curves.general_metal_loss_curves` | code → pack: absent from section 2 |
| `ffs_lookup.signature` | code → pack: absent from section 2 |
| `ffs_lookup.barlow_maop_psi` | code → pack: absent from section 2 |
| `ffs_lookup.evaluate_pipe_corroded` | code → pack: absent from section 2 |
| `ffs_lookup.evaluate_plate_metal_loss` | code → pack: absent from section 2 |
| `ffs_lookup.evaluate_query` | code → pack: absent from section 2 |
| `ffs_lookup.record_from_assessment` | code → pack: absent from section 2 |
| `ffs_lookup.build_lookup` | code → pack: absent from section 2 |
| `ffs_lookup.query_index` | code → pack: absent from section 2 |
| `ffs_lookup.write_lookup` | code → pack: absent from section 2 |
| `fracture_mechanics.fracture_mechanics` | code → pack: absent from section 2 |
| `hull_wastage_screen.SurveyScope` | code → pack: absent from section 2 |
| `hull_wastage_screen.HullPlate` | code → pack: absent from section 2 |
| `hull_wastage_screen.WastageResult` | code → pack: absent from section 2 |
| `hull_wastage_screen.screen_local_corrosion` | code → pack: absent from section 2 |
| `hull_wastage_screen.screen_global_corrosion` | code → pack: absent from section 2 |
| `inspection_planning.InspectionPlan` | code → pack: absent from section 2 |
| `inspection_planning.corrosion_rate_mm_yr` | code → pack: absent from section 2 |
| `inspection_planning.plan_inspection` | code → pack: absent from section 2 |
| `inspection_planning.router` | code → pack: absent from section 2 |
| `mooring_chain_dimensions.DiameterReading` | code → pack: absent from section 2 |
| `mooring_chain_dimensions.VisualInspection` | code → pack: absent from section 2 |
| `mooring_chain_dimensions.ChainInspection` | code → pack: absent from section 2 |
| `mooring_chain_dimensions.DimensionalResult` | code → pack: absent from section 2 |
| `mooring_chain_dimensions.ChainDimensionalScreen` | code → pack: absent from section 2 |
| `mooring_chain_dimensions.screen_dimensions` | code → pack: absent from section 2 |
| `offering_catalog.CatalogError` | code → pack: absent from section 2 |
| `offering_catalog.Row` | code → pack: absent from section 2 |
| `offering_catalog.Row.where` | code → pack: absent from section 2 |
| `offering_catalog.Catalog` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.rows` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.design_rows` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.engines` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.section_number` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.by_industry` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.by_code` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.by_status` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.gaps` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.summary` | code → pack: absent from section 2 |
| `offering_catalog.Catalog.mechanisms_for_part` | code → pack: absent from section 2 |
| `offering_catalog.load` | code → pack: absent from section 2 |
| `offering_catalog.load_registry_ids` | code → pack: absent from section 2 |
| `offering_catalog.check_engines` | code → pack: absent from section 2 |
| `offering_catalog.check_rows` | code → pack: absent from section 2 |
| `offering_catalog.check_status_semantics` | code → pack: absent from section 2 |
| `offering_catalog.check_no_licensed_thresholds` | code → pack: absent from section 2 |
| `offering_catalog.check_design` | code → pack: absent from section 2 |
| `offering_catalog.check_crosswalk` | code → pack: absent from section 2 |
| `offering_catalog.validate` | code → pack: absent from section 2 |
| `offering_catalog.asset_column` | code → pack: absent from section 2 |
| `offering_catalog.render_markdown` | code → pack: absent from section 2 |
| `offering_catalog.capability_map_entries` | code → pack: absent from section 2 |
| `offering_catalog.render_capability_map` | code → pack: absent from section 2 |
| `offering_catalog.check` | code → pack: absent from section 2 |
| `offering_catalog.main` | code → pack: absent from section 2 |
| `pipeline_defect_screen.assess` | code → pack: absent from section 2 |
| `pipeline_defect_screen.render_report` | code → pack: absent from section 2 |
| `pipeline_defect_screen.router` | code → pack: absent from section 2 |
| `pipeline_defect_screen_inputs.validate` | code → pack: absent from section 2 |
| `pipeline_skill.PipelineInput` | code → pack: absent from section 2 |
| `pipeline_skill.PipelineIntegrityResult` | code → pack: absent from section 2 |
| `pipeline_skill.pipeline_integrity` | code → pack: absent from section 2 |
| `rbi_screening.RBIParameters` | code → pack: absent from section 2 |
| `rbi_screening.RBIParameters.basis_notes` | code → pack: absent from section 2 |
| `rbi_screening.RBIScreeningResult` | code → pack: absent from section 2 |
| `rbi_screening.RBIScreeningResult.to_dict` | code → pack: absent from section 2 |
| `rbi_screening.cof_category` | code → pack: absent from section 2 |
| `rbi_screening.pof_category` | code → pack: absent from section 2 |
| `rbi_screening.risk_band` | code → pack: absent from section 2 |
| `rbi_screening.recommend_interval` | code → pack: absent from section 2 |
| `rbi_screening.screen_component` | code → pack: absent from section 2 |
| `rbi_screening.screen_ffs_result` | code → pack: absent from section 2 |
| `rbi_screening.rbi_rank_register` | code → pack: absent from section 2 |
| `rbi_screening.summarize_ranking` | code → pack: absent from section 2 |
| `riser_joint_ffs.level1_flaw_envelope` | code → pack: absent from section 2 |
| `riser_joint_ffs.collapse_pressure_psi` | code → pack: absent from section 2 |
| `riser_joint_ffs.acceptable_water_depth_ft` | code → pack: absent from section 2 |
| `riser_joint_ffs.JointPlacement` | code → pack: absent from section 2 |
| `riser_joint_ffs.place_joint` | code → pack: absent from section 2 |
| `riser_joint_ffs.fleet_rollup` | code → pack: absent from section 2 |
| `riser_joint_ffs.RiserJointFFSWorkflow` | code → pack: absent from section 2 |
| `riser_joint_ffs.RiserJointFFSWorkflow.router` | code → pack: absent from section 2 |
| `riser_joint_report.RiserJointReport` | code → pack: absent from section 2 |
| `riser_joint_report.RiserJointReport.generate` | code → pack: absent from section 2 |
| `riser_joint_report.run_report` | code → pack: absent from section 2 |
| `rsf_calculations.calculate_rsf` | code → pack: absent from section 2 |
| `rsf_calculations.check_ffs_level1` | code → pack: absent from section 2 |
| `rsf_calculations.remaining_life` | code → pack: absent from section 2 |
| `rsf_calculations.process_ut_grid` | code → pack: absent from section 2 |
| `rstreng_2d.RiverBottom2DResult` | code → pack: absent from section 2 |
| `rstreng_2d.RiverBottom2DResult.applicability` | code → pack: absent from section 2 |
| `rstreng_2d.RiverBottom2DResult.failure_pressure_psi` | code → pack: absent from section 2 |
| `rstreng_2d.RiverBottom2DResult.intact_pressure_psi` | code → pack: absent from section 2 |
| `rstreng_2d.RiverBottom2DResult.safe_pressure_psi` | code → pack: absent from section 2 |
| `rstreng_2d.RiverBottom2DResult.rsf` | code → pack: absent from section 2 |
| `rstreng_2d.project_grid` | code → pack: absent from section 2 |
| `rstreng_2d.rstreng_2d_river_bottom` | code → pack: absent from section 2 |
| `shell_distortion_screen.PipingContext` | code → pack: absent from section 2 |
| `shell_distortion_screen.GeometryScreenResult` | code → pack: absent from section 2 |
| `shell_distortion_screen.assess_piping_ovality` | code → pack: absent from section 2 |
| `shell_distortion_screen.assess_angular_misalignment` | code → pack: absent from section 2 |
| `uniform_loss_diagnostic.diagnose_case` | code → pack: absent from section 2 |
| `uniform_loss_diagnostic.run_study` | code → pack: absent from section 2 |
| `uniform_loss_diagnostic.run_preliminary_pressure_study` | code → pack: absent from section 2 |
| `uniform_loss_diagnostic.plot_preliminary_pressure_study` | code → pack: absent from section 2 |
| `uniform_loss_diagnostic.main` | code → pack: absent from section 2 |
| `uniform_loss_envelope.prepare_envelope_cell` | code → pack: absent from section 2 |
| `uniform_loss_envelope.prepare_envelope_study` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 7. `src/digitalmodel/cathodic_protection/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 368.

| Function | Direction |
| --- | --- |
| `_edition.normalize_edition` | code → pack: absent from section 2 |
| `_edition.standard_for_edition` | code → pack: absent from section 2 |
| `_edition.normalize_f103_edition` | code → pack: absent from section 2 |
| `_edition.f103_standard_for_edition` | code → pack: absent from section 2 |
| `_evidence.iccp_evidence` | code → pack: absent from section 2 |
| `_experimental.ExperimentalModelError` | code → pack: absent from section 2 |
| `_experimental.require_experimental` | code → pack: absent from section 2 |
| `_kernels.current_demand` | code → pack: absent from section 2 |
| `_kernels.anode_mass` | code → pack: absent from section 2 |
| `_kernels.anode_mass_from_current_years` | code → pack: absent from section 2 |
| `_kernels.mass_consumed` | code → pack: absent from section 2 |
| `_kernels.coating_breakdown_linear` | code → pack: absent from section 2 |
| `_kernels.coating_breakdown_mean` | code → pack: absent from section 2 |
| `_kernels.coating_breakdown_final` | code → pack: absent from section 2 |
| `_kernels.anode_current_output` | code → pack: absent from section 2 |
| `_kernels.anode_count` | code → pack: absent from section 2 |
| `_kernels.anodes_for_current` | code → pack: absent from section 2 |
| `_kernels.pipeline_steel_area` | code → pack: absent from section 2 |
| `_kernels.longitudinal_resistance_per_m` | code → pack: absent from section 2 |
| `_kernels.parallel_resistance` | code → pack: absent from section 2 |
| `_kernels.conservative_metallic_drop` | code → pack: absent from section 2 |
| `_kernels.positive_quadratic_root` | code → pack: absent from section 2 |
| `_kernels.long_slender_standoff` | code → pack: absent from section 2 |
| `_kernels.short_slender_standoff` | code → pack: absent from section 2 |
| `_kernels.slender_standoff` | code → pack: absent from section 2 |
| `_kernels.long_flush` | code → pack: absent from section 2 |
| `_kernels.short_flush_or_bracelet` | code → pack: absent from section 2 |
| `_kernels.resistance_proximity_factor` | code → pack: absent from section 2 |
| `_kernels.equivalent_radius_from_periphery` | code → pack: absent from section 2 |
| `_kernels.equivalent_radius_from_mass` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.PhaseValues` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.StructureDemandInput` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.BankAnodeInput` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.PipelineSideInput` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.PipelineSideInput.validate_joint_area` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.BankInput` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.AnodeBankDesignInput` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.CoatingResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.EnvelopeResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.SideResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.ResistanceResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.DemandResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.StatusResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.BankResult` | code → pack: absent from section 2 |
| `_pipeline_anode_bank_models.AnodeBankDesignResult` | code → pack: absent from section 2 |
| `_provisional.ProvisionalValueError` | code → pack: absent from section 2 |
| `_provisional.ProvisionalValue` | code → pack: absent from section 2 |
| `_provisional.ProvisionalValue.range_text` | code → pack: absent from section 2 |
| `_provisional.render_provisional` | code → pack: absent from section 2 |
| `_provisional.render_provisional_table` | code → pack: absent from section 2 |
| `abs_ships.DepletedLongFlush` | code → pack: absent from section 2 |
| `abs_ships.depleted_long_flush` | code → pack: absent from section 2 |
| `abs_ships.design_abs_ships` | code → pack: absent from section 2 |
| `abs_ships_layout.evaluate_layout` | code → pack: absent from section 2 |
| `abs_ships_result.build_result` | code → pack: absent from section 2 |
| `abs_ships_tables.aluminium_properties` | code → pack: absent from section 2 |
| `abs_ships_tables.zinc_properties` | code → pack: absent from section 2 |
| `abs_ships_tables.coating_breakdown_range` | code → pack: absent from section 2 |
| `abs_ships_tables.design_current_density` | code → pack: absent from section 2 |
| `abs_ships_tables.average_coated_current_density` | code → pack: absent from section 2 |
| `abs_ships_tables.equation_reference` | code → pack: absent from section 2 |
| `abs_ships_tables.protection_potential` | code → pack: absent from section 2 |
| `abs_ships_tables.citation_label` | code → pack: absent from section 2 |
| `anode_depletion.AnodeStatus` | code → pack: absent from section 2 |
| `anode_depletion.DepletionResult` | code → pack: absent from section 2 |
| `anode_depletion.DepletionProfile` | code → pack: absent from section 2 |
| `anode_depletion.InspectionRecommendation` | code → pack: absent from section 2 |
| `anode_depletion.calculate_remaining_life` | code → pack: absent from section 2 |
| `anode_depletion.generate_depletion_profile` | code → pack: absent from section 2 |
| `anode_depletion.recommend_inspection_interval` | code → pack: absent from section 2 |
| `anode_sizing.AnodeType` | code → pack: absent from section 2 |
| `anode_sizing.AnodeSizingInput` | code → pack: absent from section 2 |
| `anode_sizing.AnodeSizingResult` | code → pack: absent from section 2 |
| `anode_sizing.calculate_current_demand` | code → pack: absent from section 2 |
| `anode_sizing.calculate_anode_mass` | code → pack: absent from section 2 |
| `anode_sizing.calculate_anode_resistance` | code → pack: absent from section 2 |
| `anode_sizing.depleted_equivalent_radius` | code → pack: absent from section 2 |
| `anode_sizing.governing_case` | code → pack: absent from section 2 |
| `anode_sizing.design_cp_system` | code → pack: absent from section 2 |
| `api_rp_1632.anode_driving_voltage` | code → pack: absent from section 2 |
| `api_rp_1632.anode_resistance_vertical_rod` | code → pack: absent from section 2 |
| `api_rp_1632.current_demand` | code → pack: absent from section 2 |
| `api_rp_1632.current_per_anode` | code → pack: absent from section 2 |
| `api_rp_1632.number_of_anodes` | code → pack: absent from section 2 |
| `api_rp_1632.anode_life_years` | code → pack: absent from section 2 |
| `api_rp_1632.check_protection_potential` | code → pack: absent from section 2 |
| `b401_anode_families.AnodeFamily` | code → pack: absent from section 2 |
| `b401_anode_families.FamilyDemand` | code → pack: absent from section 2 |
| `b401_anode_families.design_anode_family` | code → pack: absent from section 2 |
| `b401_anode_families.overall_family_status` | code → pack: absent from section 2 |
| `b401_component_allocations.named_rows` | code → pack: absent from section 2 |
| `b401_component_allocations.parse_allocations` | code → pack: absent from section 2 |
| `b401_component_allocations.parse_families` | code → pack: absent from section 2 |
| `b401_component_allocations.allocation_check` | code → pack: absent from section 2 |
| `b401_component_allocations.reconcile_allocations` | code → pack: absent from section 2 |
| `b401_component_connectivity.components` | code → pack: absent from section 2 |
| `b401_component_connectivity.continuity` | code → pack: absent from section 2 |
| `b401_component_connectivity.continuity_path` | code → pack: absent from section 2 |
| `b401_component_connectivity.continuity_records` | code → pack: absent from section 2 |
| `b401_component_report.build_b401_component_sections` | code → pack: absent from section 2 |
| `b401_component_route.run_b401_components` | code → pack: absent from section 2 |
| `b401_component_zones.f103_breakdown` | code → pack: absent from section 2 |
| `b401_component_zones.zone_calculations` | code → pack: absent from section 2 |
| `b401_family_report.build_b401_family_sections` | code → pack: absent from section 2 |
| `b401_family_route.run_b401_families` | code → pack: absent from section 2 |
| `b401_structures_phases.run_b401_structures_phases` | code → pack: absent from section 2 |
| `b401_structures_phases_report.build_b401_structures_phases_sections` | code → pack: absent from section 2 |
| `b401_structures_phases_retrofit.retrofit_result` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.mapping` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.nonempty` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.number` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.basis` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.project_basis` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.flatten_components` | code → pack: absent from section 2 |
| `b401_structures_phases_schema.rule_citations` | code → pack: absent from section 2 |
| `b401_tables.EditionSource` | code → pack: absent from section 2 |
| `b401_tables.Climate` | code → pack: absent from section 2 |
| `b401_tables.DepthBand` | code → pack: absent from section 2 |
| `b401_tables.PaintCategory` | code → pack: absent from section 2 |
| `b401_tables.AnodeMaterial` | code → pack: absent from section 2 |
| `b401_tables.AnodeEnvironment` | code → pack: absent from section 2 |
| `b401_tables.AnodeShape` | code → pack: absent from section 2 |
| `b401_tables.DesignPhase` | code → pack: absent from section 2 |
| `b401_tables.AnodeTemperatureRow` | code → pack: absent from section 2 |
| `b401_tables.citation_label` | code → pack: absent from section 2 |
| `b401_tables.edition_source` | code → pack: absent from section 2 |
| `b401_tables.edition_provenance` | code → pack: absent from section 2 |
| `b401_tables.table_label` | code → pack: absent from section 2 |
| `b401_tables.climate_from_temperature` | code → pack: absent from section 2 |
| `b401_tables.depth_band` | code → pack: absent from section 2 |
| `b401_tables.anode_temperature_row` | code → pack: absent from section 2 |
| `b401_tables.design_current_density` | code → pack: absent from section 2 |
| `b401_tables.reinforcement_current_density` | code → pack: absent from section 2 |
| `b401_tables.coating_breakdown_constants` | code → pack: absent from section 2 |
| `b401_tables.anode_capacity` | code → pack: absent from section 2 |
| `b401_tables.anode_closed_circuit_potential` | code → pack: absent from section 2 |
| `b401_tables.utilisation_factor` | code → pack: absent from section 2 |
| `b401_tables.anode_resistance_citation` | code → pack: absent from section 2 |
| `b401_tables.protection_potential` | code → pack: absent from section 2 |
| `b401_tables.design_driving_voltage` | code → pack: absent from section 2 |
| `b401_tables.buried_current_density` | code → pack: absent from section 2 |
| `coating.CoatingCategory` | code → pack: absent from section 2 |
| `coating.CoatingConstants` | code → pack: absent from section 2 |
| `coating.CoatingConstants.citations` | code → pack: absent from section 2 |
| `coating.coating_constants` | code → pack: absent from section 2 |
| `coating.CoatingBreakdownResult` | code → pack: absent from section 2 |
| `coating.CoatingBreakdownResult.edition` | code → pack: absent from section 2 |
| `coating.CoatingLifeResult` | code → pack: absent from section 2 |
| `coating.coating_breakdown_factors` | code → pack: absent from section 2 |
| `coating.coating_life_estimate` | code → pack: absent from section 2 |
| `coating.effective_bare_area_coated` | code → pack: absent from section 2 |
| `corrosion_rate.CO2CorrosionInput` | code → pack: absent from section 2 |
| `corrosion_rate.CO2CorrosionResult` | code → pack: absent from section 2 |
| `corrosion_rate.faraday_rate_factor` | code → pack: absent from section 2 |
| `corrosion_rate.de_waard_milliams_co2` | code → pack: absent from section 2 |
| `corrosion_rate.norsok_m506_co2` | code → pack: absent from section 2 |
| `corrosion_rate.GalvanicCorrosionInput` | code → pack: absent from section 2 |
| `corrosion_rate.GalvanicCorrosionInput.rate_factor` | code → pack: absent from section 2 |
| `corrosion_rate.GalvanicCorrosionResult` | code → pack: absent from section 2 |
| `corrosion_rate.oxygen_limiting_current_density` | code → pack: absent from section 2 |
| `corrosion_rate.galvanic_corrosion` | code → pack: absent from section 2 |
| `corrosion_rate.pitting_rate_estimate` | code → pack: absent from section 2 |
| `cp_monitoring.ReferenceElectrodeType` | code → pack: absent from section 2 |
| `cp_monitoring.MonitoringEnvironment` | code → pack: absent from section 2 |
| `cp_monitoring.ReferenceElectrodeRecommendation` | code → pack: absent from section 2 |
| `cp_monitoring.MonitoringSystemSpec` | code → pack: absent from section 2 |
| `cp_monitoring.AlarmThresholds` | code → pack: absent from section 2 |
| `cp_monitoring.select_reference_electrode` | code → pack: absent from section 2 |
| `cp_monitoring.design_monitoring_system` | code → pack: absent from section 2 |
| `cp_monitoring.set_alarm_thresholds` | code → pack: absent from section 2 |
| `cp_reporting.ComplianceStatus` | code → pack: absent from section 2 |
| `cp_reporting.RecommendationPriority` | code → pack: absent from section 2 |
| `cp_reporting.ComplianceCheck` | code → pack: absent from section 2 |
| `cp_reporting.Recommendation` | code → pack: absent from section 2 |
| `cp_reporting.RemainingLifeSummary` | code → pack: absent from section 2 |
| `cp_reporting.CPAssessmentReport` | code → pack: absent from section 2 |
| `cp_reporting.compliance_check_potential` | code → pack: absent from section 2 |
| `cp_reporting.generate_assessment_report` | code → pack: absent from section 2 |
| `cp_reporting.remaining_life_summary` | code → pack: absent from section 2 |
| `cp_survey.CISSurveyPoint` | code → pack: absent from section 2 |
| `cp_survey.CISAnalysisResult` | code → pack: absent from section 2 |
| `cp_survey.DCVGIndication` | code → pack: absent from section 2 |
| `cp_survey.AttenuationResult` | code → pack: absent from section 2 |
| `cp_survey.analyze_cis_survey` | code → pack: absent from section 2 |
| `cp_survey.classify_dcvg_indication` | code → pack: absent from section 2 |
| `cp_survey.attenuation_analysis` | code → pack: absent from section 2 |
| `dnv_rp_b401.current_demand` | code → pack: absent from section 2 |
| `dnv_rp_b401.anode_mass_requirement` | code → pack: absent from section 2 |
| `dnv_rp_b401.coating_breakdown_factor` | code → pack: absent from section 2 |
| `dnv_rp_b401.anode_resistance_slender_standoff` | code → pack: absent from section 2 |
| `dnv_rp_b401.anode_current_output` | code → pack: absent from section 2 |
| `dnv_rp_b401.equivalent_radius_from_mass` | code → pack: absent from section 2 |
| `dnv_rp_b401.number_of_anodes` | code → pack: absent from section 2 |
| `dnv_rp_b401.protected_length` | code → pack: absent from section 2 |
| `dnv_rp_b401.flush_anode_resistance` | code → pack: absent from section 2 |
| `dnv_rp_f103.BraceletDesignInput` | code → pack: absent from section 2 |
| `dnv_rp_f103.BraceletDesignInput.surface_area_m2` | code → pack: absent from section 2 |
| `dnv_rp_f103.BraceletDesignInput.field_joint_fraction` | code → pack: absent from section 2 |
| `dnv_rp_f103.BraceletDesignInput.resolved_anode_environment` | code → pack: absent from section 2 |
| `dnv_rp_f103.BraceletDesignInput.bracelet_area_m2` | code → pack: absent from section 2 |
| `dnv_rp_f103.protected_length` | code → pack: absent from section 2 |
| `dnv_rp_f103.design_bracelet_cp` | code → pack: absent from section 2 |
| `dnv_rp_f106.CoatingType` | code → pack: absent from section 2 |
| `dnv_rp_f106.CoatingProperties` | code → pack: absent from section 2 |
| `dnv_rp_f106.SelectionResult` | code → pack: absent from section 2 |
| `dnv_rp_f106.ValidationResult` | code → pack: absent from section 2 |
| `dnv_rp_f106.HolidayDetectionResult` | code → pack: absent from section 2 |
| `dnv_rp_f106.select_coating` | code → pack: absent from section 2 |
| `dnv_rp_f106.validate_thickness` | code → pack: absent from section 2 |
| `dnv_rp_f106.holiday_detection_result` | code → pack: absent from section 2 |
| `dnv_rp_f106.holiday_detection_voltage` | code → pack: absent from section 2 |
| `engine_adapter.run_cathodic_protection` | code → pack: absent from section 2 |
| `f103_tables.F103EditionSource` | code → pack: absent from section 2 |
| `f103_tables.Exposure` | code → pack: absent from section 2 |
| `f103_tables.FluidTemperatureBand` | code → pack: absent from section 2 |
| `f103_tables.LinepipeCoating` | code → pack: absent from section 2 |
| `f103_tables.LinepipeCoating.cds_number` | code → pack: absent from section 2 |
| `f103_tables.LinepipeCoating.max_temperature_c` | code → pack: absent from section 2 |
| `f103_tables.FieldJointCoating` | code → pack: absent from section 2 |
| `f103_tables.FieldJointCoating.max_temperature_c` | code → pack: absent from section 2 |
| `f103_tables.FieldJointCoating2019` | code → pack: absent from section 2 |
| `f103_tables.FieldJointCoating2019.max_temperature_c` | code → pack: absent from section 2 |
| `f103_tables.LinepipeCoatingRow` | code → pack: absent from section 2 |
| `f103_tables.LinepipeCoatingRow2019` | code → pack: absent from section 2 |
| `f103_tables.FieldJointCoatingRow` | code → pack: absent from section 2 |
| `f103_tables.FieldJointCoatingRow2019` | code → pack: absent from section 2 |
| `f103_tables.b401_edition_for_f103` | code → pack: absent from section 2 |
| `f103_tables.edition_source` | code → pack: absent from section 2 |
| `f103_tables.edition_provenance` | code → pack: absent from section 2 |
| `f103_tables.temperature_bands` | code → pack: absent from section 2 |
| `f103_tables.anode_bank_formula_citations` | code → pack: absent from section 2 |
| `f103_tables.fluid_temperature_band` | code → pack: absent from section 2 |
| `f103_tables.mean_current_density` | code → pack: absent from section 2 |
| `f103_tables.linepipe_coating_row` | code → pack: absent from section 2 |
| `f103_tables.linepipe_coating_constants` | code → pack: absent from section 2 |
| `f103_tables.field_joint_coating_row` | code → pack: absent from section 2 |
| `f103_tables.field_joint_coating_constants` | code → pack: absent from section 2 |
| `f103_tables.field_joint_coating_2019_ids` | code → pack: absent from section 2 |
| `f103_tables.field_joint_infill_choices_2019` | code → pack: absent from section 2 |
| `f103_tables.resolve_field_joint_coating_2019` | code → pack: absent from section 2 |
| `f103_tables.bracelet_utilisation_factor` | code → pack: absent from section 2 |
| `f103_tables.anode_capacity` | code → pack: absent from section 2 |
| `f103_tables.anode_closed_circuit_potential` | code → pack: absent from section 2 |
| `f103_tables.protection_potential` | code → pack: absent from section 2 |
| `f103_tables.design_driving_voltage` | code → pack: absent from section 2 |
| `fuel_system_cp.CoatingType` | code → pack: absent from section 2 |
| `fuel_system_cp.FuelPipeSegment` | code → pack: absent from section 2 |
| `fuel_system_cp.ImpressedCurrentGroundBed` | code → pack: absent from section 2 |
| `fuel_system_cp.RectifierOutput` | code → pack: absent from section 2 |
| `fuel_system_cp.pipe_surface_area` | code → pack: absent from section 2 |
| `fuel_system_cp.effective_bare_area` | code → pack: absent from section 2 |
| `fuel_system_cp.current_demand_segment` | code → pack: absent from section 2 |
| `fuel_system_cp.total_current_demand` | code → pack: absent from section 2 |
| `fuel_system_cp.design_ground_bed` | code → pack: absent from section 2 |
| `fuel_system_cp.design_rectifier` | code → pack: absent from section 2 |
| `iccp_design.AnodeBedType` | code → pack: absent from section 2 |
| `iccp_design.AnodeMaterial` | code → pack: absent from section 2 |
| `iccp_design.IccpEnvironment` | code → pack: absent from section 2 |
| `iccp_design.AnodeLifeBasis` | code → pack: absent from section 2 |
| `iccp_design.IccpAnodeMaterialRecord` | code → pack: absent from section 2 |
| `iccp_design.RectifierSizingInput` | code → pack: absent from section 2 |
| `iccp_design.RectifierSizingResult` | code → pack: absent from section 2 |
| `iccp_design.rectifier_sizing` | code → pack: absent from section 2 |
| `iccp_design.AnodeBedResult` | code → pack: absent from section 2 |
| `iccp_design.single_vertical_anode_resistance` | code → pack: absent from section 2 |
| `iccp_design.multiple_vertical_anode_resistance` | code → pack: absent from section 2 |
| `iccp_design.deep_well_resistance` | code → pack: absent from section 2 |
| `iccp_design.horizontal_column_resistance` | code → pack: absent from section 2 |
| `iccp_design.iccp_anode_life` | code → pack: absent from section 2 |
| `iccp_design.anode_bed_design` | code → pack: absent from section 2 |
| `iccp_design.cable_sizing` | code → pack: absent from section 2 |
| `iso_15589_2.initial_current_density` | code → pack: absent from section 2 |
| `iso_15589_2.coating_breakdown_factor` | code → pack: absent from section 2 |
| `iso_15589_2.pipeline_current_demand` | code → pack: absent from section 2 |
| `iso_15589_2.anode_resistance` | code → pack: absent from section 2 |
| `iso_15589_2.anode_output_current` | code → pack: absent from section 2 |
| `iso_15589_2.anode_mass_requirement` | code → pack: absent from section 2 |
| `iso_15589_2.check_protection_potential` | code → pack: absent from section 2 |
| `marine_cp.ZoneType` | code → pack: absent from section 2 |
| `marine_cp.Zone` | code → pack: absent from section 2 |
| `marine_cp.MarineCPInput` | code → pack: absent from section 2 |
| `marine_cp.MarineCPResult` | code → pack: absent from section 2 |
| `marine_cp.MarineCPResult.edition` | code → pack: absent from section 2 |
| `marine_cp.seawater_current_density` | code → pack: absent from section 2 |
| `marine_cp.get_seawater_current_density` | code → pack: absent from section 2 |
| `marine_cp.zone_current_density` | code → pack: absent from section 2 |
| `marine_cp.calculate_zone_demand` | code → pack: absent from section 2 |
| `marine_structure_cp.ExposureZone` | code → pack: absent from section 2 |
| `marine_structure_cp.ClimateRegion` | code → pack: absent from section 2 |
| `marine_structure_cp.DesignLoopResult` | code → pack: absent from section 2 |
| `marine_structure_cp.standoff_anode_design_loop` | code → pack: absent from section 2 |
| `marine_structure_cp.StructuralZone` | code → pack: absent from section 2 |
| `marine_structure_cp.StructuralZone.design_area_m2` | code → pack: absent from section 2 |
| `marine_structure_cp.StructuralZone.area_basis` | code → pack: absent from section 2 |
| `marine_structure_cp.StructuralZone.effective_breakdown_factor` | code → pack: absent from section 2 |
| `marine_structure_cp.MarineCPResult` | code → pack: absent from section 2 |
| `marine_structure_cp.MarineCPResult.edition` | code → pack: absent from section 2 |
| `marine_structure_cp.RetrofitAssessment` | code → pack: absent from section 2 |
| `marine_structure_cp.zone_current_density` | code → pack: absent from section 2 |
| `marine_structure_cp.marine_structure_current_demand` | code → pack: absent from section 2 |
| `marine_structure_cp.anode_distribution` | code → pack: absent from section 2 |
| `marine_structure_cp.retrofit_assessment` | code → pack: absent from section 2 |
| `pipeline_anode_bank.design_anode_bank_cp` | code → pack: absent from section 2 |
| `pipeline_cp.PipelineEnvironment` | code → pack: absent from section 2 |
| `pipeline_cp.PipelineCPInput` | code → pack: absent from section 2 |
| `pipeline_cp.PipelineCPResult` | code → pack: absent from section 2 |
| `pipeline_cp.AnodeSpacingResult` | code → pack: absent from section 2 |
| `pipeline_cp.pipeline_current_demand` | code → pack: absent from section 2 |
| `pipeline_cp.anode_spacing` | code → pack: absent from section 2 |
| `pipeline_cp.holiday_detection_voltage` | code → pack: absent from section 2 |
| `pipeline_cp.CriteriaResult` | code → pack: absent from section 2 |
| `pipeline_cp.PipelineDesignResult` | code → pack: absent from section 2 |
| `pipeline_cp.calculate_pipeline_current_demand` | code → pack: absent from section 2 |
| `pipeline_cp.calculate_anode_spacing` | code → pack: absent from section 2 |
| `pipeline_cp.soil_resistivity_correction` | code → pack: absent from section 2 |
| `pipeline_cp.check_potential_criteria` | code → pack: absent from section 2 |
| `pipeline_cp.design_pipeline_cp` | code → pack: absent from section 2 |
| `report_adapters.build_assessment_spec` | code → pack: absent from section 2 |
| `report_adapters.assessment_report` | code → pack: absent from section 2 |
| `report_elevation.Member` | code → pack: absent from section 2 |
| `report_elevation.Zone` | code → pack: absent from section 2 |
| `report_elevation.Geometry` | code → pack: absent from section 2 |
| `report_elevation.validate_geometry` | code → pack: absent from section 2 |
| `report_elevation.geometry_zone_areas` | code → pack: absent from section 2 |
| `report_elevation.monitoring_conclusion` | code → pack: absent from section 2 |
| `report_elevation.design_blocks` | code → pack: absent from section 2 |
| `report_elevation.percentage_text` | code → pack: absent from section 2 |
| `report_elevation.geometry_caveats` | code → pack: absent from section 2 |
| `report_recommendations.arrangement_options` | code → pack: absent from section 2 |
| `report_recommendations.failed_keys` | code → pack: absent from section 2 |
| `report_recommendations.recommendation_block` | code → pack: absent from section 2 |
| `report_recommendations.conclusions_block` | code → pack: absent from section 2 |
| `report_recommendations.recommendation_caveats` | code → pack: absent from section 2 |
| `report_reference.reference_projection` | code → pack: absent from section 2 |
| `report_reference.reference_figure` | code → pack: absent from section 2 |
| `report_reference.reference_zone_rows` | code → pack: absent from section 2 |
| `report_reference.reference_zone_blocks` | code → pack: absent from section 2 |
| `report_reference.reference_arrangements` | code → pack: absent from section 2 |
| `report_reference.reference_checks` | code → pack: absent from section 2 |
| `report_reference.reference_sections` | code → pack: absent from section 2 |
| `report_reference.reference_basis` | code → pack: absent from section 2 |
| `report_reference.reference_member_tables` | code → pack: absent from section 2 |
| `report_reference.reference_comparison` | code → pack: absent from section 2 |
| `report_reference.reference_references` | code → pack: absent from section 2 |
| `report_verification.published_outputs` | code → pack: absent from section 2 |
| `report_verification.aggregate_table` | code → pack: absent from section 2 |
| `report_verification.measured_statement` | code → pack: absent from section 2 |
| `report_verification.published_status` | code → pack: absent from section 2 |
| `report_verification.measured_failure` | code → pack: absent from section 2 |
| `report_verification.measured_options` | code → pack: absent from section 2 |
| `report_verification.published_recommendations` | code → pack: absent from section 2 |
| `report_verification.verification_summary` | code → pack: absent from section 2 |
| `stray_current.InterferenceType` | code → pack: absent from section 2 |
| `stray_current.MitigationType` | code → pack: absent from section 2 |
| `stray_current.ShiftBasis` | code → pack: absent from section 2 |
| `stray_current.StrayCurrentInput` | code → pack: absent from section 2 |
| `stray_current.StrayCurrentResult` | code → pack: absent from section 2 |
| `stray_current.MitigationDesign` | code → pack: absent from section 2 |
| `stray_current.dc_shift_limit_mV` | code → pack: absent from section 2 |
| `stray_current.pipe_attenuation_constant` | code → pack: absent from section 2 |
| `stray_current.point_source_shift_mV` | code → pack: absent from section 2 |
| `stray_current.coupon_diameter_m` | code → pack: absent from section 2 |
| `stray_current.ac_current_density_A_m2` | code → pack: absent from section 2 |
| `stray_current.assess_stray_current` | code → pack: absent from section 2 |
| `stray_current.design_drainage_bond` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `[path removed]` | pack → code: file missing or range invalid |

### 8. `src/digitalmodel/code_checks/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 12.

| Function | Direction |
| --- | --- |
| `dnv_os_f101.plastic_axial_capacity` | code → pack: absent from section 2 |
| `dnv_os_f101.plastic_moment_capacity` | code → pack: absent from section 2 |
| `dnv_os_f101.pressure_containment_resistance` | code → pack: absent from section 2 |
| `dnv_os_f101.local_buckling_lcc` | code → pack: absent from section 2 |
| `dnv_os_f101.elastic_collapse_pressure` | code → pack: absent from section 2 |
| `dnv_os_f101.plastic_collapse_pressure` | code → pack: absent from section 2 |
| `dnv_os_f101.collapse_pressure` | code → pack: absent from section 2 |
| `dnv_os_f101.propagation_pressure` | code → pack: absent from section 2 |
| `dnv_os_f101.propagation_check` | code → pack: absent from section 2 |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 9. `src/digitalmodel/data/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 6.

| Function | Direction |
| --- | --- |
| `mooring_component_schema.row_identity` | code → pack: absent from section 2 |
| `mooring_component_schema.validate_provenance` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 10. `src/digitalmodel/data_systems/data_procurement/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 2.

| Function | Direction |
| --- | --- |
| `bsee_data_refresh.GetData_download_file_from_url` | code → pack: absent from section 2 |
| `rig_name_matcher_openwells.rig_name_matcher_openwells` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |
| `[path removed]` | pack → code: file missing or range invalid |

### 11. `src/digitalmodel/drilling_riser/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 151.

| Function | Direction |
| --- | --- |
| `adapter.register_riser_components` | code → pack: absent from section 2 |
| `adapter.compute_riser_string_weight_kn` | code → pack: absent from section 2 |
| `assembly.StackupItem.component_id` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel.from_component_counts` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel.from_records` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel.total_submerged_weight_kn` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel.total_dry_weight_kn` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel.gross_submerged_weight_and_uplift` | code → pack: absent from section 2 |
| `assembly.RiserStackupModel.total_length_m` | code → pack: absent from section 2 |
| `assembly.RigCapabilityCheck` | code → pack: absent from section 2 |
| `assembly.check_rig_capability` | code → pack: absent from section 2 |
| `calc_contracts.CalcContractError` | code → pack: absent from section 2 |
| `calc_contracts.resolve_wiki_root` | code → pack: absent from section 2 |
| `calc_contracts.load_calc_contracts` | code → pack: absent from section 2 |
| `calc_contracts.contract_referenced_rsu_ids` | code → pack: absent from section 2 |
| `calc_contracts.find_contract` | code → pack: absent from section 2 |
| `campaign.load_base_spec` | code → pack: absent from section 2 |
| `campaign.offset_xy_m` | code → pack: absent from section 2 |
| `campaign.split_at_couplings` | code → pack: absent from section 2 |
| `campaign.case_spec` | code → pack: absent from section 2 |
| `campaign.seeded_statics` | code → pack: absent from section 2 |
| `campaign.statics_paths` | code → pack: absent from section 2 |
| `campaign.set_line_tension` | code → pack: absent from section 2 |
| `campaign.calibrate_line_tension` | code → pack: absent from section 2 |
| `campaign.robust_statics` | code → pack: absent from section 2 |
| `campaign.open_water_statics` | code → pack: absent from section 2 |
| `campaign.hang_off_statics` | code → pack: absent from section 2 |
| `campaign.tensioner_vertical_target_n` | code → pack: absent from section 2 |
| `campaign.physical_state_checks` | code → pack: absent from section 2 |
| `campaign.apply_statics_settings` | code → pack: absent from section 2 |
| `campaign.RiserCampaignAdapter` | code → pack: absent from section 2 |
| `campaign.RiserCampaignAdapter.spec` | code → pack: absent from section 2 |
| `campaign.RiserCampaignAdapter.build` | code → pack: absent from section 2 |
| `campaign.RiserCampaignAdapter.prepare` | code → pack: absent from section 2 |
| `campaign.RiserCampaignAdapter.statics` | code → pack: absent from section 2 |
| `campaign.RiserCampaignAdapter.extract` | code → pack: absent from section 2 |
| `conductor_response.ConductorResponse` | code → pack: absent from section 2 |
| `conductor_response.ConductorResponse.max_moment_nm` | code → pack: absent from section 2 |
| `damping.structural_damping_ratio` | code → pack: absent from section 2 |
| `damping.rayleigh_damping_coefficients` | code → pack: absent from section 2 |
| `damping.modal_damping_equivalent` | code → pack: absent from section 2 |
| `drift_off.load_config` | code → pack: absent from section 2 |
| `drift_off.DriftOffResult` | code → pack: absent from section 2 |
| `drift_off.environmental_force_n` | code → pack: absent from section 2 |
| `drift_off.watch_circle_limit` | code → pack: absent from section 2 |
| `envelope.OperatingMode` | code → pack: absent from section 2 |
| `envelope.SeaState` | code → pack: absent from section 2 |
| `envelope.CurrentProfile` | code → pack: absent from section 2 |
| `envelope.CurrentProfile.drag_load_n_per_m` | code → pack: absent from section 2 |
| `envelope.RiserSection` | code → pack: absent from section 2 |
| `envelope.RiserSection.inner_diameter_m` | code → pack: absent from section 2 |
| `envelope.RiserSection.second_moment_m4` | code → pack: absent from section 2 |
| `envelope.RiserSection.ei_nm2` | code → pack: absent from section 2 |
| `envelope.RigEnvelopeLimits` | code → pack: absent from section 2 |
| `envelope.ConductorInput` | code → pack: absent from section 2 |
| `envelope.ConductorInput.ei_nm2` | code → pack: absent from section 2 |
| `envelope.EnvelopeCriteria` | code → pack: absent from section 2 |
| `envelope.EnvelopeResult` | code → pack: absent from section 2 |
| `envelope.EnvelopeResult.operable_fraction` | code → pack: absent from section 2 |
| `envelope.resolve_envelope_criteria` | code → pack: absent from section 2 |
| `generic_campaign.horizontal_shift` | code → pack: absent from section 2 |
| `generic_campaign.load_criteria` | code → pack: absent from section 2 |
| `generic_campaign.digest` | code → pack: absent from section 2 |
| `generic_campaign.source_digest` | code → pack: absent from section 2 |
| `generic_campaign.calculation_fingerprints` | code → pack: absent from section 2 |
| `generic_campaign.write_text` | code → pack: absent from section 2 |
| `generic_campaign.tagged` | code → pack: absent from section 2 |
| `generic_campaign.read_spec` | code → pack: absent from section 2 |
| `generic_campaign.sourced_tree` | code → pack: absent from section 2 |
| `generic_campaign.column_basis` | code → pack: absent from section 2 |
| `generic_campaign.case_table` | code → pack: absent from section 2 |
| `generic_campaign.validate_model` | code → pack: absent from section 2 |
| `generic_campaign.build_models` | code → pack: absent from section 2 |
| `generic_campaign.screen_rows` | code → pack: absent from section 2 |
| `generic_campaign.prepare` | code → pack: absent from section 2 |
| `generic_tension.surviving_share` | code → pack: absent from section 2 |
| `generic_tension.weight_reserve` | code → pack: absent from section 2 |
| `generic_tension.sections` | code → pack: absent from section 2 |
| `generic_tension.column_geometry` | code → pack: absent from section 2 |
| `generic_tension.member_basis` | code → pack: absent from section 2 |
| `generic_tension.link_geometry` | code → pack: absent from section 2 |
| `generic_tension.tension_basis` | code → pack: absent from section 2 |
| `generic_tension.checks` | code → pack: absent from section 2 |
| `generic_tension.hand_estimate` | code → pack: absent from section 2 |
| `generic_tension_channels.summarize_samples` | code → pack: absent from section 2 |
| `generic_tension_channels.extract` | code → pack: absent from section 2 |
| `metocean_inputs.reduce_to_scatter` | code → pack: absent from section 2 |
| `metocean_inputs.scatter_to_seastates` | code → pack: absent from section 2 |
| `metocean_inputs.current_to_surface_speeds` | code → pack: absent from section 2 |
| `operability.significant_wave_height_limit` | code → pack: absent from section 2 |
| `operability.operability_fraction` | code → pack: absent from section 2 |
| `operability.watch_circle_radius_m` | code → pack: absent from section 2 |
| `operability_atlas.load_config` | code → pack: absent from section 2 |
| `operability_atlas.governing_utilisation` | code → pack: absent from section 2 |
| `operability_atlas.content_fingerprint` | code → pack: absent from section 2 |
| `operability_atlas.build_atlas` | code → pack: absent from section 2 |
| `operability_screening.OperabilityScreen` | code → pack: absent from section 2 |
| `operability_screening.screen_operability` | code → pack: absent from section 2 |
| `response_correction.FlexjointModels` | code → pack: absent from section 2 |
| `response_correction.CorrectedFlexjoint` | code → pack: absent from section 2 |
| `response_correction.fit_flexjoint_models` | code → pack: absent from section 2 |
| `response_correction.correct_flexjoint_response` | code → pack: absent from section 2 |
| `response_correction.flexjoint_utilisation` | code → pack: absent from section 2 |
| `riser_response.RiserResponse` | code → pack: absent from section 2 |
| `riser_response.RiserResponse.angle_lower_deg` | code → pack: absent from section 2 |
| `riser_response.RiserResponse.angle_upper_deg` | code → pack: absent from section 2 |
| `riser_response.RiserResponse.max_bending_moment_nm` | code → pack: absent from section 2 |
| `schedule_assembly.ScheduleAssemblyError` | code → pack: absent from section 2 |
| `schedule_assembly.StringGeometry` | code → pack: absent from section 2 |
| `schedule_assembly.StringGeometry.mud_column_m` | code → pack: absent from section 2 |
| `schedule_assembly.StringGeometry.seawater_column_m` | code → pack: absent from section 2 |
| `schedule_assembly.ScheduleAssembly` | code → pack: absent from section 2 |
| `schedule_assembly.ScheduleAssembly.buoyancy_net_lift_kn` | code → pack: absent from section 2 |
| `schedule_assembly.ScheduleAssembly.minimum_top_tension_16q_kn` | code → pack: absent from section 2 |
| `schedule_assembly.load_schedule_assembly` | code → pack: absent from section 2 |
| `schedule_assembly.documented_endpoints` | code → pack: absent from section 2 |
| `section.SectionProperties` | code → pack: absent from section 2 |
| `section.cross_sectional_area` | code → pack: absent from section 2 |
| `section.moment_of_inertia` | code → pack: absent from section 2 |
| `section.polar_moment_of_inertia` | code → pack: absent from section 2 |
| `section.shear_modulus` | code → pack: absent from section 2 |
| `section.section_properties` | code → pack: absent from section 2 |
| `section.section_properties_imperial` | code → pack: absent from section 2 |
| `stackup.top_tension_required` | code → pack: absent from section 2 |
| `stackup.wall_thickness_required` | code → pack: absent from section 2 |
| `stackup.effective_tension` | code → pack: absent from section 2 |
| `stackup.minimum_slip_ring_tension` | code → pack: absent from section 2 |
| `telemetry_inputs.DPState` | code → pack: absent from section 2 |
| `telemetry_inputs.DPState.station_keeping_margin` | code → pack: absent from section 2 |
| `telemetry_inputs.MeasuredResponse` | code → pack: absent from section 2 |
| `telemetry_inputs.TelemetrySnapshot` | code → pack: absent from section 2 |
| `telemetry_inputs.snapshot_to_offset_pct` | code → pack: absent from section 2 |
| `telemetry_inputs.snapshot_to_tension_n` | code → pack: absent from section 2 |
| `telemetry_inputs.snapshots_to_offset_track` | code → pack: absent from section 2 |
| `tool_passage.annular_clearance_mm` | code → pack: absent from section 2 |
| `tool_passage.minimum_riser_id_required` | code → pack: absent from section 2 |
| `tool_passage.spacing_requirement_m` | code → pack: absent from section 2 |
| `twin_loop.evaluate_point` | code → pack: absent from section 2 |
| `twin_loop.run_twin_loop` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/drilling_riser/stackup.py:L1-L163` | pack → code: file missing or range invalid |
| `src/digitalmodel/drilling_riser/stackup.py:L111-L163` | pack → code: file missing or range invalid |

### 12. `src/digitalmodel/drilling_riser/global_model/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 108.

| Function | Direction |
| --- | --- |
| `build.equivalent_string_top_tension_n` | code → pack: absent from section 2 |
| `build.hydrodynamic_od_m` | code → pack: absent from section 2 |
| `build.winch_tension_n` | code → pack: absent from section 2 |
| `events.DriftVessel` | code → pack: absent from section 2 |
| `events.DriftEnvironment` | code → pack: absent from section 2 |
| `events.wave_drift_force_n` | code → pack: absent from section 2 |
| `events.drift_trajectory` | code → pack: absent from section 2 |
| `events.anti_recoil_stages` | code → pack: absent from section 2 |
| `events.recoil_hold_tension_n` | code → pack: absent from section 2 |
| `fatigue_channels.half_cycle_histogram` | code → pack: absent from section 2 |
| `fatigue_channels.half_cycles` | code → pack: absent from section 2 |
| `fatigue_channels.peak_to_valley_ok` | code → pack: absent from section 2 |
| `fatigue_channels.stations` | code → pack: absent from section 2 |
| `fatigue_channels.extract` | code → pack: absent from section 2 |
| `foundation.interpolate_py` | code → pack: absent from section 2 |
| `foundation.spring_stations` | code → pack: absent from section 2 |
| `foundation.spring_table_kn` | code → pack: absent from section 2 |
| `hand_checks.submerged_length_m` | code → pack: absent from section 2 |
| `hand_checks.submerged_weight_n` | code → pack: absent from section 2 |
| `hand_checks.effective_tension_chain` | code → pack: absent from section 2 |
| `hand_checks.ring_weight_n` | code → pack: absent from section 2 |
| `hand_checks.BeamSegment` | code → pack: absent from section 2 |
| `hand_checks.tensioned_beam_periods` | code → pack: absent from section 2 |
| `hand_checks.wkb_string_periods` | code → pack: absent from section 2 |
| `hand_checks.tensioner_lateral_stiffness_n_m` | code → pack: absent from section 2 |
| `hand_checks.beam_model` | code → pack: absent from section 2 |
| `hang_off.section_weight_n` | code → pack: absent from section 2 |
| `hang_off.hung_weight_n` | code → pack: absent from section 2 |
| `hang_off.hang_off_spec` | code → pack: absent from section 2 |
| `hang_off.running_spec` | code → pack: absent from section 2 |
| `open_water.TensionFrame` | code → pack: absent from section 2 |
| `open_water.EdpRelease` | code → pack: absent from section 2 |
| `open_water.OpenWaterRiserSpec` | code → pack: absent from section 2 |
| `open_water.OpenWaterRiserSpec.edp_interface_z_m` | code → pack: absent from section 2 |
| `open_water.OpenWaterRiserSpec.stress_joint_base_arc_m` | code → pack: absent from section 2 |
| `open_water.frame_weight_n` | code → pack: absent from section 2 |
| `open_water.beam_model` | code → pack: absent from section 2 |
| `open_water.release_reference` | code → pack: absent from section 2 |
| `orcaflex_run.load_model` | code → pack: absent from section 2 |
| `orcaflex_run.load_and_solve_statics` | code → pack: absent from section 2 |
| `orcaflex_run.set_time_step` | code → pack: absent from section 2 |
| `orcaflex_run.set_log_interval` | code → pack: absent from section 2 |
| `orcaflex_run.apply_rayleigh_damping` | code → pack: absent from section 2 |
| `orcaflex_run.main_period` | code → pack: absent from section 2 |
| `orcaflex_run.run_dynamics` | code → pack: absent from section 2 |
| `orcaflex_run.governing_responses` | code → pack: absent from section 2 |
| `orcaflex_run.static_responses` | code → pack: absent from section 2 |
| `orcaflex_run.model_files_sha256` | code → pack: absent from section 2 |
| `orcaflex_run.end_effective_tensions` | code → pack: absent from section 2 |
| `orcaflex_run.ring_static_z_m` | code → pack: absent from section 2 |
| `orcaflex_run.tensioner_vertical_sum_n` | code → pack: absent from section 2 |
| `orcaflex_run.ring_vertical_residual_n` | code → pack: absent from section 2 |
| `orcaflex_run.riser_modal_periods` | code → pack: absent from section 2 |
| `orcaflex_run.as_analysed_sections` | code → pack: absent from section 2 |
| `orcaflex_run.open_water_end_tensions` | code → pack: absent from section 2 |
| `orcaflex_run.open_water_physical_checks` | code → pack: absent from section 2 |
| `orcaflex_run.open_water_static_responses` | code → pack: absent from section 2 |
| `orcaflex_run.open_water_governing_responses` | code → pack: absent from section 2 |
| `orcaflex_run.edp_release_history` | code → pack: absent from section 2 |
| `orcaflex_run.window_mean` | code → pack: absent from section 2 |
| `raos.parse_direction_grouped_rao_text` | code → pack: absent from section 2 |
| `spec.tube_section` | code → pack: absent from section 2 |
| `spec.LineSection` | code → pack: absent from section 2 |
| `spec.FlexJoint` | code → pack: absent from section 2 |
| `spec.flex_joint_from_secants` | code → pack: absent from section 2 |
| `spec.TensionRing` | code → pack: absent from section 2 |
| `spec.Tensioners` | code → pack: absent from section 2 |
| `spec.Contents` | code → pack: absent from section 2 |
| `spec.GlobalEnvironment` | code → pack: absent from section 2 |
| `spec.StructuralDamping.stiffness_coefficient_s` | code → pack: absent from section 2 |
| `spec.RiserGlobalModelSpec` | code → pack: absent from section 2 |
| `spec.RiserGlobalModelSpec.ring_z_geometric_m` | code → pack: absent from section 2 |
| `w5_channels.stats` | code → pack: absent from section 2 |
| `w5_channels.compact` | code → pack: absent from section 2 |
| `w5_channels.extreme_rows` | code → pack: absent from section 2 |
| `w5_channels.convex_hull_indices` | code → pack: absent from section 2 |
| `w5_channels.tm_hull_rows` | code → pack: absent from section 2 |
| `w5_channels.missing_channels` | code → pack: absent from section 2 |
| `w5_channels.extract` | code → pack: absent from section 2 |
| `w5_channels.open_water_points` | code → pack: absent from section 2 |
| `w5_channels.extract_open_water` | code → pack: absent from section 2 |
| `w5_channels.event_series` | code → pack: absent from section 2 |
| `w5_channels.recoil_summary` | code → pack: absent from section 2 |
| `w5_channels.extract_hang_off` | code → pack: absent from section 2 |
| `w5_channels.hang_off_summary` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `scripts/capabilities/build_onepagers.py:L1-L915` | pack → code: file missing or range invalid |
| `scripts/capabilities/build_onepagers.py:L6-L915` | pack → code: file missing or range invalid |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L396-L426` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L270-L300` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 13. `src/digitalmodel/drilling_riser/postprocess/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 69.

| Function | Direction |
| --- | --- |
| `aggregate.DigestMismatch` | code → pack: absent from section 2 |
| `aggregate.CaseRecord.base_id` | code → pack: absent from section 2 |
| `aggregate.sha256_file` | code → pack: absent from section 2 |
| `aggregate.load_verified` | code → pack: absent from section 2 |
| `aggregate.seed_groups` | code → pack: absent from section 2 |
| `aggregate.case_key` | code → pack: absent from section 2 |
| `channels.point_value` | code → pack: absent from section 2 |
| `channels.point_rows` | code → pack: absent from section 2 |
| `channels.row_value` | code → pack: absent from section 2 |
| `channels.extreme_row` | code → pack: absent from section 2 |
| `channels.range_series` | code → pack: absent from section 2 |
| `channels.range_extreme` | code → pack: absent from section 2 |
| `channels.stroke_range` | code → pack: absent from section 2 |
| `checks.NotEvaluated` | code → pack: absent from section 2 |
| `checks.CheckValue` | code → pack: absent from section 2 |
| `checks.envelope_min_capacity` | code → pack: absent from section 2 |
| `checks.bore_differential_kpa` | code → pack: absent from section 2 |
| `checks.zero_crossing` | code → pack: absent from section 2 |
| `checks.fj_mean` | code → pack: absent from section 2 |
| `checks.fj_max` | code → pack: absent from section 2 |
| `checks.fj_max_avail` | code → pack: absent from section 2 |
| `checks.von_mises` | code → pack: absent from section 2 |
| `checks.coupling_rating` | code → pack: absent from section 2 |
| `checks.t_eff_min` | code → pack: absent from section 2 |
| `checks.t_min` | code → pack: absent from section 2 |
| `checks.tension_setting_max` | code → pack: absent from section 2 |
| `checks.tj_stroke` | code → pack: absent from section 2 |
| `checks.tensioner_stroke` | code → pack: absent from section 2 |
| `checks.connector_tmp` | code → pack: absent from section 2 |
| `checks.conductor_bending` | code → pack: absent from section 2 |
| `checks.cr10_station_mean` | code → pack: absent from section 2 |
| `checks.dyn_stress_range` | code → pack: absent from section 2 |
| `checks.moonpool_clearance` | code → pack: absent from section 2 |
| `checks.evaluate_doc` | code → pack: absent from section 2 |
| `clearance.reach` | code → pack: absent from section 2 |
| `clearance.limiting_angle_deg` | code → pack: absent from section 2 |
| `evaluate.evaluate_case` | code → pack: absent from section 2 |
| `evaluate.summarise` | code → pack: absent from section 2 |
| `extremes.GumbelFit` | code → pack: absent from section 2 |
| `extremes.GumbelFit.as_dict` | code → pack: absent from section 2 |
| `extremes.gumbel_quantile` | code → pack: absent from section 2 |
| `extremes.gumbel_fit` | code → pack: absent from section 2 |
| `pressure.burst_pressure_mpa` | code → pack: absent from section 2 |
| `pressure.collapse_pressure_mpa` | code → pack: absent from section 2 |
| `station_geometry.section_lengths` | code → pack: absent from section 2 |
| `stress_range.stress_range_limit_ksi` | code → pack: absent from section 2 |
| `stress_range.range_over_theta` | code → pack: absent from section 2 |
| `stress_range.extract` | code → pack: absent from section 2 |
| `stress_range.merge_stress_range` | code → pack: absent from section 2 |
| `stress_range.turning_points` | code → pack: absent from section 2 |
| `stress_range.significant_range` | code → pack: absent from section 2 |
| `stress_range.theta_histories` | code → pack: absent from section 2 |
| `stress_range.significant_over_theta` | code → pack: absent from section 2 |
| `stress_range.coupling_positions` | code → pack: absent from section 2 |
| `stress_range.joint_sections` | code → pack: absent from section 2 |
| `stress_range.classify_stations` | code → pack: absent from section 2 |
| `stress_range.extract_significant` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 14. `src/digitalmodel/drilling_riser/stackup_drawing/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 51.

| Function | Direction |
| --- | --- |
| `adapters.from_json` | code → pack: absent from section 2 |
| `adapters.from_schedule_assembly` | code → pack: absent from section 2 |
| `reconcile.canonical_digests` | code → pack: absent from section 2 |
| `reconcile.main` | code → pack: absent from section 2 |
| `render.Layout` | code → pack: absent from section 2 |
| `render.ElevationTransform` | code → pack: absent from section 2 |
| `render.ElevationTransform.build` | code → pack: absent from section 2 |
| `render.ElevationTransform.y_bottom` | code → pack: absent from section 2 |
| `render.ElevationTransform.y` | code → pack: absent from section 2 |
| `render.ElevationTransform.pieces` | code → pack: absent from section 2 |
| `render.ElevationTransform.to_json` | code → pack: absent from section 2 |
| `render.svg_rect` | code → pack: absent from section 2 |
| `render.svg_line` | code → pack: absent from section 2 |
| `render.svg_circle` | code → pack: absent from section 2 |
| `render.svg_path` | code → pack: absent from section 2 |
| `render.svg_polyline` | code → pack: absent from section 2 |
| `render.svg_text` | code → pack: absent from section 2 |
| `render.fmt_num` | code → pack: absent from section 2 |
| `render.dd_text_width` | code → pack: absent from section 2 |
| `schema.valid_basis` | code → pack: absent from section 2 |
| `schema.ComponentType` | code → pack: absent from section 2 |
| `schema.Provenance` | code → pack: absent from section 2 |
| `schema.Reference` | code → pack: absent from section 2 |
| `schema.DesignDataItem` | code → pack: absent from section 2 |
| `schema.DesignDataItem.citations` | code → pack: absent from section 2 |
| `schema.StackupComponent.known` | code → pack: absent from section 2 |
| `schema.StackupComponent.length_m` | code → pack: absent from section 2 |
| `schema.TitleBlock` | code → pack: absent from section 2 |
| `schema.ReferenceValue` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.is_overlay` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.stacked` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.validate` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.design_data_problems` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.design_item` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.non_finite_fields` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.component` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.to_dict` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.to_json` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.from_dict` | code → pack: absent from section 2 |
| `schema.StackupDrawingSpec.from_json` | code → pack: absent from section 2 |
| `schema.open_review_entries` | code → pack: absent from section 2 |
| `schema.not_found_fields` | code → pack: absent from section 2 |
| `workflow.StackupReconcileError` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 15. `src/digitalmodel/fatigue/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 150.

| Function | Direction |
| --- | --- |
| `assessment.HistogramCase.matching_lengths` | code → pack: absent from section 2 |
| `assessment.AssessmentInput.unique_names` | code → pack: absent from section 2 |
| `assessment.CycleResult` | code → pack: absent from section 2 |
| `assessment.CaseResult` | code → pack: absent from section 2 |
| `assessment.AssessmentResult` | code → pack: absent from section 2 |
| `assessment.assess_fatigue` | code → pack: absent from section 2 |
| `assessment_export.AssessmentArtifacts` | code → pack: absent from section 2 |
| `assessment_export.export_assessment` | code → pack: absent from section 2 |
| `c203_editions.c203_sn_table` | code → pack: absent from section 2 |
| `c203_editions.c203_label` | code → pack: absent from section 2 |
| `c203_sn_tables.BilinearClass` | code → pack: absent from section 2 |
| `c203_sn_tables.FreeCorrosionClass` | code → pack: absent from section 2 |
| `counting_contract.ResidualPolicy` | code → pack: absent from section 2 |
| `counting_contract.ContractViolation` | code → pack: absent from section 2 |
| `counting_contract.NonConservativeCountingError` | code → pack: absent from section 2 |
| `counting_contract.CycleCount` | code → pack: absent from section 2 |
| `counting_contract.CycleCount.total_counts` | code → pack: absent from section 2 |
| `counting_contract.reversals_with_endpoints` | code → pack: absent from section 2 |
| `counting_contract.peak_to_valley_span` | code → pack: absent from section 2 |
| `counting_contract.check_counting_contract` | code → pack: absent from section 2 |
| `counting_contract.assert_counting_contract` | code → pack: absent from section 2 |
| `counting_contract.check_exact_distribution` | code → pack: absent from section 2 |
| `counting_contract.assert_exact_distribution` | code → pack: absent from section 2 |
| `counting_contract.refuse_damage_calculation` | code → pack: absent from section 2 |
| `counting_contract.CountingPath` | code → pack: absent from section 2 |
| `counting_contract.CountingPath.count` | code → pack: absent from section 2 |
| `counting_contract.discover_counting_callables` | code → pack: absent from section 2 |
| `counting_contract.registered_implementations` | code → pack: absent from section 2 |
| `counting_contract.unregistered_counting_callables` | code → pack: absent from section 2 |
| `crack_growth.stress_intensity_factor` | code → pack: absent from section 2 |
| `crack_growth.paris_law_life` | code → pack: absent from section 2 |
| `crack_growth.inspection_interval` | code → pack: absent from section 2 |
| `crack_growth.crack_growth_curve_plot` | code → pack: absent from section 2 |
| `damage.miner_damage` | code → pack: absent from section 2 |
| `damage.design_life_check` | code → pack: absent from section 2 |
| `damage.thickness_correction` | code → pack: absent from section 2 |
| `environmental_correction.EnvironmentInput` | code → pack: absent from section 2 |
| `environmental_correction.EnvironmentResult` | code → pack: absent from section 2 |
| `environmental_correction.zone_factor` | code → pack: absent from section 2 |
| `environmental_correction.temperature_derating_factor` | code → pack: absent from section 2 |
| `environmental_correction.environment_correction` | code → pack: absent from section 2 |
| `environmental_correction.apply_environment_to_sn` | code → pack: absent from section 2 |
| `fatg.FatgSeaState` | code → pack: absent from section 2 |
| `fatg.FatgSeaStateResult` | code → pack: absent from section 2 |
| `fatg.FatgResult` | code → pack: absent from section 2 |
| `fatg.fatg_significant_stress_range` | code → pack: absent from section 2 |
| `fatg.fatg_heading_cycles` | code → pack: absent from section 2 |
| `fatg.fatg_sea_state_damage` | code → pack: absent from section 2 |
| `fatg.fatg_annual_damage` | code → pack: absent from section 2 |
| `fatigue_reporting.FatigueCheckLocation` | code → pack: absent from section 2 |
| `fatigue_reporting.FatigueReport` | code → pack: absent from section 2 |
| `fatigue_reporting.generate_report` | code → pack: absent from section 2 |
| `fatigue_reporting.report_to_markdown` | code → pack: absent from section 2 |
| `fatigue_reporting.report_to_dict` | code → pack: absent from section 2 |
| `fatigue_reporting.damage_barchart_data` | code → pack: absent from section 2 |
| `fatigue_reporting.inspection_recommendations` | code → pack: absent from section 2 |
| `fatigue_reporting.report_to_html` | code → pack: absent from section 2 |
| `hotspot_stress.HotspotInput` | code → pack: absent from section 2 |
| `hotspot_stress.HotspotResult` | code → pack: absent from section 2 |
| `hotspot_stress.ThroughThicknessResult` | code → pack: absent from section 2 |
| `hotspot_stress.extrapolate_hotspot_linear` | code → pack: absent from section 2 |
| `hotspot_stress.extrapolate_hotspot_quadratic` | code → pack: absent from section 2 |
| `hotspot_stress.extrapolate_hotspot` | code → pack: absent from section 2 |
| `hotspot_stress.through_thickness_linearisation` | code → pack: absent from section 2 |
| `hotspot_stress.recommended_readout_distances` | code → pack: absent from section 2 |
| `multiaxial_fatigue.StressState` | code → pack: absent from section 2 |
| `multiaxial_fatigue.MultiaxialResult` | code → pack: absent from section 2 |
| `multiaxial_fatigue.von_mises_equivalent` | code → pack: absent from section 2 |
| `multiaxial_fatigue.principal_stress_range` | code → pack: absent from section 2 |
| `multiaxial_fatigue.findley_critical_plane` | code → pack: absent from section 2 |
| `multiaxial_fatigue.shear_stress_correction` | code → pack: absent from section 2 |
| `multiaxial_fatigue.multiaxial_damage_interaction` | code → pack: absent from section 2 |
| `plotting.plot_sn_curve` | code → pack: absent from section 2 |
| `plotting.plot_damage_histogram` | code → pack: absent from section 2 |
| `rainflow.rainflow_count` | code → pack: absent from section 2 |
| `rainflow.stress_histogram` | code → pack: absent from section 2 |
| `rainflow.fatigue_life` | code → pack: absent from section 2 |
| `scf_library.TubularJointGeometry` | code → pack: absent from section 2 |
| `scf_library.TubularJointGeometry.beta` | code → pack: absent from section 2 |
| `scf_library.TubularJointGeometry.gamma` | code → pack: absent from section 2 |
| `scf_library.TubularJointGeometry.tau` | code → pack: absent from section 2 |
| `scf_library.TubularJointGeometry.alpha` | code → pack: absent from section 2 |
| `scf_library.TubularJointGeometry.zeta` | code → pack: absent from section 2 |
| `scf_library.SCFResult` | code → pack: absent from section 2 |
| `scf_library.SCFResult.model_post_init` | code → pack: absent from section 2 |
| `scf_library.short_chord_factor_f1` | code → pack: absent from section 2 |
| `scf_library.efthymiou_ty_axial` | code → pack: absent from section 2 |
| `scf_library.efthymiou_ty_ipb` | code → pack: absent from section 2 |
| `scf_library.efthymiou_ty_opb` | code → pack: absent from section 2 |
| `scf_library.efthymiou_k_axial` | code → pack: absent from section 2 |
| `scf_library.PlateGeometry` | code → pack: absent from section 2 |
| `scf_library.scf_butt_weld_misalignment` | code → pack: absent from section 2 |
| `scf_library.scf_cruciform_joint` | code → pack: absent from section 2 |
| `scf_library.scf_fillet_weld_toe` | code → pack: absent from section 2 |
| `scf_library.scf_shoulder_fillet` | code → pack: absent from section 2 |
| `scf_library.scf_circumferential_groove` | code → pack: absent from section 2 |
| `sn_curves.get_sn_curve` | code → pack: absent from section 2 |
| `sn_library.Environment` | code → pack: absent from section 2 |
| `sn_library.SNCurveRecord` | code → pack: absent from section 2 |
| `sn_library.SNCurveRecord.knee_stress` | code → pack: absent from section 2 |
| `sn_library.SNCurveRecord.switch_stress` | code → pack: absent from section 2 |
| `sn_library.SNCurveRecord.cycles` | code → pack: absent from section 2 |
| `sn_library.SNCatalog` | code → pack: absent from section 2 |
| `sn_library.SNCatalog.standards` | code → pack: absent from section 2 |
| `sn_library.SNCatalog.classes_for_standard` | code → pack: absent from section 2 |
| `sn_library.SNCatalog.filter` | code → pack: absent from section 2 |
| `sn_library.get_catalog` | code → pack: absent from section 2 |
| `sn_library.get_library_curve` | code → pack: absent from section 2 |
| `sn_library.search_curves` | code → pack: absent from section 2 |
| `sn_library.list_standards` | code → pack: absent from section 2 |
| `sn_library.curve_count` | code → pack: absent from section 2 |
| `sn_library.summary_table` | code → pack: absent from section 2 |
| `sn_library_api.CurveInfo` | code → pack: absent from section 2 |
| `sn_library_api.SNCurve` | code → pack: absent from section 2 |
| `sn_library_api.ComparisonData` | code → pack: absent from section 2 |
| `sn_library_api.list_curves` | code → pack: absent from section 2 |
| `sn_library_api.get_curve` | code → pack: absent from section 2 |
| `sn_library_api.calculate_endurance` | code → pack: absent from section 2 |
| `sn_library_api.compare_curves` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralMoments` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralMoments.rms` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralMoments.nu_p` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralMoments.nu_0` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralMoments.bandwidth_parameter` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralMoments.irregularity_factor` | code → pack: absent from section 2 |
| `spectral_fatigue.SpectralFatigueResult` | code → pack: absent from section 2 |
| `spectral_fatigue.compute_spectral_moments` | code → pack: absent from section 2 |
| `spectral_fatigue.narrow_band_damage` | code → pack: absent from section 2 |
| `spectral_fatigue.wirsching_light_damage` | code → pack: absent from section 2 |
| `spectral_fatigue.dirlik_damage` | code → pack: absent from section 2 |
| `spectral_fatigue.benasciutti_tovo_damage` | code → pack: absent from section 2 |
| `weld_classification.WeldDetail` | code → pack: absent from section 2 |
| `weld_classification.ClassificationResult` | code → pack: absent from section 2 |
| `weld_classification.classify_weld_detail` | code → pack: absent from section 2 |
| `weld_classification.list_dnv_detail_categories` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.StressBin` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.WeldFatigueResult` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.WeldFatigueResult.passed` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.weld_fatigue_quickcheck` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.run_from_config` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.result_to_csv` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.result_to_html` | code → pack: absent from section 2 |
| `weld_fatigue_quickcheck.main` | code → pack: absent from section 2 |
| `woehler_fitting.generate_test_data` | code → pack: absent from section 2 |
| `woehler_fitting.fit_woehler_curve` | code → pack: absent from section 2 |
| `woehler_fitting.design_curve` | code → pack: absent from section 2 |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 16. `src/digitalmodel/field_development/schematics/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 10.

| Function | Direction |
| --- | --- |
| `fpso_spread.FpsoSpreadSchematic` | code → pack: absent from section 2 |
| `fpso_spread.FpsoSpreadSchematic.compute_mooring_angles` | code → pack: absent from section 2 |
| `fpso_spread.FpsoSpreadSchematic.render` | code → pack: absent from section 2 |
| `platform_standalone.PlatformSchematic` | code → pack: absent from section 2 |
| `platform_standalone.PlatformSchematic.render` | code → pack: absent from section 2 |
| `renderer.render_figure` | code → pack: absent from section 2 |
| `subsea_tieback.SubseaTiebackSchematic` | code → pack: absent from section 2 |
| `subsea_tieback.SubseaTiebackSchematic.compute_template_positions` | code → pack: absent from section 2 |
| `subsea_tieback.SubseaTiebackSchematic.compute_well_positions` | code → pack: absent from section 2 |
| `subsea_tieback.SubseaTiebackSchematic.render` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@3256bab9:L1-L4` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab9:L345-L26502` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab9:L53340-L53367` | pack → code: file missing or range invalid |

### 17. `src/digitalmodel/floating_wind/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 119.

| Function | Direction |
| --- | --- |
| `collaboration.JointIndustryProject` | code → pack: absent from section 2 |
| `collaboration.CollaborationProgram` | code → pack: absent from section 2 |
| `collaboration.CollaborationProgram.total_uplift` | code → pack: absent from section 2 |
| `collaboration.CollaborationProgram.uplift_scores` | code → pack: absent from section 2 |
| `collaboration.CollaborationProgram.from_mapping` | code → pack: absent from section 2 |
| `collaboration.CollaborationProgram.from_yaml` | code → pack: absent from section 2 |
| `collaboration.apply_collaboration` | code → pack: absent from section 2 |
| `collaboration.requalify_with_collaboration` | code → pack: absent from section 2 |
| `collaboration.default_collaboration_program` | code → pack: absent from section 2 |
| `economics.CapexBreakdown` | code → pack: absent from section 2 |
| `economics.CapexBreakdown.total_per_kw` | code → pack: absent from section 2 |
| `economics.ProjectEconomics` | code → pack: absent from section 2 |
| `economics.ProjectEconomics.farm_capacity_mw` | code → pack: absent from section 2 |
| `economics.ProjectEconomics.farm_capacity_kw` | code → pack: absent from section 2 |
| `economics.ProjectEconomics.from_mapping` | code → pack: absent from section 2 |
| `economics.ProjectEconomics.from_yaml` | code → pack: absent from section 2 |
| `economics.annual_energy_production_mwh` | code → pack: absent from section 2 |
| `economics.capex_total_usd` | code → pack: absent from section 2 |
| `economics.opex_year_usd` | code → pack: absent from section 2 |
| `economics.decomex_total_usd` | code → pack: absent from section 2 |
| `economics.compute_lcoe` | code → pack: absent from section 2 |
| `economics.base_case` | code → pack: absent from section 2 |
| `economics_report.LCOEDataSheetData` | code → pack: absent from section 2 |
| `economics_report.lcoe_data_sheet` | code → pack: absent from section 2 |
| `economics_report.render_lcoe_data_sheet_html` | code → pack: absent from section 2 |
| `economics_report.write_lcoe_data_sheet` | code → pack: absent from section 2 |
| `economics_workflow.FloatingWindEconomicsWorkflow` | code → pack: absent from section 2 |
| `economics_workflow.FloatingWindEconomicsWorkflow.router` | code → pack: absent from section 2 |
| `floaters.FloaterArchetype` | code → pack: absent from section 2 |
| `floaters.TurbineTopside.total_mass_t` | code → pack: absent from section 2 |
| `floaters.TurbineTopside.cg_above_waterline_m` | code → pack: absent from section 2 |
| `floaters.SemiSubmersible.properties` | code → pack: absent from section 2 |
| `floaters.Spar.properties` | code → pack: absent from section 2 |
| `floaters.TLP.properties` | code → pack: absent from section 2 |
| `floaters.Barge.properties` | code → pack: absent from section 2 |
| `floaters.build_floater` | code → pack: absent from section 2 |
| `qualification.trl_to_maturity` | code → pack: absent from section 2 |
| `qualification.QualificationVerdict` | code → pack: absent from section 2 |
| `qualification.Criterion` | code → pack: absent from section 2 |
| `qualification.QualificationCriteria.total_weight` | code → pack: absent from section 2 |
| `qualification.QualificationCriteria.verdict_for` | code → pack: absent from section 2 |
| `qualification.QualificationCriteria.from_mapping` | code → pack: absent from section 2 |
| `qualification.QualificationCriteria.from_yaml` | code → pack: absent from section 2 |
| `qualification.QualificationResult` | code → pack: absent from section 2 |
| `qualification.score_concept` | code → pack: absent from section 2 |
| `qualification.rank_concepts` | code → pack: absent from section 2 |
| `qualification.default_criteria` | code → pack: absent from section 2 |
| `reliability.ReliabilityScenario.opex_multiplier` | code → pack: absent from section 2 |
| `reliability.ReliabilityScenario.availability_gain` | code → pack: absent from section 2 |
| `reliability.apply_reliability` | code → pack: absent from section 2 |
| `reliability.lcoe_with_reliability` | code → pack: absent from section 2 |
| `report.ConceptProvenance` | code → pack: absent from section 2 |
| `report.ConceptDataSheetData` | code → pack: absent from section 2 |
| `report.concept_data_sheet` | code → pack: absent from section 2 |
| `report.render_concept_data_sheet_html` | code → pack: absent from section 2 |
| `report.write_concept_data_sheet` | code → pack: absent from section 2 |
| `scaling.ComponentScaling.per_mw` | code → pack: absent from section 2 |
| `scaling.EconomiesOfScale.from_mapping` | code → pack: absent from section 2 |
| `scaling.EconomiesOfScale.from_yaml` | code → pack: absent from section 2 |
| `scaling.scale_economics` | code → pack: absent from section 2 |
| `scaling.lcoe_at_scale` | code → pack: absent from section 2 |
| `scaling.default_scaling` | code → pack: absent from section 2 |
| `screening.VariantScreening` | code → pack: absent from section 2 |
| `screening.ScreeningResults` | code → pack: absent from section 2 |
| `screening.ScreeningResults.passing` | code → pack: absent from section 2 |
| `screening.ScreeningResults.to_rows` | code → pack: absent from section 2 |
| `screening.screen_concept` | code → pack: absent from section 2 |
| `screening.screen_concepts` | code → pack: absent from section 2 |
| `solver_tier.SolverCase` | code → pack: absent from section 2 |
| `solver_tier.box_gdf` | code → pack: absent from section 2 |
| `solver_tier.cylinder_gdf` | code → pack: absent from section 2 |
| `solver_tier.build_panel_mesh` | code → pack: absent from section 2 |
| `solver_tier.build_orcaflex_motion_config` | code → pack: absent from section 2 |
| `solver_tier.prepare_variant` | code → pack: absent from section 2 |
| `solver_tier.prepare_shortlist` | code → pack: absent from section 2 |
| `solver_tier.dispatch_case` | code → pack: absent from section 2 |
| `solver_tier.fold_rao_results` | code → pack: absent from section 2 |
| `standardization.LearningCurve` | code → pack: absent from section 2 |
| `standardization.LearningCurve.factor` | code → pack: absent from section 2 |
| `standardization.StandardizationDiscount` | code → pack: absent from section 2 |
| `standardization.StandardizationDiscount.factor` | code → pack: absent from section 2 |
| `standardization.apply_standardization` | code → pack: absent from section 2 |
| `standardization.lcoe_with_standardization` | code → pack: absent from section 2 |
| `standardization.default_learning_curve` | code → pack: absent from section 2 |
| `sweep.LeverChange` | code → pack: absent from section 2 |
| `sweep.DriverScenario` | code → pack: absent from section 2 |
| `sweep.SweepRow` | code → pack: absent from section 2 |
| `sweep.apply_change` | code → pack: absent from section 2 |
| `sweep.run_sweep` | code → pack: absent from section 2 |
| `tradespace.Objective` | code → pack: absent from section 2 |
| `tradespace.variant_metrics` | code → pack: absent from section 2 |
| `tradespace.metric_records` | code → pack: absent from section 2 |
| `tradespace.pareto_front` | code → pack: absent from section 2 |
| `tradespace.correlation_map` | code → pack: absent from section 2 |
| `tradespace_economics.variant_lcoe` | code → pack: absent from section 2 |
| `tradespace_economics.mean_steel_mass_t` | code → pack: absent from section 2 |
| `tradespace_economics.lcoe_records` | code → pack: absent from section 2 |
| `tradespace_economics.pareto_front_with_lcoe` | code → pack: absent from section 2 |
| `workflow.FloatingWindSizingWorkflow` | code → pack: absent from section 2 |
| `workflow.FloatingWindSizingWorkflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 18. `src/digitalmodel/geotechnical/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 73.

| Function | Direction |
| --- | --- |
| `anchors.DragAnchorResult` | code → pack: absent from section 2 |
| `anchors.SuctionAnchorResult` | code → pack: absent from section 2 |
| `anchors.drag_anchor_capacity` | code → pack: absent from section 2 |
| `anchors.suction_anchor_capacity` | code → pack: absent from section 2 |
| `cfg_workflows.AnchorCapacityWorkflow` | code → pack: absent from section 2 |
| `cfg_workflows.AnchorCapacityWorkflow.router` | code → pack: absent from section 2 |
| `cfg_workflows.PileCapacityWorkflow` | code → pack: absent from section 2 |
| `cfg_workflows.PileCapacityWorkflow.router` | code → pack: absent from section 2 |
| `cfg_workflows.ScourWorkflow` | code → pack: absent from section 2 |
| `cfg_workflows.ScourWorkflow.router` | code → pack: absent from section 2 |
| `cfg_workflows.MudmatBearingCapacityWorkflow` | code → pack: absent from section 2 |
| `cfg_workflows.MudmatBearingCapacityWorkflow.router` | code → pack: absent from section 2 |
| `cfg_workflows.LiquefactionWorkflow` | code → pack: absent from section 2 |
| `cfg_workflows.LiquefactionWorkflow.router` | code → pack: absent from section 2 |
| `database.Database.lookup` | code → pack: absent from section 2 |
| `database.Database.quantities_for` | code → pack: absent from section 2 |
| `database.load_database` | code → pack: absent from section 2 |
| `database_schema.freeze` | code → pack: absent from section 2 |
| `database_schema.Source.from_entry` | code → pack: absent from section 2 |
| `database_schema.ParameterRow` | code → pack: absent from section 2 |
| `database_schema.ParameterRow.from_entry` | code → pack: absent from section 2 |
| `database_schema.UnitValue` | code → pack: absent from section 2 |
| `database_schema.Quantity` | code → pack: absent from section 2 |
| `database_schema.Quantity.parent_row_id` | code → pack: absent from section 2 |
| `database_schema.Quantity.source` | code → pack: absent from section 2 |
| `database_schema.Quantity.in_printed_units` | code → pack: absent from section 2 |
| `database_schema.Quantity.in_si` | code → pack: absent from section 2 |
| `liquefaction.LayerResult` | code → pack: absent from section 2 |
| `liquefaction.LiquefactionResult` | code → pack: absent from section 2 |
| `liquefaction.stress_reduction_factor` | code → pack: absent from section 2 |
| `liquefaction.magnitude_scaling_factor` | code → pack: absent from section 2 |
| `liquefaction.cyclic_stress_ratio` | code → pack: absent from section 2 |
| `liquefaction.cyclic_resistance_ratio_75` | code → pack: absent from section 2 |
| `liquefaction.assess_liquefaction` | code → pack: absent from section 2 |
| `mudmat.BearingCapacityResult` | code → pack: absent from section 2 |
| `mudmat.bearing_capacity_factors` | code → pack: absent from section 2 |
| `mudmat.mudmat_bearing_capacity` | code → pack: absent from section 2 |
| `pile_capacity.PileCapacityResult` | code → pack: absent from section 2 |
| `pile_capacity.alpha_method_capacity` | code → pack: absent from section 2 |
| `pile_capacity.alpha_method_capacity_multilayer` | code → pack: absent from section 2 |
| `piles.validate_pile_inputs` | code → pack: absent from section 2 |
| `piles.SkinFrictionClayResult` | code → pack: absent from section 2 |
| `piles.SkinFrictionSandResult` | code → pack: absent from section 2 |
| `piles.EndBearingResult` | code → pack: absent from section 2 |
| `piles.AxialCapacityResult` | code → pack: absent from section 2 |
| `piles.skin_friction_clay` | code → pack: absent from section 2 |
| `piles.skin_friction_sand` | code → pack: absent from section 2 |
| `piles.end_bearing_clay` | code → pack: absent from section 2 |
| `piles.end_bearing_sand` | code → pack: absent from section 2 |
| `scour.PipelineScourResult` | code → pack: absent from section 2 |
| `scour.MonopileScourResult` | code → pack: absent from section 2 |
| `scour.RockArmourResult` | code → pack: absent from section 2 |
| `scour.CriticalSedimentMotionResult` | code → pack: absent from section 2 |
| `scour.critical_sediment_motion` | code → pack: absent from section 2 |
| `scour.pipeline_scour_depth` | code → pack: absent from section 2 |
| `scour.monopile_scour_depth` | code → pack: absent from section 2 |
| `scour.rock_armour_thickness` | code → pack: absent from section 2 |
| `slope_profile.validate_profile` | code → pack: absent from section 2 |
| `slope_stability.MethodResult` | code → pack: absent from section 2 |
| `slope_stability.SeepageResult` | code → pack: absent from section 2 |
| `slope_stability.SlopeResult` | code → pack: absent from section 2 |
| `slope_stability.build_slope_model` | code → pack: absent from section 2 |
| `slope_stability.run_seepage` | code → pack: absent from section 2 |
| `slope_stability.analyze_model` | code → pack: absent from section 2 |
| `slope_stability.analyze_profile` | code → pack: absent from section 2 |
| `slope_workflow.SlopeStabilityWorkflow` | code → pack: absent from section 2 |
| `slope_workflow.SlopeStabilityWorkflow.router` | code → pack: absent from section 2 |
| `workflow.GeotechnicalWorkflow` | code → pack: absent from section 2 |
| `workflow.GeotechnicalWorkflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 19. `src/digitalmodel/hull_girder_screening/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 1.

| Function | Direction |
| --- | --- |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 20. `src/digitalmodel/hydrodynamics/aqwa/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 165.

| Function | Direction |
| --- | --- |
| `aqwa_analysis_damping.AqwaDamping` | code → pack: absent from section 2 |
| `aqwa_analysis_damping.AqwaDamping.router` | code → pack: absent from section 2 |
| `aqwa_analysis_damping.AqwaDamping.run_all_files` | code → pack: absent from section 2 |
| `aqwa_analysis_damping.AqwaDamping.compute_heading_rms` | code → pack: absent from section 2 |
| `aqwa_analysis_damping.AqwaDamping.derive_viscous_damping_statistical` | code → pack: absent from section 2 |
| `aqwa_analysis_damping.AqwaDamping.combine_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_damping.AqwaDamping.compute_critical_damping_ratios` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.ef_server_router` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.define_result_df` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.save_result_df` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.UF1` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.UF2` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.UF3` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.UFFender` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.get_ramp_force` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.get_ramp_factor` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.uf_wlng_fst` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.assign_results_to_df` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.wsp_dampener_by_def_pos` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.get_numerical_added_mass` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.get_wsp_dampener_force` | code → pack: absent from section 2 |
| `aqwa_analysis_ef_server.AqwaEFServer.print_heartbeat` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.dm_engine` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.rao_router` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.split_dat_to_decks` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.prepare_decks` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.prepare_no_damping_runs` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_SplitToDeck` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_prepare_decks` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_no_damping_runs` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_damping_runs` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.create_decks_directory` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.run_raos` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.derive_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_natural_period_from_lis_file` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_no_of_frequencies` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_directions_labels` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_percent_critical_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_absolute_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_natural_period` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_critical_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_absolute_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_template_raos` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.prepare_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.prepare_damping_runs` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.prepare_damping_decks` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_natural_periods` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_damping_values` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_additional_damping_by_cfg` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_additional_damping` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_raos` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.get_raos_by_cfg` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.plot_raos` | code → pack: absent from section 2 |
| `aqwa_analysis_raos.AqwaRAOs.plot_raos_by_cfg` | code → pack: absent from section 2 |
| `aqwa_cli.create_parser` | code → pack: absent from section 2 |
| `aqwa_cli.list_methods` | code → pack: absent from section 2 |
| `aqwa_cli.load_config_file` | code → pack: absent from section 2 |
| `aqwa_cli.build_config_from_args` | code → pack: absent from section 2 |
| `aqwa_cli.run_analysis` | code → pack: absent from section 2 |
| `aqwa_cli.main` | code → pack: absent from section 2 |
| `aqwa_dat_files.DATFrequencyGrid` | code → pack: absent from section 2 |
| `aqwa_dat_files.DATFrequencyGrid.as_array` | code → pack: absent from section 2 |
| `aqwa_dat_files.DATDragMatrix` | code → pack: absent from section 2 |
| `aqwa_dat_files.DATDeckOneMetadata` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.process_all_data_categories` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_data` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_header` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_footer` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_01_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_03_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_04_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_06_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_07_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_10_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_direction_data` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_WFC_data` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_CFC_data` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_11_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_15_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_15_body_generic` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_15_wlng` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.get_dc_18_body` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.inspect_dat_file` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.validate_ldrg_option` | code → pack: absent from section 2 |
| `aqwa_dat_files.AqwaDATFiles.validate_cog_element` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.get_data` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.get_refine_data_using_cfg` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.get_data_for_refine_data_line_number` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.get_header_columns` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.get_sorted_dataframe` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.get_output_transformed_data` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.transform_by_data_key` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.save_to_csv` | code → pack: absent from section 2 |
| `aqwa_lis_files.AqwaLISFiles.injectSummaryToExcel` | code → pack: absent from section 2 |
| `aqwa_post_process.AqwaPostProcess` | code → pack: absent from section 2 |
| `aqwa_post_process.AqwaPostProcess.post_process_router` | code → pack: absent from section 2 |
| `aqwa_post_process.AqwaPostProcess.get_visualizations` | code → pack: absent from section 2 |
| `aqwa_pre_process.AqwaPreProcess` | code → pack: absent from section 2 |
| `aqwa_pre_process.AqwaPreProcess.pre_process_router` | code → pack: absent from section 2 |
| `aqwa_pre_process.AqwaPreProcess.get_cfg_with_master_data` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.postprocess` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.get_result_groups` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.get_result_group` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.get_change_of_data_result_group` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.get_args_for_data` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.get_property_args_array` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.process_equilibrium_result` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.process_frequency_result` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.process_timeresponse_result` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.inject_to_excel` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.save_to_csv` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.save_statistics` | code → pack: absent from section 2 |
| `aqwa_reader.AqwaReader.update_iteration_item_with_master_settings` | code → pack: absent from section 2 |
| `aqwa_router.Aqwa.get_cfg_with_master_data` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities.is_license_available` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities.get_aqwareader_exe` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities.get_workbench_bat` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities.get_aqwa_exe` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities.run_aqwa_analysis_as_process` | code → pack: absent from section 2 |
| `aqwa_utilities.AqwaUtilities.run_aqwa_analysis_as_subprocess` | code → pack: absent from section 2 |
| `damping_physics.citation_sidecar` | code → pack: absent from section 2 |
| `damping_physics.positive` | code → pack: absent from section 2 |
| `damping_physics.frequency_grid` | code → pack: absent from section 2 |
| `damping_physics.motion_amplitudes` | code → pack: absent from section 2 |
| `damping_physics.resonance` | code → pack: absent from section 2 |
| `damping_physics.critical_dof` | code → pack: absent from section 2 |
| `dat_frequency.parse_frequency_records` | code → pack: absent from section 2 |
| `dat_frequency.parse_frequency_grids` | code → pack: absent from section 2 |
| `dat_frequency.record_values` | code → pack: absent from section 2 |
| `mes_files.MesFiles.read_mes_files` | code → pack: absent from section 2 |
| `mes_files.MesFiles.assign_ids` | code → pack: absent from section 2 |
| `mes_files.MesFiles.summarize_warnings_and_errors` | code → pack: absent from section 2 |
| `mes_files.MesFiles.generate_file_status_table` | code → pack: absent from section 2 |
| `mes_files.MesFiles.to_dataframe` | code → pack: absent from section 2 |
| `mes_files.MesFiles.merge_cells` | code → pack: absent from section 2 |
| `mes_files.MesFiles.generate_id_file_matrix` | code → pack: absent from section 2 |
| `qa_gates.validate_external_gate` | code → pack: absent from section 2 |
| `qa_gates.validate_input_binding` | code → pack: absent from section 2 |
| `qa_gates.validate_native_result` | code → pack: absent from section 2 |
| `qa_gates.input_manifest` | code → pack: absent from section 2 |
| `qa_gates.verify_manifest` | code → pack: absent from section 2 |
| `qa_gates.evidence_digest` | code → pack: absent from section 2 |
| `qa_gates.validate_signature` | code → pack: absent from section 2 |
| `qa_gates.validate_report_gates` | code → pack: absent from section 2 |
| `qa_gates.record_gates` | code → pack: absent from section 2 |
| `qa_gates.write_report_evidence` | code → pack: absent from section 2 |
| `viscous_damping_determination.ViscousDampingDetermination` | code → pack: absent from section 2 |
| `viscous_damping_determination.ViscousDampingDetermination.run` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L1-L260` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L202-L260` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L224-L244` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_analysis_damping.py:L61-L260` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L1-L731` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L564-L731` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L595-L731` | pack → code: file missing or range invalid |
| `src/digitalmodel/hydrodynamics/aqwa/aqwa_dat_files.py:L706-L724` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L127-L151` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L132-L140` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L143-L151` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L17-L99` | pack → code: file missing or range invalid |

### 21. `src/digitalmodel/hydrodynamics/bemrosetta/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 9.

| Function | Direction |
| --- | --- |
| `__init__.get_module_info` | code → pack: absent from section 2 |
| `cli.convert` | code → pack: absent from section 2 |
| `cli.info` | code → pack: absent from section 2 |
| `cli.validate` | code → pack: absent from section 2 |
| `cli.convert_mesh_cmd` | code → pack: absent from section 2 |
| `cli.validate_mesh` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L1-L4` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10660-L10671` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10672-L10687` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10688-L10699` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10700-L10713` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10714-L10725` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10726-L10737` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10738-L10749` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10750-L10763` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10764-L10777` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L10778-L10789` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L13220-L13556` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L13307-L13316` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L4921-L9496` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L6202-L6211` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@7d15df7734aaa1e3e74035789661abd37d866597:L9497-L13219` | pack → code: file missing or range invalid |

### 22. `src/digitalmodel/hydrodynamics/diffraction/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 555.

| Function | Direction |
| --- | --- |
| `analytical_cylinder.CylinderGeometry` | code → pack: absent from section 2 |
| `analytical_cylinder.CylinderGeometry.gap` | code → pack: absent from section 2 |
| `analytical_cylinder.CylinderGeometry.displaced_volume` | code → pack: absent from section 2 |
| `analytical_cylinder.HeaveCoefficients` | code → pack: absent from section 2 |
| `analytical_cylinder.HeaveCoefficients.period` | code → pack: absent from section 2 |
| `analytical_cylinder.wavenumbers` | code → pack: absent from section 2 |
| `analytical_cylinder.heave_added_mass_damping` | code → pack: absent from section 2 |
| `aqwa_ah1_parser.AH1ParseResult` | code → pack: absent from section 2 |
| `aqwa_ah1_parser.parse_ah1` | code → pack: absent from section 2 |
| `aqwa_backend.ParsedMesh` | code → pack: absent from section 2 |
| `aqwa_backend.ParsedMesh.n_vertices` | code → pack: absent from section 2 |
| `aqwa_backend.ParsedMesh.n_panels` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.generate_single` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.generate_modular` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck0` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck1` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck2` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck3` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck4` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck5` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck7` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck8` | code → pack: absent from section 2 |
| `aqwa_backend.AQWABackend.build_deck6` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWAExecutionMode` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchJobConfig` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchConfig` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchConfig.from_yaml` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchJobResult` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchReport` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchRunner` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchRunner.run` | code → pack: absent from section 2 |
| `aqwa_batch_runner.AQWABatchRunner.export_report` | code → pack: absent from section 2 |
| `aqwa_batch_runner.run_aqwa_batch` | code → pack: absent from section 2 |
| `aqwa_batch_runner.run_aqwa_batch_from_specs` | code → pack: absent from section 2 |
| `aqwa_converter.AQWAConverter` | code → pack: absent from section 2 |
| `aqwa_converter.AQWAConverter.convert_to_unified_schema` | code → pack: absent from section 2 |
| `aqwa_converter.convert_aqwa_results` | code → pack: absent from section 2 |
| `aqwa_deck_basis.AqwaBasis` | code → pack: absent from section 2 |
| `aqwa_deck_basis.AqwaBasis.radii_of_gyration` | code → pack: absent from section 2 |
| `aqwa_deck_basis.AqwaBasis.displaced_volume_if_balanced` | code → pack: absent from section 2 |
| `aqwa_deck_basis.AqwaBasis.has_additional_damping` | code → pack: absent from section 2 |
| `aqwa_deck_basis.AqwaBasis.describe` | code → pack: absent from section 2 |
| `aqwa_deck_basis.AmbiguousDeck` | code → pack: absent from section 2 |
| `aqwa_deck_basis.structures_in_deck` | code → pack: absent from section 2 |
| `aqwa_deck_basis.read_aqwa_basis` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.find_section` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.extract_frequencies_and_periods` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.extract_headings` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.parse_added_mass_table` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.parse_damping_table` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.parse_rao_table` | code → pack: absent from section 2 |
| `aqwa_lis_parser.AQWALISParser.parse_all` | code → pack: absent from section 2 |
| `aqwa_lis_parser.parse_aqwa_lis_file` | code → pack: absent from section 2 |
| `aqwa_lis_parser.extract_aqwa_frequencies` | code → pack: absent from section 2 |
| `aqwa_lis_parser.extract_aqwa_headings` | code → pack: absent from section 2 |
| `aqwa_lis_parser.extract_aqwa_added_mass` | code → pack: absent from section 2 |
| `aqwa_lis_parser.extract_aqwa_damping` | code → pack: absent from section 2 |
| `aqwa_lis_parser.extract_aqwa_raos` | code → pack: absent from section 2 |
| `aqwa_result_extractor.AQWAExtractionResult` | code → pack: absent from section 2 |
| `aqwa_result_extractor.AQWAResultExtractor` | code → pack: absent from section 2 |
| `aqwa_result_extractor.AQWAResultExtractor.extract` | code → pack: absent from section 2 |
| `aqwa_result_extractor.AQWAResultExtractor.find_lis_file` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunStatus` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunConfig` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunConfig.coerce_executable_path` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunConfig.coerce_output_dir` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunResult` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunner` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunner.result` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunner.run` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunner.prepare` | code → pack: absent from section 2 |
| `aqwa_runner.AQWARunner.execute` | code → pack: absent from section 2 |
| `aqwa_runner.run_aqwa` | code → pack: absent from section 2 |
| `batch_processor.BatchConfiguration` | code → pack: absent from section 2 |
| `batch_processor.BatchResult` | code → pack: absent from section 2 |
| `batch_processor.BatchReport` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor.process_configuration` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor.run` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor.export_report` | code → pack: absent from section 2 |
| `batch_processor.process_batch_from_config_file` | code → pack: absent from section 2 |
| `benchmark_abscissa.AbscissaOrderError` | code → pack: absent from section 2 |
| `benchmark_abscissa.AbscissaOverlapError` | code → pack: absent from section 2 |
| `benchmark_abscissa.AbscissaGapError` | code → pack: absent from section 2 |
| `benchmark_abscissa.AbscissaConfig` | code → pack: absent from section 2 |
| `benchmark_abscissa.OrderedSolverData` | code → pack: absent from section 2 |
| `benchmark_abscissa.ComplexResponse` | code → pack: absent from section 2 |
| `benchmark_abscissa.AlignedResponses` | code → pack: absent from section 2 |
| `benchmark_abscissa.InsufficientSampling` | code → pack: absent from section 2 |
| `benchmark_abscissa.order_solver_data` | code → pack: absent from section 2 |
| `benchmark_abscissa.build_evaluation_grid` | code → pack: absent from section 2 |
| `benchmark_abscissa.interpolate_complex_response` | code → pack: absent from section 2 |
| `benchmark_abscissa.assess_sampling` | code → pack: absent from section 2 |
| `benchmark_abscissa.align_responses` | code → pack: absent from section 2 |
| `benchmark_correlation.plot_pairwise_correlation_heatmap` | code → pack: absent from section 2 |
| `benchmark_correlation.render_6x6_matrix` | code → pack: absent from section 2 |
| `benchmark_correlation.build_hydro_coefficients_html` | code → pack: absent from section 2 |
| `benchmark_correlation.build_coupling_heatmap_html` | code → pack: absent from section 2 |
| `benchmark_correlation.build_raw_rao_data_html` | code → pack: absent from section 2 |
| `benchmark_dof_sections.build_dof_report_sections` | code → pack: absent from section 2 |
| `benchmark_helpers.optional_float` | code → pack: absent from section 2 |
| `benchmark_helpers.optional_round` | code → pack: absent from section 2 |
| `benchmark_helpers.is_refusal_quality` | code → pack: absent from section 2 |
| `benchmark_helpers.format_absence` | code → pack: absent from section 2 |
| `benchmark_helpers.format_quality_distribution` | code → pack: absent from section 2 |
| `benchmark_helpers.format_optional_correlation` | code → pack: absent from section 2 |
| `benchmark_helpers.coefficient_coverage` | code → pack: absent from section 2 |
| `benchmark_helpers.format_coverage_summary` | code → pack: absent from section 2 |
| `benchmark_helpers.generate_dof_observations` | code → pack: absent from section 2 |
| `benchmark_input_comparison.summarize_semantic_equivalence` | code → pack: absent from section 2 |
| `benchmark_input_comparison.build_input_comparison_html` | code → pack: absent from section 2 |
| `benchmark_input_comparison.build_semantic_equivalence_html` | code → pack: absent from section 2 |
| `benchmark_input_files.build_input_files_html` | code → pack: absent from section 2 |
| `benchmark_matrix_alignment.AlignedMatrices` | code → pack: absent from section 2 |
| `benchmark_matrix_alignment.align_matrix_sets` | code → pack: absent from section 2 |
| `benchmark_mesh_schematic.build_mesh_schematic_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_amplitude_overlay` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_phase_overlay` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_combined_overlay` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_difference` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_all` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_per_dof` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.plot_pairwise_correlation_heatmap` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_hydro_coefficients_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_coupling_heatmap_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_raw_rao_data_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_dof_report_sections` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_input_comparison_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_input_files_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_mesh_schematic_html` | code → pack: absent from section 2 |
| `benchmark_plotter.BenchmarkPlotter.build_semantic_equivalence_html` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.get_solver_style` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.get_x_values` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.x_axis_label` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.get_heading_indices` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.get_significant_heading_indices` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.add_solver_traces` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.apply_layout` | code → pack: absent from section 2 |
| `benchmark_rao_helpers.save_figure` | code → pack: absent from section 2 |
| `benchmark_rao_plots.plot_amplitude_overlay` | code → pack: absent from section 2 |
| `benchmark_rao_plots.plot_phase_overlay` | code → pack: absent from section 2 |
| `benchmark_rao_plots.plot_combined_overlay` | code → pack: absent from section 2 |
| `benchmark_rao_plots.plot_difference` | code → pack: absent from section 2 |
| `benchmark_rao_plots.plot_per_dof` | code → pack: absent from section 2 |
| `benchmark_rao_summary.compute_amplitude_summary` | code → pack: absent from section 2 |
| `benchmark_rao_summary.compute_phase_summary` | code → pack: absent from section 2 |
| `benchmark_rao_summary.build_summary_table` | code → pack: absent from section 2 |
| `benchmark_rao_summary.render_html_with_table` | code → pack: absent from section 2 |
| `benchmark_runner.PinnedNullDumper` | code → pack: absent from section 2 |
| `benchmark_runner.SolverType` | code → pack: absent from section 2 |
| `benchmark_runner.BenchmarkConfig` | code → pack: absent from section 2 |
| `benchmark_runner.BenchmarkRunResult` | code → pack: absent from section 2 |
| `benchmark_runner.BenchmarkRunner` | code → pack: absent from section 2 |
| `benchmark_runner.BenchmarkRunner.run_from_results` | code → pack: absent from section 2 |
| `benchmark_runner.run_benchmark` | code → pack: absent from section 2 |
| `benchmark_runner.benchmark_solvers_cmd` | code → pack: absent from section 2 |
| `benchmark_verdict.BenchmarkVerdict` | code → pack: absent from section 2 |
| `benchmark_verdict.derive_status` | code → pack: absent from section 2 |
| `cli.cli` | code → pack: absent from section 2 |
| `cli.convert_aqwa` | code → pack: absent from section 2 |
| `cli.convert_orcawave` | code → pack: absent from section 2 |
| `cli.compare` | code → pack: absent from section 2 |
| `cli.batch` | code → pack: absent from section 2 |
| `cli.convert_spec` | code → pack: absent from section 2 |
| `cli.validate_spec` | code → pack: absent from section 2 |
| `cli.resolve_cmd` | code → pack: absent from section 2 |
| `cli.orcawave_doctor_cmd` | code → pack: absent from section 2 |
| `cli.run_orcawave_cmd` | code → pack: absent from section 2 |
| `cli.run_aqwa_cmd` | code → pack: absent from section 2 |
| `cli.batch_aqwa_cmd` | code → pack: absent from section 2 |
| `cli.batch_orcawave_cmd` | code → pack: absent from section 2 |
| `cli.plot_raos_cmd` | code → pack: absent from section 2 |
| `cli.mesh_build` | code → pack: absent from section 2 |
| `comparison_framework.DeviationStatistics` | code → pack: absent from section 2 |
| `comparison_framework.RAOComparison` | code → pack: absent from section 2 |
| `comparison_framework.MatrixComparison` | code → pack: absent from section 2 |
| `comparison_framework.ComparisonReport` | code → pack: absent from section 2 |
| `comparison_framework.DiffractionComparator` | code → pack: absent from section 2 |
| `comparison_framework.DiffractionComparator.compare_raos` | code → pack: absent from section 2 |
| `comparison_framework.DiffractionComparator.compare_added_mass` | code → pack: absent from section 2 |
| `comparison_framework.DiffractionComparator.compare_damping` | code → pack: absent from section 2 |
| `comparison_framework.DiffractionComparator.generate_report` | code → pack: absent from section 2 |
| `comparison_framework.DiffractionComparator.export_report` | code → pack: absent from section 2 |
| `comparison_framework.compare_diffraction_results` | code → pack: absent from section 2 |
| `cross_solver_acceptance.Category` | code → pack: absent from section 2 |
| `cross_solver_acceptance.Verdict` | code → pack: absent from section 2 |
| `cross_solver_acceptance.Criterion` | code → pack: absent from section 2 |
| `cross_solver_acceptance.category_for` | code → pack: absent from section 2 |
| `cross_solver_acceptance.ConvergenceEvidence` | code → pack: absent from section 2 |
| `cross_solver_acceptance.convergence_from_self_study` | code → pack: absent from section 2 |
| `cross_solver_acceptance.Evaluation` | code → pack: absent from section 2 |
| `cross_solver_acceptance.Evaluation.margin` | code → pack: absent from section 2 |
| `cross_solver_acceptance.evaluate` | code → pack: absent from section 2 |
| `deck_validation.validate_mesh` | code → pack: absent from section 2 |
| `diagnostic_basis.canonical_decimal` | code → pack: absent from section 2 |
| `diagnostic_basis.unique_pairs` | code → pack: absent from section 2 |
| `diagnostic_basis.reject_constant` | code → pack: absent from section 2 |
| `diagnostic_basis.strict_json` | code → pack: absent from section 2 |
| `diagnostic_basis.canonical_intent` | code → pack: absent from section 2 |
| `diagnostic_basis.roundtrip_equal` | code → pack: absent from section 2 |
| `diagnostic_basis.prescribed_basis` | code → pack: absent from section 2 |
| `diagnostic_inputs.divergence_ledger` | code → pack: absent from section 2 |
| `diagnostic_inputs.build_bundle` | code → pack: absent from section 2 |
| `diagnostic_inputs.native_yaml` | code → pack: absent from section 2 |
| `diagnostic_mesh.transform_mesh` | code → pack: absent from section 2 |
| `diffraction_cli.create_parser` | code → pack: absent from section 2 |
| `diffraction_cli.list_tools` | code → pack: absent from section 2 |
| `diffraction_cli.run_aqwa` | code → pack: absent from section 2 |
| `diffraction_cli.run_orcawave` | code → pack: absent from section 2 |
| `diffraction_cli.main` | code → pack: absent from section 2 |
| `diffraction_units.kg_to_tonnes` | code → pack: absent from section 2 |
| `diffraction_units.tonnes_to_kg` | code → pack: absent from section 2 |
| `diffraction_units.density_kg_m3_to_t_m3` | code → pack: absent from section 2 |
| `diffraction_units.density_t_m3_to_kg_m3` | code → pack: absent from section 2 |
| `diffraction_units.inertia_kg_m2_to_t_m2` | code → pack: absent from section 2 |
| `diffraction_units.inertia_t_m2_to_kg_m2` | code → pack: absent from section 2 |
| `diffraction_units.hz_to_rad_per_s` | code → pack: absent from section 2 |
| `diffraction_units.rad_per_s_to_hz` | code → pack: absent from section 2 |
| `diffraction_units.rad_per_s_to_period_s` | code → pack: absent from section 2 |
| `diffraction_units.period_s_to_rad_per_s` | code → pack: absent from section 2 |
| `diffraction_units.radians_to_degrees` | code → pack: absent from section 2 |
| `diffraction_units.degrees_to_radians` | code → pack: absent from section 2 |
| `diffraction_units.complex_phase_degrees` | code → pack: absent from section 2 |
| `equilibrium_checks.check_cog_cob` | code → pack: absent from section 2 |
| `equilibrium_checks.check_mass_displacement` | code → pack: absent from section 2 |
| `equilibrium_checks.check_force_balance` | code → pack: absent from section 2 |
| `fleet_decks.FleetResult` | code → pack: absent from section 2 |
| `fleet_decks.FleetResult.n_built` | code → pack: absent from section 2 |
| `fleet_decks.FleetResult.n_parametric` | code → pack: absent from section 2 |
| `fleet_decks.FleetResult.n_placeholder` | code → pack: absent from section 2 |
| `fleet_decks.iter_fleet_records` | code → pack: absent from section 2 |
| `fleet_decks.generate_fleet_decks` | code → pack: absent from section 2 |
| `fleet_decks.emit_orcawave_batch_config` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityReport` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.check_watertightness` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.check_normals` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.check_panel_count` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.check_aspect_ratios` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.check_element_sizes` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.generate_report` | code → pack: absent from section 2 |
| `geometry_quality.GeometryQualityChecker.export_report` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.GmshMeshSpec` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.GmshMeshSpec.validate_geometry_type` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.GmshMeshSpec.validate_for_geometry` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.GmshMeshSpec.from_dict` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.MeshExportResult` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.MeshExportResult.to_dict` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.GmshMeshBuilder` | code → pack: absent from section 2 |
| `gmsh_mesh_builder.GmshMeshBuilder.build` | code → pack: absent from section 2 |
| `input_schemas.AnalysisType` | code → pack: absent from section 2 |
| `input_schemas.MeshFormatType` | code → pack: absent from section 2 |
| `input_schemas.SymmetryType` | code → pack: absent from section 2 |
| `input_schemas.FrequencyInputType` | code → pack: absent from section 2 |
| `input_schemas.FrequencyDistribution` | code → pack: absent from section 2 |
| `input_schemas.LoadRAOMethod` | code → pack: absent from section 2 |
| `input_schemas.SolverPrecision` | code → pack: absent from section 2 |
| `input_schemas.OutputFormat` | code → pack: absent from section 2 |
| `input_schemas.OutputComponent` | code → pack: absent from section 2 |
| `input_schemas.VesselGeometry` | code → pack: absent from section 2 |
| `input_schemas.VesselGeometry.validate_reference_point` | code → pack: absent from section 2 |
| `input_schemas.VesselInertia` | code → pack: absent from section 2 |
| `input_schemas.VesselInertia.validate_cog` | code → pack: absent from section 2 |
| `input_schemas.VesselInertia.validate_rog` | code → pack: absent from section 2 |
| `input_schemas.VesselInertia.check_inertia_specified` | code → pack: absent from section 2 |
| `input_schemas.VesselSpec` | code → pack: absent from section 2 |
| `input_schemas.EnvironmentSpec` | code → pack: absent from section 2 |
| `input_schemas.EnvironmentSpec.validate_water_depth` | code → pack: absent from section 2 |
| `input_schemas.EnvironmentSpec.validate_water_density` | code → pack: absent from section 2 |
| `input_schemas.FrequencyRangeSpec` | code → pack: absent from section 2 |
| `input_schemas.FrequencySpec` | code → pack: absent from section 2 |
| `input_schemas.FrequencySpec.check_values_or_range` | code → pack: absent from section 2 |
| `input_schemas.FrequencySpec.to_frequencies_rad_s` | code → pack: absent from section 2 |
| `input_schemas.FrequencySpec.to_periods_s` | code → pack: absent from section 2 |
| `input_schemas.HeadingRangeSpec` | code → pack: absent from section 2 |
| `input_schemas.WaveHeadingSpec` | code → pack: absent from section 2 |
| `input_schemas.WaveHeadingSpec.check_values_or_range` | code → pack: absent from section 2 |
| `input_schemas.WaveHeadingSpec.to_heading_list` | code → pack: absent from section 2 |
| `input_schemas.IrregularFrequencyMethod` | code → pack: absent from section 2 |
| `input_schemas.QTFOptions` | code → pack: absent from section 2 |
| `input_schemas.SolverOptions` | code → pack: absent from section 2 |
| `input_schemas.SolverOptions.resolved_qtf` | code → pack: absent from section 2 |
| `input_schemas.FieldPointSpec` | code → pack: absent from section 2 |
| `input_schemas.OutputSpec` | code → pack: absent from section 2 |
| `input_schemas.MetadataSpec` | code → pack: absent from section 2 |
| `input_schemas.DampingLidSpec` | code → pack: absent from section 2 |
| `input_schemas.FreeSurfaceZoneSpec` | code → pack: absent from section 2 |
| `input_schemas.ControlSurfaceSpec` | code → pack: absent from section 2 |
| `input_schemas.ControlSurfaceSpec.check_control_surface_defined` | code → pack: absent from section 2 |
| `input_schemas.MorisonElementSpec` | code → pack: absent from section 2 |
| `input_schemas.BodySpec` | code → pack: absent from section 2 |
| `input_schemas.BodySpec.resolve_control_surface` | code → pack: absent from section 2 |
| `input_schemas.DiffractionSpec.check_vessel_or_bodies` | code → pack: absent from section 2 |
| `input_schemas.DiffractionSpec.get_bodies` | code → pack: absent from section 2 |
| `input_schemas.DiffractionSpec.from_yaml` | code → pack: absent from section 2 |
| `input_schemas.DiffractionSpec.to_yaml` | code → pack: absent from section 2 |
| `input_schemas.DiffractionSpec.to_json_schema` | code → pack: absent from section 2 |
| `long_wave_limits.wavenumber` | code → pack: absent from section 2 |
| `long_wave_limits.dynamic_amplification` | code → pack: absent from section 2 |
| `long_wave_limits.expected_rao` | code → pack: absent from section 2 |
| `long_wave_limits.LongWaveCheck` | code → pack: absent from section 2 |
| `long_wave_limits.judge` | code → pack: absent from section 2 |
| `mesh_orientation.OrientationReport` | code → pack: absent from section 2 |
| `mesh_orientation.OrientationReport.sector_fraction` | code → pack: absent from section 2 |
| `mesh_orientation.OrientationReport.ok` | code → pack: absent from section 2 |
| `mesh_orientation.OrientationReport.describe` | code → pack: absent from section 2 |
| `mesh_orientation.panel_vector_areas` | code → pack: absent from section 2 |
| `mesh_orientation.orientation_report` | code → pack: absent from section 2 |
| `mesh_orientation.UnreliableOrientation` | code → pack: absent from section 2 |
| `mesh_orientation.orient_outward` | code → pack: absent from section 2 |
| `mesh_orientation.repair_gdf_text` | code → pack: absent from section 2 |
| `mesh_orientation.format_report` | code → pack: absent from section 2 |
| `mesh_packaging.MeshReference` | code → pack: absent from section 2 |
| `mesh_packaging.iter_mesh_references` | code → pack: absent from section 2 |
| `mesh_packaging.resolve_mesh_path` | code → pack: absent from section 2 |
| `mesh_packaging.copy_spec_meshes` | code → pack: absent from section 2 |
| `mesh_packaging.package_spec_meshes` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.load` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.convert` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.convert_by_name` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.prepare_for_solver` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.validate` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.detect_format` | code → pack: absent from section 2 |
| `mesh_pipeline.MeshPipeline.map_format_type` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy.from_uncertainties` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy.relative_rms_tolerance` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy.absolute_rms_floor` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy.null_response_magnitude` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy.correlation_minimum` | code → pack: absent from section 2 |
| `multi_solver_comparator.ComparisonPolicy.to_dict` | code → pack: absent from section 2 |
| `multi_solver_comparator.PairwiseRAOComparison` | code → pack: absent from section 2 |
| `multi_solver_comparator.HydrostaticComparison` | code → pack: absent from section 2 |
| `multi_solver_comparator.PairwiseResult` | code → pack: absent from section 2 |
| `multi_solver_comparator.ConsensusMetrics` | code → pack: absent from section 2 |
| `multi_solver_comparator.BenchmarkReport` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.compare_raos` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.compare_added_mass` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.compare_damping` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.compare_hydrostatics` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.compute_consensus` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.generate_report` | code → pack: absent from section 2 |
| `multi_solver_comparator.MultiSolverComparator.export_report_json` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_all` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_vessel_type` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_raos_csv` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_added_mass_csv` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_damping_csv` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_excel_workbook` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_summary` | code → pack: absent from section 2 |
| `orcaflex_exporter.OrcaFlexExporter.export_summary_report` | code → pack: absent from section 2 |
| `orcawave_backend.OrcaWaveBackend` | code → pack: absent from section 2 |
| `orcawave_backend.OrcaWaveBackend.generate_single` | code → pack: absent from section 2 |
| `orcawave_backend.OrcaWaveBackend.generate_modular` | code → pack: absent from section 2 |
| `orcawave_batch_runner.ExecutionMode` | code → pack: absent from section 2 |
| `orcawave_batch_runner.BatchJobConfig` | code → pack: absent from section 2 |
| `orcawave_batch_runner.OrcaWaveBatchConfig` | code → pack: absent from section 2 |
| `orcawave_batch_runner.OrcaWaveBatchConfig.from_yaml` | code → pack: absent from section 2 |
| `orcawave_batch_runner.BatchJobResult` | code → pack: absent from section 2 |
| `orcawave_batch_runner.OrcaWaveBatchReport` | code → pack: absent from section 2 |
| `orcawave_batch_runner.OrcaWaveBatchRunner` | code → pack: absent from section 2 |
| `orcawave_batch_runner.OrcaWaveBatchRunner.run` | code → pack: absent from section 2 |
| `orcawave_batch_runner.OrcaWaveBatchRunner.export_report` | code → pack: absent from section 2 |
| `orcawave_batch_runner.run_orcawave_batch` | code → pack: absent from section 2 |
| `orcawave_batch_runner.run_orcawave_batch_from_specs` | code → pack: absent from section 2 |
| `orcawave_doctor.DoctorCheck` | code → pack: absent from section 2 |
| `orcawave_doctor.run_doctor` | code → pack: absent from section 2 |
| `orcawave_runner.RunStatus` | code → pack: absent from section 2 |
| `orcawave_runner.RunConfig` | code → pack: absent from section 2 |
| `orcawave_runner.RunConfig.coerce_executable_path` | code → pack: absent from section 2 |
| `orcawave_runner.RunConfig.coerce_output_dir` | code → pack: absent from section 2 |
| `orcawave_runner.RunResult` | code → pack: absent from section 2 |
| `orcawave_runner.default_thread_count` | code → pack: absent from section 2 |
| `orcawave_runner.resolve_thread_count` | code → pack: absent from section 2 |
| `orcawave_runner.OrcaWaveRunner` | code → pack: absent from section 2 |
| `orcawave_runner.OrcaWaveRunner.result` | code → pack: absent from section 2 |
| `orcawave_runner.OrcaWaveRunner.run` | code → pack: absent from section 2 |
| `orcawave_runner.OrcaWaveRunner.prepare` | code → pack: absent from section 2 |
| `orcawave_runner.OrcaWaveRunner.execute` | code → pack: absent from section 2 |
| `orcawave_runner.run_orcawave` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockRAOEntry` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockRAOTable` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockAddedMassDampingEntry` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockAddedMassDampingTable` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockVesselType` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockVessel` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockDataGenerator` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockDataGenerator.generate_rao_magnitude` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockDataGenerator.generate_rao_phase` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockDataGenerator.generate_added_mass_matrix` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockDataGenerator.generate_damping_matrix` | code → pack: absent from section 2 |
| `orcawave_test_utilities.MockDataGenerator.create_mock_vessel` | code → pack: absent from section 2 |
| `orcawave_test_utilities.create_test_vessel` | code → pack: absent from section 2 |
| `orcawave_to_orcaflex.rao_data_to_diffraction_results` | code → pack: absent from section 2 |
| `orcawave_to_orcaflex.convert_orcawave_xlsx_to_orcaflex` | code → pack: absent from section 2 |
| `orcawave_to_orcaflex.main` | code → pack: absent from section 2 |
| `output_schemas.FrequencyData` | code → pack: absent from section 2 |
| `output_schemas.HeadingData` | code → pack: absent from section 2 |
| `output_schemas.RAOComponent` | code → pack: absent from section 2 |
| `output_schemas.RAOSet` | code → pack: absent from section 2 |
| `output_schemas.RAOSet.get_component` | code → pack: absent from section 2 |
| `output_schemas.RAOSet.to_dict` | code → pack: absent from section 2 |
| `output_schemas.RAOSet.from_dict` | code → pack: absent from section 2 |
| `output_schemas.HydrodynamicMatrix` | code → pack: absent from section 2 |
| `output_schemas.HydrodynamicMatrix.get_coupling` | code → pack: absent from section 2 |
| `output_schemas.HydrodynamicMatrix.to_dict` | code → pack: absent from section 2 |
| `output_schemas.AddedMassSet` | code → pack: absent from section 2 |
| `output_schemas.AddedMassSet.from_dict` | code → pack: absent from section 2 |
| `output_schemas.AddedMassSet.get_matrix_at_frequency` | code → pack: absent from section 2 |
| `output_schemas.AddedMassSet.to_dict` | code → pack: absent from section 2 |
| `output_schemas.DampingSet` | code → pack: absent from section 2 |
| `output_schemas.DampingSet.from_dict` | code → pack: absent from section 2 |
| `output_schemas.DampingSet.get_matrix_at_frequency` | code → pack: absent from section 2 |
| `output_schemas.DampingSet.to_dict` | code → pack: absent from section 2 |
| `output_schemas.HydrostaticResults` | code → pack: absent from section 2 |
| `output_schemas.HydrostaticResults.to_dict` | code → pack: absent from section 2 |
| `output_schemas.HydrostaticResults.from_dict` | code → pack: absent from section 2 |
| `output_schemas.DiffractionResults` | code → pack: absent from section 2 |
| `output_schemas.DiffractionResults.to_dict` | code → pack: absent from section 2 |
| `output_schemas.DiffractionResults.from_dict` | code → pack: absent from section 2 |
| `output_schemas.validate_rao_completeness` | code → pack: absent from section 2 |
| `output_schemas.validate_matrix_set` | code → pack: absent from section 2 |
| `output_schemas.validate_diffraction_results` | code → pack: absent from section 2 |
| `output_validator.OutputValidator` | code → pack: absent from section 2 |
| `output_validator.OutputValidator.run_all_validations` | code → pack: absent from section 2 |
| `output_validator.OutputValidator.export_report` | code → pack: absent from section 2 |
| `output_validator.validate_results` | code → pack: absent from section 2 |
| `parametric_spec_generator.SpecGeneratorConfig` | code → pack: absent from section 2 |
| `parametric_spec_generator.estimate_mass_from_dimensions` | code → pack: absent from section 2 |
| `parametric_spec_generator.estimate_cog_from_dimensions` | code → pack: absent from section 2 |
| `parametric_spec_generator.estimate_radii_of_gyration_from_dimensions` | code → pack: absent from section 2 |
| `parametric_spec_generator.estimate_mass` | code → pack: absent from section 2 |
| `parametric_spec_generator.estimate_cog` | code → pack: absent from section 2 |
| `parametric_spec_generator.estimate_radii_of_gyration` | code → pack: absent from section 2 |
| `parametric_spec_generator.generate_diffraction_spec` | code → pack: absent from section 2 |
| `parametric_spec_generator.generate_spec_yaml` | code → pack: absent from section 2 |
| `parametric_spec_generator.generate_batch_specs` | code → pack: absent from section 2 |
| `parametric_spec_generator.generate_batch_diffraction_specs` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.raos_to_polars` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.raos_to_pandas` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.added_mass_to_polars` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.damping_to_polars` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.export_raos_csv` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.export_added_mass_csv` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.export_damping_csv` | code → pack: absent from section 2 |
| `polars_exporter.PolarsExporter.export_all_csv` | code → pack: absent from section 2 |
| `postprocess.PostprocResult` | code → pack: absent from section 2 |
| `postprocess.load_results_bundle` | code → pack: absent from section 2 |
| `postprocess.postprocess_diffraction_run` | code → pack: absent from section 2 |
| `quality_gates.MeshQualityError` | code → pack: absent from section 2 |
| `quality_gates.QualityGateResult` | code → pack: absent from section 2 |
| `quality_gates.QualityGateResult.to_dict` | code → pack: absent from section 2 |
| `quality_gates.curvature_gate_issues` | code → pack: absent from section 2 |
| `quality_gates.run_mesh_quality_gate` | code → pack: absent from section 2 |
| `quality_gates.enforce_quality_gates` | code → pack: absent from section 2 |
| `rao_plotter.RAOPlotter` | code → pack: absent from section 2 |
| `rao_plotter.RAOPlotter.plot_all` | code → pack: absent from section 2 |
| `rao_plotter.RAOPlotter.plot_amplitude` | code → pack: absent from section 2 |
| `rao_plotter.RAOPlotter.plot_phase` | code → pack: absent from section 2 |
| `rao_plotter.RAOPlotter.plot_combined` | code → pack: absent from section 2 |
| `rao_plotter.RAOPlotter.plot_single_dof` | code → pack: absent from section 2 |
| `report_catalog.StructureType` | code → pack: absent from section 2 |
| `report_catalog.list_structure_types` | code → pack: absent from section 2 |
| `report_catalog.diffraction_report_skeleton` | code → pack: absent from section 2 |
| `report_catalog.report_catalog` | code → pack: absent from section 2 |
| `report_catalog.build_diffraction_report_content` | code → pack: absent from section 2 |
| `report_catalog.render_structured_report` | code → pack: absent from section 2 |
| `report_computations.compute_stability` | code → pack: absent from section 2 |
| `report_computations.compute_radii_of_gyration` | code → pack: absent from section 2 |
| `report_computations.compute_natural_periods` | code → pack: absent from section 2 |
| `report_computations.compute_peak_responses` | code → pack: absent from section 2 |
| `report_computations.compute_coupling_significance` | code → pack: absent from section 2 |
| `report_computations.generate_executive_warnings` | code → pack: absent from section 2 |
| `report_data_models.HydrostaticData` | code → pack: absent from section 2 |
| `report_data_models.RollDampingData` | code → pack: absent from section 2 |
| `report_data_models.LoadRAOData` | code → pack: absent from section 2 |
| `report_data_models.MeshQualityData` | code → pack: absent from section 2 |
| `report_data_models.DiffractionReportData` | code → pack: absent from section 2 |
| `report_generator.generate_diffraction_report` | code → pack: absent from section 2 |
| `report_generator.generate_report_from_owr` | code → pack: absent from section 2 |
| `resolver.Outcome` | code → pack: absent from section 2 |
| `resolver.PrincipalDimensions` | code → pack: absent from section 2 |
| `resolver.PrincipalDimensions.check_length_available` | code → pack: absent from section 2 |
| `resolver.PrincipalDimensions.length` | code → pack: absent from section 2 |
| `resolver.ResolverInputs` | code → pack: absent from section 2 |
| `resolver.ResolverConfig` | code → pack: absent from section 2 |
| `resolver.resolve` | code → pack: absent from section 2 |
| `result_extractor.ExtractionResult` | code → pack: absent from section 2 |
| `result_extractor.ResultExtractor` | code → pack: absent from section 2 |
| `result_extractor.ResultExtractor.extract` | code → pack: absent from section 2 |
| `result_extractor.ResultExtractor.find_model_file` | code → pack: absent from section 2 |
| `reverse_parsers.AQWAInputParser` | code → pack: absent from section 2 |
| `reverse_parsers.AQWAInputParser.parse` | code → pack: absent from section 2 |
| `reverse_parsers.OrcaWaveInputParser` | code → pack: absent from section 2 |
| `reverse_parsers.OrcaWaveInputParser.parse` | code → pack: absent from section 2 |
| `roll_damping.RollDampingError` | code → pack: absent from section 2 |
| `roll_damping.critical_damping` | code → pack: absent from section 2 |
| `roll_damping.percent_critical_to_damping` | code → pack: absent from section 2 |
| `roll_damping.damping_to_percent_critical` | code → pack: absent from section 2 |
| `roll_damping.to_orcawave_si` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis.total_inertia` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis.critical_damping` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis.natural_frequency` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis.natural_period` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis.damping_for` | code → pack: absent from section 2 |
| `roll_damping.CriticalDampingBasis.percent_for` | code → pack: absent from section 2 |
| `roll_damping.resonant_basis` | code → pack: absent from section 2 |
| `roll_damping.representable_value` | code → pack: absent from section 2 |
| `roll_damping.fidp_row_card` | code → pack: absent from section 2 |
| `spec_author.AuthoredIntent` | code → pack: absent from section 2 |
| `spec_author.IntentAuthor` | code → pack: absent from section 2 |
| `spec_author.IntentAuthor.author` | code → pack: absent from section 2 |
| `spec_author.build_system_prompt` | code → pack: absent from section 2 |
| `spec_author.AnthropicIntentAuthor` | code → pack: absent from section 2 |
| `spec_author.AnthropicIntentAuthor.author` | code → pack: absent from section 2 |
| `spec_author.ClaudeCliIntentAuthor` | code → pack: absent from section 2 |
| `spec_author.ClaudeCliIntentAuthor.author` | code → pack: absent from section 2 |
| `spec_author.intent_to_resolver_inputs` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.to_spec_yaml` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.assumptions_markdown` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.write` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.report_html` | code → pack: absent from section 2 |
| `spec_author.author_spec` | code → pack: absent from section 2 |
| `spec_author.main` | code → pack: absent from section 2 |
| `spec_converter.SpecConverter` | code → pack: absent from section 2 |
| `spec_converter.SpecConverter.convert` | code → pack: absent from section 2 |
| `spec_converter.SpecConverter.convert_all` | code → pack: absent from section 2 |
| `spec_converter.SpecConverter.validate` | code → pack: absent from section 2 |
| `spec_converter.SpecConverter.check_mesh_quality` | code → pack: absent from section 2 |
| `validation_runner.ValidationOutcome` | code → pack: absent from section 2 |
| `validation_runner.flatten_issues` | code → pack: absent from section 2 |
| `validation_runner.run_validation` | code → pack: absent from section 2 |
| `vessel_deck_builder.clean_vessel_name` | code → pack: absent from section 2 |
| `vessel_deck_builder.select_form` | code → pack: absent from section 2 |
| `vessel_deck_builder.parametric_hull_profile` | code → pack: absent from section 2 |
| `vessel_deck_builder.DeckResult` | code → pack: absent from section 2 |
| `vessel_deck_builder.build_deck` | code → pack: absent from section 2 |
| `wamit_reference_loader.WamitReferenceLoader` | code → pack: absent from section 2 |
| `wamit_reference_loader.WamitReferenceLoader.from_owd` | code → pack: absent from section 2 |
| `wamit_reference_loader.WamitReferenceLoader.from_yaml` | code → pack: absent from section 2 |
| `wamit_reference_loader.WamitReferenceLoader.redimensionalize_matrix` | code → pack: absent from section 2 |
| `workflow.DiffractionWorkflow` | code → pack: absent from section 2 |
| `workflow.DiffractionWorkflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 23. `src/digitalmodel/hydrodynamics/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 119.

| Function | Direction |
| --- | --- |
| `cli.spectrum_cmd` | code → pack: absent from section 2 |
| `cli.ocimf_wind_cmd` | code → pack: absent from section 2 |
| `cli.ocimf_current_cmd` | code → pack: absent from section 2 |
| `cli.combined_env_cmd` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.store_matrix` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.store` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.get_matrix` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.get_matrices` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.get_frequencies` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.get_infinite_frequency_added_mass` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.save_to_file` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.load_from_file` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.list_vessels` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.check_symmetry` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.check_positive_definite` | code → pack: absent from section 2 |
| `coefficient_database.CoefficientDatabase.summary` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.load_raos` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.interpolate_rao_1d` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.interpolate_rao_2d` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.interpolate_all_dofs` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.frequency_interpolation` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.extract_rao_at_frequency` | code → pack: absent from section 2 |
| `interpolator.CoefficientsInterpolator.extract_rao_at_direction` | code → pack: absent from section 2 |
| `models.WaveSpectrumType` | code → pack: absent from section 2 |
| `models.MatrixDOF` | code → pack: absent from section 2 |
| `models.HydrodynamicMatrix` | code → pack: absent from section 2 |
| `models.HydrodynamicMatrix.is_symmetric` | code → pack: absent from section 2 |
| `models.HydrodynamicMatrix.is_positive_definite` | code → pack: absent from section 2 |
| `models.HydrodynamicMatrix.get_component` | code → pack: absent from section 2 |
| `models.HydrodynamicMatrix.to_dict` | code → pack: absent from section 2 |
| `models.VesselProperties.length_to_beam_ratio` | code → pack: absent from section 2 |
| `models.VesselProperties.beam_to_draft_ratio` | code → pack: absent from section 2 |
| `models.VesselProperties.estimated_wetted_surface` | code → pack: absent from section 2 |
| `models.VesselProperties.to_dict` | code → pack: absent from section 2 |
| `models.WaveParameters.peak_frequency` | code → pack: absent from section 2 |
| `models.WaveParameters.zero_crossing_period` | code → pack: absent from section 2 |
| `models.WaveParameters.frequency_array` | code → pack: absent from section 2 |
| `models.WaveParameters.to_dict` | code → pack: absent from section 2 |
| `models.EnvironmentalConditions.to_dict` | code → pack: absent from section 2 |
| `models.RAOData.get_rao` | code → pack: absent from section 2 |
| `models.get_vessel_type` | code → pack: absent from section 2 |
| `ocimf_loading.OCIMFLoading` | code → pack: absent from section 2 |
| `ocimf_loading.OCIMFLoading.wind_load` | code → pack: absent from section 2 |
| `ocimf_loading.OCIMFLoading.current_load` | code → pack: absent from section 2 |
| `ocimf_loading.OCIMFLoading.combined_environmental_load` | code → pack: absent from section 2 |
| `ocimf_loading.OCIMFLoading.estimate_wind_area` | code → pack: absent from section 2 |
| `ocimf_loading.OCIMFLoading.newman_drift_force` | code → pack: absent from section 2 |
| `propeller_rudder.VesselPropulsion` | code → pack: absent from section 2 |
| `propeller_rudder.RudderGeometry` | code → pack: absent from section 2 |
| `propeller_rudder.RudderForces` | code → pack: absent from section 2 |
| `propeller_rudder.kt_from_poly` | code → pack: absent from section 2 |
| `propeller_rudder.soding_forces` | code → pack: absent from section 2 |
| `propeller_rudder.actuator_disk_velocity` | code → pack: absent from section 2 |
| `propeller_rudder.flat_plate_rudder_cl` | code → pack: absent from section 2 |
| `propeller_rudder.ad_flat_plate_forces` | code → pack: absent from section 2 |
| `propeller_rudder_workflow.PropellerRudderInput` | code → pack: absent from section 2 |
| `propeller_rudder_workflow.validate_propeller_rudder_input` | code → pack: absent from section 2 |
| `propeller_rudder_workflow.run_propeller_rudder_sweep` | code → pack: absent from section 2 |
| `propeller_rudder_workflow.write_propeller_rudder_results` | code → pack: absent from section 2 |
| `propeller_rudder_workflow.PropellerRudderWorkflow` | code → pack: absent from section 2 |
| `propeller_rudder_workflow.PropellerRudderWorkflow.router` | code → pack: absent from section 2 |
| `quad_mesh_quality.QualityCheck` | code → pack: absent from section 2 |
| `quad_mesh_quality.QuadQualityCriteria` | code → pack: absent from section 2 |
| `quad_mesh_quality.QuadPanelQuality` | code → pack: absent from section 2 |
| `quad_mesh_quality.QuadPanelQuality.status` | code → pack: absent from section 2 |
| `quad_mesh_quality.QuadMeshQualityReport` | code → pack: absent from section 2 |
| `quad_mesh_quality.QuadMeshQualityReport.status` | code → pack: absent from section 2 |
| `quad_mesh_quality.assess_quad_mesh` | code → pack: absent from section 2 |
| `seakeeping.compute_response_spectrum` | code → pack: absent from section 2 |
| `seakeeping.spectral_moments` | code → pack: absent from section 2 |
| `seakeeping.significant_amplitude` | code → pack: absent from section 2 |
| `seakeeping.motion_exceedance` | code → pack: absent from section 2 |
| `seakeeping.operability_analysis` | code → pack: absent from section 2 |
| `sloshing.SloshingResult` | code → pack: absent from section 2 |
| `sloshing.rectangular_tank_periods` | code → pack: absent from section 2 |
| `sloshing.cylindrical_tank_periods` | code → pack: absent from section 2 |
| `sloshing.api650_convective_period` | code → pack: absent from section 2 |
| `sloshing.oval_tank_periods` | code → pack: absent from section 2 |
| `sloshing.horizontal_cylinder_periods` | code → pack: absent from section 2 |
| `sloshing.resonance_check` | code → pack: absent from section 2 |
| `sloshing.screen_tank` | code → pack: absent from section 2 |
| `sloshing.router` | code → pack: absent from section 2 |
| `utube_tank.UTubeGeometry` | code → pack: absent from section 2 |
| `utube_tank.UTubeGeometry.leg_area` | code → pack: absent from section 2 |
| `utube_tank.TankMoment` | code → pack: absent from section 2 |
| `utube_tank.TankMoment.magnitude` | code → pack: absent from section 2 |
| `utube_tank.ConduitOptimum` | code → pack: absent from section 2 |
| `utube_tank.natural_period` | code → pack: absent from section 2 |
| `utube_tank.calibrate_effective_conduit_length` | code → pack: absent from section 2 |
| `utube_tank.equivalent_damping` | code → pack: absent from section 2 |
| `utube_tank.tank_moment` | code → pack: absent from section 2 |
| `utube_tank.coupled_roll_response` | code → pack: absent from section 2 |
| `utube_tank.optimize_conduit_area` | code → pack: absent from section 2 |
| `wang_passing_ship.WangVessel` | code → pack: absent from section 2 |
| `wang_passing_ship.WangForceResult` | code → pack: absent from section 2 |
| `wang_passing_ship.wang_surge` | code → pack: absent from section 2 |
| `wang_passing_ship.wang_sway` | code → pack: absent from section 2 |
| `wang_passing_ship.wang_yaw` | code → pack: absent from section 2 |
| `wang_passing_ship.wang_passing_forces` | code → pack: absent from section 2 |
| `wang_passing_ship.wang_forces_scalar` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.jonswap` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.pierson_moskowitz` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.bretschneider` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.issc` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.generate_spectrum` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.spectral_moment` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.significant_height_from_spectrum` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.zero_crossing_period_from_spectrum` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.peak_frequency_from_spectrum` | code → pack: absent from section 2 |
| `wave_spectra.WaveSpectra.spectrum_statistics` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 24. `src/digitalmodel/hydrodynamics/hull_library/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 215.

| Function | Direction |
| --- | --- |
| `_cutout_geometry.triangle_aspect` | code → pack: absent from section 2 |
| `_cutout_geometry.triangulate_remnant` | code → pack: absent from section 2 |
| `_cutout_geometry.validate_clearance` | code → pack: absent from section 2 |
| `_cutout_geometry.conform_local` | code → pack: absent from section 2 |
| `_cutout_geometry.mesh_quality` | code → pack: absent from section 2 |
| `_decimation_helpers.quads_to_tris` | code → pack: absent from section 2 |
| `_decimation_helpers.compute_plane_quadric` | code → pack: absent from section 2 |
| `_decimation_helpers.compute_vertex_quadrics` | code → pack: absent from section 2 |
| `_decimation_helpers.find_boundary_vertices` | code → pack: absent from section 2 |
| `_decimation_helpers.optimal_contraction_vertex` | code → pack: absent from section 2 |
| `_decimation_helpers.filter_degenerate_panels` | code → pack: absent from section 2 |
| `_decimation_helpers.merge_tris_to_quads` | code → pack: absent from section 2 |
| `analysis_setup.HullAnalysisInput` | code → pack: absent from section 2 |
| `analysis_setup.HullAnalysisResult` | code → pack: absent from section 2 |
| `analysis_setup.setup_hull_analysis` | code → pack: absent from section 2 |
| `catalog.SeaStateDefinition` | code → pack: absent from section 2 |
| `catalog.HullVariation` | code → pack: absent from section 2 |
| `catalog.MotionResponse` | code → pack: absent from section 2 |
| `catalog.HullCatalogEntry` | code → pack: absent from section 2 |
| `catalog.HullCatalog` | code → pack: absent from section 2 |
| `catalog.HullCatalog.list_hulls` | code → pack: absent from section 2 |
| `catalog.HullCatalog.get_hull` | code → pack: absent from section 2 |
| `catalog.HullCatalog.register_hull` | code → pack: absent from section 2 |
| `catalog.HullCatalog.generate_mesh` | code → pack: absent from section 2 |
| `catalog.HullCatalog.screen_hull` | code → pack: absent from section 2 |
| `catalog.HullCatalog.compute_motions` | code → pack: absent from section 2 |
| `catalog.HullCatalog.compute_accelerations` | code → pack: absent from section 2 |
| `coarsen_mesh.DecimationResult` | code → pack: absent from section 2 |
| `coarsen_mesh.coarsen_mesh` | code → pack: absent from section 2 |
| `column_pontoon_flip.flip_planar` | code → pack: absent from section 2 |
| `column_pontoon_flip.triangle_scores` | code → pack: absent from section 2 |
| `column_pontoon_form.ColumnPontoonParameters` | code → pack: absent from section 2 |
| `column_pontoon_form.ColumnPontoonParameters.validate_geometry` | code → pack: absent from section 2 |
| `column_pontoon_form.ColumnPontoonParameters.resolved_tank_depth` | code → pack: absent from section 2 |
| `column_pontoon_form.ColumnPontoonParameters.radius` | code → pack: absent from section 2 |
| `column_pontoon_form.ColumnPontoonParameters.resolved_spacing` | code → pack: absent from section 2 |
| `column_pontoon_form.ColumnPontoonReport` | code → pack: absent from section 2 |
| `column_pontoon_patches.triangulate` | code → pack: absent from section 2 |
| `column_pontoon_patches.refine` | code → pack: absent from section 2 |
| `column_pontoon_patches.edge_key` | code → pack: absent from section 2 |
| `column_pontoon_patches.split_triangle` | code → pack: absent from section 2 |
| `column_pontoon_quality.quad_ratios` | code → pack: absent from section 2 |
| `column_pontoon_quality.optimize_quad_centres` | code → pack: absent from section 2 |
| `curvature_screen.hullprod_available` | code → pack: absent from section 2 |
| `curvature_screen.require_hullprod` | code → pack: absent from section 2 |
| `curvature_screen.saddle_warning_threshold` | code → pack: absent from section 2 |
| `curvature_screen.CurvatureSignature` | code → pack: absent from section 2 |
| `curvature_screen.CurvatureSignature.as_vector` | code → pack: absent from section 2 |
| `curvature_screen.CurvatureSignature.saddle_warning` | code → pack: absent from section 2 |
| `curvature_screen.CurvatureFields` | code → pack: absent from section 2 |
| `curvature_screen.CurvatureScreenResult` | code → pack: absent from section 2 |
| `curvature_screen.panel_mesh_to_trimesh` | code → pack: absent from section 2 |
| `curvature_screen.screen_trimesh` | code → pack: absent from section 2 |
| `curvature_screen.screen_panel_mesh` | code → pack: absent from section 2 |
| `curvature_screen.screen_step` | code → pack: absent from section 2 |
| `curvature_screen.representation_delta` | code → pack: absent from section 2 |
| `curvature_screen.screen_profile` | code → pack: absent from section 2 |
| `decimation.decimate_mesh` | code → pack: absent from section 2 |
| `decimation_gmsh.remesh_coarsen` | code → pack: absent from section 2 |
| `decimation_vtk.panel_mesh_to_pyvista` | code → pack: absent from section 2 |
| `decimation_vtk.pyvista_to_panel_mesh` | code → pack: absent from section 2 |
| `decimation_vtk.decimate_mesh_vtk` | code → pack: absent from section 2 |
| `generic_drillship.AssumedDimension` | code → pack: absent from section 2 |
| `generic_drillship.GenericDrillshipBasis` | code → pack: absent from section 2 |
| `generic_drillship.generic_drillship_basis` | code → pack: absent from section 2 |
| `generic_semi.generic_semi_parameters` | code → pack: absent from section 2 |
| `hull_surface_brep.profile_point_grid` | code → pack: absent from section 2 |
| `hull_surface_brep.bspline_face_from_grid` | code → pack: absent from section 2 |
| `hull_surface_brep.mirror_and_sew` | code → pack: absent from section 2 |
| `hull_surface_brep.flat_bottom_face` | code → pack: absent from section 2 |
| `hull_surface_brep.export_step` | code → pack: absent from section 2 |
| `hull_surface_brep.profile_to_step` | code → pack: absent from section 2 |
| `hull_surface_brep_sampling.reference_section` | code → pack: absent from section 2 |
| `hull_surface_brep_sampling.section_arclength_grid` | code → pack: absent from section 2 |
| `hull_surface_brep_sampling.shared_bottom_face` | code → pack: absent from section 2 |
| `lookup.HullLookupTarget` | code → pack: absent from section 2 |
| `lookup.HullMatch` | code → pack: absent from section 2 |
| `lookup.HullLookup` | code → pack: absent from section 2 |
| `lookup.HullLookup.find_closest` | code → pack: absent from section 2 |
| `lookup.HullLookup.get_hull_form` | code → pack: absent from section 2 |
| `lookup.get_hull_form` | code → pack: absent from section 2 |
| `lookup.closest_hull` | code → pack: absent from section 2 |
| `mesh_cutouts.MoonpoolFootprint` | code → pack: absent from section 2 |
| `mesh_cutouts.MoonpoolFootprint.rectangle` | code → pack: absent from section 2 |
| `mesh_cutouts.MoonpoolFootprint.circle` | code → pack: absent from section 2 |
| `mesh_cutouts.MoonpoolFootprint.polygon` | code → pack: absent from section 2 |
| `mesh_cutouts.MeshCutoutResult` | code → pack: absent from section 2 |
| `mesh_cutouts.cut_moonpool` | code → pack: absent from section 2 |
| `mesh_generator.HullMeshGenerator` | code → pack: absent from section 2 |
| `mesh_generator.HullMeshGenerator.generate` | code → pack: absent from section 2 |
| `mesh_refiner.MeshQualityMetrics` | code → pack: absent from section 2 |
| `mesh_refiner.MeshFamilyMember` | code → pack: absent from section 2 |
| `mesh_refiner.refine_mesh` | code → pack: absent from section 2 |
| `mesh_refiner.compute_quality_metrics` | code → pack: absent from section 2 |
| `mesh_refiner.generate_mesh_family` | code → pack: absent from section 2 |
| `mesh_refiner.export_mesh_family` | code → pack: absent from section 2 |
| `mesh_refiner.convergence_summary` | code → pack: absent from section 2 |
| `mesh_scaler.ScaleDimensions` | code → pack: absent from section 2 |
| `mesh_scaler.ScaleResult` | code → pack: absent from section 2 |
| `mesh_scaler.scale_mesh_uniform` | code → pack: absent from section 2 |
| `mesh_scaler.scale_mesh_parametric` | code → pack: absent from section 2 |
| `mesh_scaler.scale_mesh_to_target` | code → pack: absent from section 2 |
| `mesh_scaler.validate_scaled_mesh` | code → pack: absent from section 2 |
| `mesh_scaler.export_scaled_gdf` | code → pack: absent from section 2 |
| `panel_catalog.PanelFormat` | code → pack: absent from section 2 |
| `panel_catalog.RaoReference` | code → pack: absent from section 2 |
| `panel_catalog.RaoReference.to_dict` | code → pack: absent from section 2 |
| `panel_catalog.PanelCatalogEntry` | code → pack: absent from section 2 |
| `panel_catalog.PanelCatalog` | code → pack: absent from section 2 |
| `panel_catalog.PanelCatalog.validate_unique_hull_ids` | code → pack: absent from section 2 |
| `panel_catalog.PanelCatalog.to_yaml` | code → pack: absent from section 2 |
| `panel_catalog.PanelCatalog.from_yaml` | code → pack: absent from section 2 |
| `panel_catalog.PanelCatalog.to_csv` | code → pack: absent from section 2 |
| `panel_inventory.scan_gdf_directory` | code → pack: absent from section 2 |
| `panel_inventory.scan_aqwa_dat_directory` | code → pack: absent from section 2 |
| `panel_inventory.scan_orcaflex_vessels` | code → pack: absent from section 2 |
| `panel_inventory.scan_metadata_hulls` | code → pack: absent from section 2 |
| `panel_inventory.build_full_catalog` | code → pack: absent from section 2 |
| `parametric_form.MonohullFormParameters` | code → pack: absent from section 2 |
| `parametric_form.midship_section` | code → pack: absent from section 2 |
| `parametric_form.midship_area` | code → pack: absent from section 2 |
| `parametric_form.sectional_area_curve` | code → pack: absent from section 2 |
| `parametric_form.station_offsets` | code → pack: absent from section 2 |
| `parametric_form.FormReport` | code → pack: absent from section 2 |
| `parametric_form.generate_profile` | code → pack: absent from section 2 |
| `parametric_form.form_report` | code → pack: absent from section 2 |
| `parametric_form.sweep_forms` | code → pack: absent from section 2 |
| `parametric_form_ends.end_parameters` | code → pack: absent from section 2 |
| `parametric_form_ends.end_curve` | code → pack: absent from section 2 |
| `parametric_form_ends.solve_sac` | code → pack: absent from section 2 |
| `parametric_hull.ParametricRange` | code → pack: absent from section 2 |
| `parametric_hull.ParametricRange.values` | code → pack: absent from section 2 |
| `parametric_hull.HullParametricSpace` | code → pack: absent from section 2 |
| `parametric_hull.HullParametricSpace.combinations` | code → pack: absent from section 2 |
| `parametric_hull.HullParametricSpace.generate_profiles` | code → pack: absent from section 2 |
| `parametric_hull.HullParametricSpace.generate_signatures` | code → pack: absent from section 2 |
| `parametric_hull.form_space_profiles` | code → pack: absent from section 2 |
| `parametric_rao_pilot.digest` | code → pack: absent from section 2 |
| `parametric_rao_pilot.write_json` | code → pack: absent from section 2 |
| `parametric_rao_pilot.case_table` | code → pack: absent from section 2 |
| `parametric_rao_pilot.geometry` | code → pack: absent from section 2 |
| `parametric_rao_pilot.geometry_digest` | code → pack: absent from section 2 |
| `parametric_rao_pilot.freeze_criteria` | code → pack: absent from section 2 |
| `parametric_rao_pilot.validate_receipt` | code → pack: absent from section 2 |
| `parametric_rao_pilot.point_reasons` | code → pack: absent from section 2 |
| `parametric_rao_pilot.masked_indices` | code → pack: absent from section 2 |
| `parametric_rao_pilot.mask_irregular_points` | code → pack: absent from section 2 |
| `parametric_rao_pilot.validate_physical_points` | code → pack: absent from section 2 |
| `parametric_rao_pilot.database_entry` | code → pack: absent from section 2 |
| `parametric_rao_pilot.compare_heave` | code → pack: absent from section 2 |
| `parametric_rao_pilot.convergence_record` | code → pack: absent from section 2 |
| `parametric_rao_pilot.input_digest` | code → pack: absent from section 2 |
| `parametric_rao_pilot.read_record` | code → pack: absent from section 2 |
| `parametric_rao_pilot.floating_body` | code → pack: absent from section 2 |
| `parametric_rao_pilot.bem_solver` | code → pack: absent from section 2 |
| `parametric_rao_pilot.homogeneous_mass_properties` | code → pack: absent from section 2 |
| `parametric_rao_pilot.solve_case` | code → pack: absent from section 2 |
| `parametric_rao_report.comparison_graph` | code → pack: absent from section 2 |
| `parametric_rao_report.build_lookup` | code → pack: absent from section 2 |
| `profile_schema.HullType` | code → pack: absent from section 2 |
| `profile_schema.HullStation` | code → pack: absent from section 2 |
| `profile_schema.HullStation.validate_offsets` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.validate_minimum_stations` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.validate_block_coefficient` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.validate_stations_within_dimensions` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.to_yaml_dict` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.from_yaml_dict` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.save_yaml` | code → pack: absent from section 2 |
| `profile_schema.HullProfile.load_yaml` | code → pack: absent from section 2 |
| `rao_database.RAODatabaseEntry` | code → pack: absent from section 2 |
| `rao_database.RAODatabase` | code → pack: absent from section 2 |
| `rao_database.RAODatabase.store` | code → pack: absent from section 2 |
| `rao_database.RAODatabase.get_by_id` | code → pack: absent from section 2 |
| `rao_database.RAODatabase.query` | code → pack: absent from section 2 |
| `rao_database.RAODatabase.list_parameters` | code → pack: absent from section 2 |
| `rao_database.RAODatabase.save_to_disk` | code → pack: absent from section 2 |
| `rao_database.RAODatabase.load_from_disk` | code → pack: absent from section 2 |
| `rao_extractor.HydroCoefficients` | code → pack: absent from section 2 |
| `rao_extractor.XlsxFormatDetector` | code → pack: absent from section 2 |
| `rao_extractor.XlsxFormatDetector.detect` | code → pack: absent from section 2 |
| `rao_extractor.PipelineXlsxReader` | code → pack: absent from section 2 |
| `rao_extractor.PipelineXlsxReader.read_rao_data` | code → pack: absent from section 2 |
| `rao_extractor.PipelineXlsxReader.read_hydro_coefficients` | code → pack: absent from section 2 |
| `rao_extractor.NativeXlsxReader` | code → pack: absent from section 2 |
| `rao_extractor.NativeXlsxReader.read_rao_data` | code → pack: absent from section 2 |
| `rao_extractor.NativeXlsxReader.read_hydro_coefficients` | code → pack: absent from section 2 |
| `rao_extractor.xlsx_to_rao_data` | code → pack: absent from section 2 |
| `rao_extractor.populate_database_from_xlsx` | code → pack: absent from section 2 |
| `rao_fixture_comparison.xlsx_significant_digits` | code → pack: absent from section 2 |
| `rao_fixture_comparison.compare_box_exports` | code → pack: absent from section 2 |
| `rao_fixture_population.store_fixture_entry` | code → pack: absent from section 2 |
| `rao_fixture_population.populate_fixture_database` | code → pack: absent from section 2 |
| `rao_fixture_population.regenerate_fixture_database` | code → pack: absent from section 2 |
| `rao_lookup_plots.per_hull_rao_plot` | code → pack: absent from section 2 |
| `rao_lookup_plots.comparison_plot` | code → pack: absent from section 2 |
| `rao_lookup_plots.parameter_sweep_plot` | code → pack: absent from section 2 |
| `rao_lookup_plots.export_html` | code → pack: absent from section 2 |
| `rao_lookup_plots.export_png` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry.index_path` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry.register_rao` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry.get_raos` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry.list_hulls` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry.load_rao_data` | code → pack: absent from section 2 |
| `rao_registry.RaoRegistry.link_to_catalog` | code → pack: absent from section 2 |
| `schematic_generator.SchematicGenerator` | code → pack: absent from section 2 |
| `schematic_generator.SchematicGenerator.profile_view` | code → pack: absent from section 2 |
| `schematic_generator.SchematicGenerator.plan_view` | code → pack: absent from section 2 |
| `schematic_generator.SchematicGenerator.body_plan` | code → pack: absent from section 2 |
| `schematic_generator.SchematicGenerator.generate_all` | code → pack: absent from section 2 |
| `schematic_generator.SchematicGenerator.save_all` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 25. `src/digitalmodel/hydrodynamics/parametric_hull_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 29.

| Function | Direction |
| --- | --- |
| `charts.rao_comparison_grid` | code → pack: absent from section 2 |
| `charts.parameter_sensitivity_plot` | code → pack: absent from section 2 |
| `charts.depth_sensitivity_plot` | code → pack: absent from section 2 |
| `charts.passing_ship_contour` | code → pack: absent from section 2 |
| `charts.operability_chart` | code → pack: absent from section 2 |
| `forward_speed.encounter_frequency` | code → pack: absent from section 2 |
| `forward_speed.correct_rao_for_speed` | code → pack: absent from section 2 |
| `forward_speed.strip_theory_speed_correction` | code → pack: absent from section 2 |
| `models.DepthClassification` | code → pack: absent from section 2 |
| `models.BankSlopeType` | code → pack: absent from section 2 |
| `models.SweepConfig` | code → pack: absent from section 2 |
| `models.PassingShipSweepConfig` | code → pack: absent from section 2 |
| `models.SweepResultEntry` | code → pack: absent from section 2 |
| `models.PassingShipSweepEntry` | code → pack: absent from section 2 |
| `models.classify_depth` | code → pack: absent from section 2 |
| `passing_ship_sweep.hull_profile_to_vessel_config` | code → pack: absent from section 2 |
| `passing_ship_sweep.run_passing_ship_sweep` | code → pack: absent from section 2 |
| `passing_ship_sweep.passing_ship_to_dataframe` | code → pack: absent from section 2 |
| `passing_ship_sweep.peak_force_envelope` | code → pack: absent from section 2 |
| `passing_ship_sweep.pianc_operability_check` | code → pack: absent from section 2 |
| `shallow_water.dnv_shallow_water_factor` | code → pack: absent from section 2 |
| `shallow_water.validate_shallow_water_results` | code → pack: absent from section 2 |
| `shallow_water.pianc_bank_suction_force` | code → pack: absent from section 2 |
| `shallow_water.pianc_bank_clearance_width` | code → pack: absent from section 2 |
| `sweep.run_parametric_sweep` | code → pack: absent from section 2 |
| `sweep.sweep_to_dataframe` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 26. `src/digitalmodel/hydrodynamics/planing_hull/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 14.

| Function | Direction |
| --- | --- |
| `solver.PlaningMotionSolver` | code → pack: absent from section 2 |
| `solver.PlaningMotionSolver.integrate` | code → pack: absent from section 2 |
| `strip_model.PlaningStripModel` | code → pack: absent from section 2 |
| `strip_model.PlaningStripModel.added_mass_per_length` | code → pack: absent from section 2 |
| `strip_model.PlaningStripModel.half_wetted_beam` | code → pack: absent from section 2 |
| `strip_model.PlaningStripModel.water_entry_force_per_length` | code → pack: absent from section 2 |
| `strip_model.PlaningStripModel.buoyancy_force_per_length` | code → pack: absent from section 2 |
| `strip_model.PlaningStripModel.planing_lift_per_length` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L3` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26253-L26502` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26256-L26258` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26260-L26269` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26270-L26279` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26280-L26289` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26290-L26299` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26300-L26309` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26310-L26319` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26320-L26329` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26330-L26339` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26340-L26349` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26350-L26361` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26362-L26373` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26374-L26384` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26385-L26395` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26396-L26405` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26406-L26415` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26416-L26425` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26426-L26435` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26436-L26447` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26448-L26458` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26459-L26474` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L26475-L26486` | pack → code: file missing or range invalid |

### 27. `src/digitalmodel/infrastructure/base_solvers/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 37.

| Function | Direction |
| --- | --- |
| `base.SolverStatus` | code → pack: absent from section 2 |
| `base.BaseSolver` | code → pack: absent from section 2 |
| `base.BaseSolver.validate_inputs` | code → pack: absent from section 2 |
| `base.BaseSolver.solve` | code → pack: absent from section 2 |
| `base.BaseSolver.get_solver_metadata` | code → pack: absent from section 2 |
| `base.BaseSolver.get_status` | code → pack: absent from section 2 |
| `base.BaseSolver.get_results` | code → pack: absent from section 2 |
| `base.ConfigurableSolver` | code → pack: absent from section 2 |
| `base.ConfigurableSolver.get_config` | code → pack: absent from section 2 |
| `base.ConfigurableSolver.set_config` | code → pack: absent from section 2 |
| `base.ConfigurableSolver.get_all_config` | code → pack: absent from section 2 |
| `base.AnalysisSolver` | code → pack: absent from section 2 |
| `base.AnalysisSolver.set_input_data` | code → pack: absent from section 2 |
| `base.AnalysisSolver.get_input_data` | code → pack: absent from section 2 |
| `base.AnalysisSolver.set_validation_errors` | code → pack: absent from section 2 |
| `base.AnalysisSolver.get_validation_errors` | code → pack: absent from section 2 |
| `base.AnalysisSolver.add_validation_error` | code → pack: absent from section 2 |
| `base.AnalysisSolver.clear_validation_errors` | code → pack: absent from section 2 |
| `base.AnalysisSolver.has_validation_errors` | code → pack: absent from section 2 |
| `interfaces.SolverProtocol` | code → pack: absent from section 2 |
| `interfaces.SolverProtocol.validate_inputs` | code → pack: absent from section 2 |
| `interfaces.SolverProtocol.solve` | code → pack: absent from section 2 |
| `interfaces.SolverProtocol.get_solver_metadata` | code → pack: absent from section 2 |
| `interfaces.SolverProtocol.get_status` | code → pack: absent from section 2 |
| `interfaces.SolverProtocol.get_results` | code → pack: absent from section 2 |
| `interfaces.ConfigurableSolverProtocol` | code → pack: absent from section 2 |
| `interfaces.ConfigurableSolverProtocol.get_config` | code → pack: absent from section 2 |
| `interfaces.ConfigurableSolverProtocol.set_config` | code → pack: absent from section 2 |
| `interfaces.ConfigurableSolverProtocol.get_all_config` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.set_input_data` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.get_input_data` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.set_validation_errors` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.get_validation_errors` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.add_validation_error` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.clear_validation_errors` | code → pack: absent from section 2 |
| `interfaces.AnalysisSolverProtocol.has_validation_errors` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 28. `src/digitalmodel/installation/installation_pamphlet/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 19.

| Function | Direction |
| --- | --- |
| `calculations.load_mudmats` | code → pack: absent from section 2 |
| `calculations.load_jumpers` | code → pack: absent from section 2 |
| `calculations.load_structures` | code → pack: absent from section 2 |
| `calculations.load_rao_set_from_artifact` | code → pack: absent from section 2 |
| `calculations.resolve_rao_artifact` | code → pack: absent from section 2 |
| `calculations.assess_lifts` | code → pack: absent from section 2 |
| `calculations.compute_envelopes` | code → pack: absent from section 2 |
| `calculations.compute_envelope_band` | code → pack: absent from section 2 |
| `calculations.compute_splashzone_envelope` | code → pack: absent from section 2 |
| `calculations.compute_splashzone_set` | code → pack: absent from section 2 |
| `calculations.compute_operability` | code → pack: absent from section 2 |
| `calculations.select_rao_basis` | code → pack: absent from section 2 |
| `calculations.select_basis_run_id` | code → pack: absent from section 2 |
| `calculations.build_provenance` | code → pack: absent from section 2 |
| `calculations.assemble_result` | code → pack: absent from section 2 |
| `render.forecast_cell` | code → pack: absent from section 2 |
| `render.render_pamphlet_html` | code → pack: absent from section 2 |
| `workflow.scan_completed_runs` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 29. `src/digitalmodel/installation/pipelay/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 17.

| Function | Direction |
| --- | --- |
| `integrity.bending_strain_from_radius` | code → pack: absent from section 2 |
| `integrity.concrete_crushing_strain` | code → pack: absent from section 2 |
| `integrity.concrete_crushing_check` | code → pack: absent from section 2 |
| `integrity.minimum_route_radius` | code → pack: absent from section 2 |
| `integrity.route_radius_factor_of_safety` | code → pack: absent from section 2 |
| `integrity.weld_repair_allowable_length` | code → pack: absent from section 2 |
| `integrity.is_weld_repair_acceptable` | code → pack: absent from section 2 |
| `integrity.displaced_weight_per_length` | code → pack: absent from section 2 |
| `integrity.submerged_weight_per_length` | code → pack: absent from section 2 |
| `integrity.horizontal_lay_tension` | code → pack: absent from section 2 |
| `integrity.top_lay_tension` | code → pack: absent from section 2 |
| `stress.bending_stress_from_radius` | code → pack: absent from section 2 |
| `stress.axial_stress` | code → pack: absent from section 2 |
| `stress.smys_stress_check` | code → pack: absent from section 2 |
| `stress.allowable_bend_radius` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 30. `src/digitalmodel/marine_ops/artificial_lift/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 39.

| Function | Direction |
| --- | --- |
| `_reference_catalog_io.sha256_file` | code → pack: absent from section 2 |
| `_reference_catalog_io.source_path` | code → pack: absent from section 2 |
| `_reference_catalog_io.text` | code → pack: absent from section 2 |
| `_reference_catalog_io.decimal_text` | code → pack: absent from section 2 |
| `_reference_catalog_io.is_numeric` | code → pack: absent from section 2 |
| `_reference_catalog_io.read_rows` | code → pack: absent from section 2 |
| `_reference_catalog_io.scan_safe` | code → pack: absent from section 2 |
| `_reference_catalog_io.contains_prohibited` | code → pack: absent from section 2 |
| `_reference_catalog_io.write_csv` | code → pack: absent from section 2 |
| `_reference_catalog_io.output_lock` | code → pack: absent from section 2 |
| `_reference_catalog_manifest.emit_tree` | code → pack: absent from section 2 |
| `_reference_catalog_manifest.counts` | code → pack: absent from section 2 |
| `_reference_catalog_manifest.validate_stage` | code → pack: absent from section 2 |
| `_reference_catalog_manifest.source_meta` | code → pack: absent from section 2 |
| `_reference_catalog_manifest.physics_summary` | code → pack: absent from section 2 |
| `_reference_catalog_manifest.independent_falsification` | code → pack: absent from section 2 |
| `_reference_catalog_schema.is_safe_surface_model` | code → pack: absent from section 2 |
| `_reference_catalog_schema.unit_for_field` | code → pack: absent from section 2 |
| `_reference_catalog_transform.rod_detail_rows` | code → pack: absent from section 2 |
| `_reference_catalog_transform.rods_catalog_rows` | code → pack: absent from section 2 |
| `_reference_catalog_transform.connection_rows` | code → pack: absent from section 2 |
| `_reference_catalog_transform.connection_lookup_rows` | code → pack: absent from section 2 |
| `field_health.router` | code → pack: absent from section 2 |
| `field_health.run_field_troubleshooter` | code → pack: absent from section 2 |
| `field_health.load_and_map_well` | code → pack: absent from section 2 |
| `reference_catalog.AmbiguousCatalogKeyError` | code → pack: absent from section 2 |
| `reference_catalog.RodProperties` | code → pack: absent from section 2 |
| `reference_catalog.CouplingProperties` | code → pack: absent from section 2 |
| `reference_catalog.SurfaceUnitGeometry` | code → pack: absent from section 2 |
| `reference_catalog.ReferenceCatalog` | code → pack: absent from section 2 |
| `reference_catalog.ReferenceCatalog.rod_properties` | code → pack: absent from section 2 |
| `reference_catalog.ReferenceCatalog.find_couplings` | code → pack: absent from section 2 |
| `reference_catalog.ReferenceCatalog.find_surface_units` | code → pack: absent from section 2 |
| `reference_catalog.load_catalog` | code → pack: absent from section 2 |
| `reference_catalog.rod_properties` | code → pack: absent from section 2 |
| `reference_catalog.find_couplings` | code → pack: absent from section 2 |
| `reference_catalog.coupling_properties` | code → pack: absent from section 2 |
| `reference_catalog.find_surface_units` | code → pack: absent from section 2 |
| `reference_catalog.surface_unit_geometry` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 31. `src/digitalmodel/marine_ops/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 26.

| Function | Direction |
| --- | --- |
| `envelope_numerics.finite_grid` | code → pack: absent from section 2 |
| `envelope_numerics.validate_operation` | code → pack: absent from section 2 |
| `envelope_numerics.validate_component` | code → pack: absent from section 2 |
| `envelope_numerics.integrate` | code → pack: absent from section 2 |
| `envelope_numerics.spectrum` | code → pack: absent from section 2 |
| `envelope_numerics.extrapolated_fraction` | code → pack: absent from section 2 |
| `envelope_numerics.heading_column` | code → pack: absent from section 2 |
| `operation_envelope.MotionLimit` | code → pack: absent from section 2 |
| `operation_envelope.available_operations` | code → pack: absent from section 2 |
| `operation_envelope.resolve_operation` | code → pack: absent from section 2 |
| `operation_envelope.EnvelopeResult.require_valid_points` | code → pack: absent from section 2 |
| `response_benchmark.ResponseReference` | code → pack: absent from section 2 |
| `response_benchmark.ReferenceIntake` | code → pack: absent from section 2 |
| `response_benchmark.ReferenceIntake.intake_complete` | code → pack: absent from section 2 |
| `response_benchmark.inspect_response_reference` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 32. `src/digitalmodel/marine_ops/installation/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 130.

| Function | Direction |
| --- | --- |
| `crane_tip_motion.crane_tip_raos` | code → pack: absent from section 2 |
| `crane_tip_motion.crane_tip_significant_motion` | code → pack: absent from section 2 |
| `crane_tip_motion.crane_tip_significant_velocity` | code → pack: absent from section 2 |
| `four_point_skew.solve_case` | code → pack: absent from section 2 |
| `four_point_skew.tolerance_corners` | code → pack: absent from section 2 |
| `four_point_skew.calculate_skew` | code → pack: absent from section 2 |
| `four_point_skew.main` | code → pack: absent from section 2 |
| `go_no_go.DecisionState` | code → pack: absent from section 2 |
| `go_no_go.CriterionState` | code → pack: absent from section 2 |
| `go_no_go.CriterionResult` | code → pack: absent from section 2 |
| `go_no_go.GoNoGoDecision` | code → pack: absent from section 2 |
| `go_no_go.evaluate_go_no_go` | code → pack: absent from section 2 |
| `go_no_go.print_decision` | code → pack: absent from section 2 |
| `jumper_input.validate_input` | code → pack: absent from section 2 |
| `jumper_input.generate_input` | code → pack: absent from section 2 |
| `jumper_input.main` | code → pack: absent from section 2 |
| `jumper_installation.PipelineConfig` | code → pack: absent from section 2 |
| `jumper_installation.PipelineOutput` | code → pack: absent from section 2 |
| `jumper_installation.load_spec` | code → pack: absent from section 2 |
| `jumper_installation.parse_jumper_config` | code → pack: absent from section 2 |
| `jumper_installation.stage_1_load_spec` | code → pack: absent from section 2 |
| `jumper_installation.stage_2_calculate` | code → pack: absent from section 2 |
| `jumper_installation.extract_metocean_conditions` | code → pack: absent from section 2 |
| `jumper_installation.stage_3_go_no_go` | code → pack: absent from section 2 |
| `jumper_installation.stage_4_generate_yaml` | code → pack: absent from section 2 |
| `jumper_installation.stage_5_write_outputs` | code → pack: absent from section 2 |
| `jumper_installation.run_pipeline` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperMetadata` | code → pack: absent from section 2 |
| `jumper_installation_schema.WaterSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.SeabedSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.WindSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.WaveSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.MetoceanSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperEnvironmentSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.InsulationSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.PipeSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperGeometrySpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.CraneSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperInstallationSpec` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperInstallationSpec.validate_cranes_nonempty` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperInstallationSpec.from_yaml` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperInstallationSpec.from_dict` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperInstallationSpec.to_dict` | code → pack: absent from section 2 |
| `jumper_installation_schema.JumperInstallationSpec.to_yaml` | code → pack: absent from section 2 |
| `jumper_lift.JumperConfig` | code → pack: absent from section 2 |
| `jumper_lift.BarePipeProperties` | code → pack: absent from section 2 |
| `jumper_lift.ConnectorProperties` | code → pack: absent from section 2 |
| `jumper_lift.ClampProperties` | code → pack: absent from section 2 |
| `jumper_lift.PipeSectionLengths` | code → pack: absent from section 2 |
| `jumper_lift.BuoyancyModuleProperties` | code → pack: absent from section 2 |
| `jumper_lift.StrakeProperties` | code → pack: absent from section 2 |
| `jumper_lift.RiggingProperties` | code → pack: absent from section 2 |
| `jumper_lift.CraneConfig` | code → pack: absent from section 2 |
| `jumper_lift.KitWeight` | code → pack: absent from section 2 |
| `jumper_lift.KitWeightInsulated` | code → pack: absent from section 2 |
| `jumper_lift.compute_pipe_geometry` | code → pack: absent from section 2 |
| `jumper_lift.compute_bare_pipe` | code → pack: absent from section 2 |
| `jumper_lift.compute_buoyancy` | code → pack: absent from section 2 |
| `jumper_lift.compute_strake` | code → pack: absent from section 2 |
| `jumper_lift.compute_sling_stiffness` | code → pack: absent from section 2 |
| `jumper_lift.compute_spreader_bar` | code → pack: absent from section 2 |
| `jumper_lift.compute_sling_geometry` | code → pack: absent from section 2 |
| `jumper_lift.compute_rigging_sling_lengths` | code → pack: absent from section 2 |
| `jumper_lift.compute_crane_configs` | code → pack: absent from section 2 |
| `jumper_lift.compute_crane_utilisation` | code → pack: absent from section 2 |
| `jumper_lift.compute_orcaflex_sections` | code → pack: absent from section 2 |
| `jumper_lift.compute_weight_check` | code → pack: absent from section 2 |
| `jumper_lift.compute_weight_check_uninsulated` | code → pack: absent from section 2 |
| `jumper_lift.compute_cog_uninsulated` | code → pack: absent from section 2 |
| `jumper_lift.compute_cog_insulated` | code → pack: absent from section 2 |
| `jumper_lift.compute_pipe_lengths_for_weight_estimate` | code → pack: absent from section 2 |
| `jumper_lift.compute_estimated_pipe_weight` | code → pack: absent from section 2 |
| `jumper_lift.compute_connector_buoy_totals` | code → pack: absent from section 2 |
| `jumper_lift.generate_orcaflex_line_sections_yaml` | code → pack: absent from section 2 |
| `jumper_lift.run_all` | code → pack: absent from section 2 |
| `jumper_lift.run_jumper_analysis` | code → pack: absent from section 2 |
| `jumper_to_modular_spec.build_line_types` | code → pack: absent from section 2 |
| `jumper_to_modular_spec.walk_m_profile` | code → pack: absent from section 2 |
| `jumper_to_modular_spec.build_modular_spec` | code → pack: absent from section 2 |
| `models.InstallationPhase` | code → pack: absent from section 2 |
| `models.GoNoGoState` | code → pack: absent from section 2 |
| `models.Structure.weight_air_N` | code → pack: absent from section 2 |
| `models.Structure.submerged_weight_N` | code → pack: absent from section 2 |
| `models.CraneTipConfig` | code → pack: absent from section 2 |
| `models.CraneTipConfig.position_vector` | code → pack: absent from section 2 |
| `models.CraneCurve` | code → pack: absent from section 2 |
| `models.CraneCurve.capacity_at_radius` | code → pack: absent from section 2 |
| `models.Vessel` | code → pack: absent from section 2 |
| `models.Vessel.has_complete_raos` | code → pack: absent from section 2 |
| `models.InstallationCriteria` | code → pack: absent from section 2 |
| `models.OperabilityResult` | code → pack: absent from section 2 |
| `models.SplashZoneResult` | code → pack: absent from section 2 |
| `models.InstallationCase` | code → pack: absent from section 2 |
| `operability.hs_limit_for_criterion` | code → pack: absent from section 2 |
| `operability.compute_operability` | code → pack: absent from section 2 |
| `operability.weather_window_operability` | code → pack: absent from section 2 |
| `realtime_feedback.MRUReading` | code → pack: absent from section 2 |
| `realtime_feedback.MRUBuffer` | code → pack: absent from section 2 |
| `realtime_feedback.MRUBuffer.add` | code → pack: absent from section 2 |
| `realtime_feedback.MRUBuffer.significant_heave` | code → pack: absent from section 2 |
| `realtime_feedback.MRUBuffer.max_roll` | code → pack: absent from section 2 |
| `realtime_feedback.MRUBuffer.max_pitch` | code → pack: absent from section 2 |
| `realtime_feedback.CriteriaExceedance` | code → pack: absent from section 2 |
| `realtime_feedback.InstallationStateMachine` | code → pack: absent from section 2 |
| `realtime_feedback.InstallationStateMachine.check_criteria` | code → pack: absent from section 2 |
| `realtime_feedback.InstallationStateMachine.advance_phase` | code → pack: absent from section 2 |
| `shore_approach.RouteSegment` | code → pack: absent from section 2 |
| `shore_approach.pull_in` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.numeric_paths` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.source_locator` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.audit_source` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.audit_geometry_basis` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.audit_dataset_anchors` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.audit_input_sources` | code → pack: absent from section 2 |
| `shore_pull_in_provenance.audit_entry` | code → pack: absent from section 2 |
| `splash_zone.slamming_force` | code → pack: absent from section 2 |
| `splash_zone.varying_buoyancy_force` | code → pack: absent from section 2 |
| `splash_zone.splash_zone_assessment` | code → pack: absent from section 2 |
| `suitability_report.SuitabilityReport` | code → pack: absent from section 2 |
| `suitability_report.build_report` | code → pack: absent from section 2 |
| `suitability_report.to_markdown` | code → pack: absent from section 2 |
| `suitability_report.to_html` | code → pack: absent from section 2 |
| `vessel_screening.VesselScreeningResult` | code → pack: absent from section 2 |
| `vessel_screening.screen_vessels` | code → pack: absent from section 2 |
| `vessel_suitability.LiftRequirement` | code → pack: absent from section 2 |
| `vessel_suitability.SuitabilityResult` | code → pack: absent from section 2 |
| `vessel_suitability.assess_vessel` | code → pack: absent from section 2 |
| `vessel_suitability.rank_fleet` | code → pack: absent from section 2 |
| `vessel_suitability.assess_named` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 33. `src/digitalmodel/marine_ops/marine_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 70.

| Function | Direction |
| --- | --- |
| `__main__.main` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAFileError` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.parse_lis_file` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_damping_matrices` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_added_mass_matrices` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_added_mass_at_infinite_frequency` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_drag_table` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_external_damping` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_percent_critical_damping` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.extract_restoring_coefficients` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.expand_abbreviated_data` | code → pack: absent from section 2 |
| `aqwa_enhanced_parser.AQWAEnhancedParser.export_to_csv` | code → pack: absent from section 2 |
| `aqwa_reader.AQWAFileError` | code → pack: absent from section 2 |
| `aqwa_reader.AQWAReader` | code → pack: absent from section 2 |
| `aqwa_reader.AQWAReader.parse_lis_file` | code → pack: absent from section 2 |
| `aqwa_reader_fixed.AQWAFileError` | code → pack: absent from section 2 |
| `aqwa_reader_fixed.AQWAReaderFixed` | code → pack: absent from section 2 |
| `aqwa_reader_fixed.AQWAReaderFixed.parse_lis_file` | code → pack: absent from section 2 |
| `orcaflex_reader.OrcaFlexFileError` | code → pack: absent from section 2 |
| `orcaflex_reader.OrcaFlexReader` | code → pack: absent from section 2 |
| `orcaflex_reader.OrcaFlexReader.parse_yml_file` | code → pack: absent from section 2 |
| `rao_interpolator.RAOInterpolator` | code → pack: absent from section 2 |
| `rao_interpolator.RAOInterpolator.interpolate_2d` | code → pack: absent from section 2 |
| `rao_interpolator.RAOInterpolator.interpolate_1d_frequency` | code → pack: absent from section 2 |
| `rao_interpolator.RAOInterpolator.compute_interpolation_quality` | code → pack: absent from section 2 |
| `rao_processor.RAOData` | code → pack: absent from section 2 |
| `rao_processor.RAOData.is_valid` | code → pack: absent from section 2 |
| `rao_processor.VerificationReport` | code → pack: absent from section 2 |
| `rao_processor.RAOImportError` | code → pack: absent from section 2 |
| `rao_processor.RAOImportError.user_message` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.import_aqwa_lis_file` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.import_orcaflex_yml_file` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.get_rao_dataframes` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.create_verification_report` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.generate_spot_check_values` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.check_phase_continuity` | code → pack: absent from section 2 |
| `rao_processor.RAODataProcessor.interpolate_rao_data` | code → pack: absent from section 2 |
| `rao_quality_report.RAOQualityReportGenerator` | code → pack: absent from section 2 |
| `rao_quality_report.RAOQualityReportGenerator.generate_html_report` | code → pack: absent from section 2 |
| `rao_quality_report.RAOQualityReportGenerator.export_csv_summary` | code → pack: absent from section 2 |
| `rao_validators.ValidationReport` | code → pack: absent from section 2 |
| `rao_validators.ValidationReport.add_error` | code → pack: absent from section 2 |
| `rao_validators.ValidationReport.add_warning` | code → pack: absent from section 2 |
| `rao_validators.ValidationReport.add_suggestion` | code → pack: absent from section 2 |
| `rao_validators.ValidationReport.merge` | code → pack: absent from section 2 |
| `rao_validators.PhaseConvention` | code → pack: absent from section 2 |
| `rao_validators.VesselType` | code → pack: absent from section 2 |
| `rao_validators.VesselTypeCharacteristics` | code → pack: absent from section 2 |
| `rao_validators.LongPeriodExpectation` | code → pack: absent from section 2 |
| `rao_validators.get_long_period_expectations` | code → pack: absent from section 2 |
| `rao_validators.PhaseCheckResult` | code → pack: absent from section 2 |
| `rao_validators.PeakDetectionResult` | code → pack: absent from section 2 |
| `rao_validators.DisplacementRAOQualityReport` | code → pack: absent from section 2 |
| `rao_validators.DisplacementRAOQualityReport.overall_status` | code → pack: absent from section 2 |
| `rao_validators.DisplacementRAOQualityReport.pass_rate` | code → pack: absent from section 2 |
| `rao_validators.RAODataValidators` | code → pack: absent from section 2 |
| `rao_validators.RAODataValidators.validate_rao_data` | code → pack: absent from section 2 |
| `rao_validators.RAODataValidators.validate_experimental_data` | code → pack: absent from section 2 |
| `rao_validators.RAODataValidators.validate_displacement_rao_quality` | code → pack: absent from section 2 |
| `rao_validators.RAODataValidators.get_active_dofs_for_heading` | code → pack: absent from section 2 |
| `unified_rao_reader.RAOReaderError` | code → pack: absent from section 2 |
| `unified_rao_reader.RAOReaderError.get_user_message` | code → pack: absent from section 2 |
| `unified_rao_reader.UnifiedRAOReader` | code → pack: absent from section 2 |
| `unified_rao_reader.UnifiedRAOReader.read` | code → pack: absent from section 2 |
| `unified_rao_reader.UnifiedRAOReader.read_aqwa_lis` | code → pack: absent from section 2 |
| `unified_rao_reader.UnifiedRAOReader.read_orcaflex_yml` | code → pack: absent from section 2 |
| `unified_rao_reader.UnifiedRAOReader.get_info` | code → pack: absent from section 2 |
| `unified_rao_reader.read_rao_file` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `src/digitalmodel/marine_ops/marine_analysis/catenary/adapter.py:L35-L408` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L434-L508` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L80` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L81` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L279-L342` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 34. `src/digitalmodel/marine_ops/marine_engineering/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 16.

| Function | Direction |
| --- | --- |
| `calm_buoy_fatigue.SNcurve` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.SNcurve.cycles_to_failure` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.OrcaFlexTensionReader` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.OrcaFlexTensionReader.read_csv` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.OrcaFlexTensionReader.read_xlsx` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.OrcaFlexTensionReader.parse_line_name` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.RainflowFatigue` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.RainflowFatigue.count_cycles` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.MinersRuleDamage` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.MinersRuleDamage.compute` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.ScatterDiagramFatigue` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.ScatterDiagramFatigue.compute` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.MooringLineResult` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.FatigueLifeReport.build` | code → pack: absent from section 2 |
| `calm_buoy_fatigue.compute_fatigue_life` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 35. `src/digitalmodel/marine_ops/reservoir/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 84.

| Function | Direction |
| --- | --- |
| `analysis.LogCurve.get_valid_data` | code → pack: absent from section 2 |
| `analysis.LogCurve.interpolate_to_depth` | code → pack: absent from section 2 |
| `analysis.StratigraphicLayer` | code → pack: absent from section 2 |
| `analysis.StratigraphicLayer.thickness` | code → pack: absent from section 2 |
| `analysis.StratigraphicLayer.contains_depth` | code → pack: absent from section 2 |
| `analysis.LogAnalysis.add_curve` | code → pack: absent from section 2 |
| `analysis.LogAnalysis.add_formation` | code → pack: absent from section 2 |
| `analysis.LogAnalysis.calculate_facies_from_logs` | code → pack: absent from section 2 |
| `analysis.LogAnalysis.calculate_porosity_from_logs` | code → pack: absent from section 2 |
| `analysis.LogAnalysis.calculate_water_saturation` | code → pack: absent from section 2 |
| `analysis.LogAnalysis.calculate_net_pay` | code → pack: absent from section 2 |
| `analysis.StratigraphicAnalysis` | code → pack: absent from section 2 |
| `analysis.StratigraphicAnalysis.add_well_analysis` | code → pack: absent from section 2 |
| `analysis.StratigraphicAnalysis.set_well_order` | code → pack: absent from section 2 |
| `analysis.StratigraphicAnalysis.add_correlation_marker` | code → pack: absent from section 2 |
| `analysis.StratigraphicAnalysis.create_flattened_section` | code → pack: absent from section 2 |
| `analysis.StratigraphicAnalysis.plot_stratigraphic_section` | code → pack: absent from section 2 |
| `analysis.WellCorrelation` | code → pack: absent from section 2 |
| `analysis.WellCorrelation.add_well_data` | code → pack: absent from section 2 |
| `analysis.WellCorrelation.add_structural_surface` | code → pack: absent from section 2 |
| `analysis.WellCorrelation.create_structure_map` | code → pack: absent from section 2 |
| `analysis.ReservoirMapping` | code → pack: absent from section 2 |
| `analysis.ReservoirMapping.add_property_data` | code → pack: absent from section 2 |
| `analysis.ReservoirMapping.create_property_map` | code → pack: absent from section 2 |
| `analysis.PerformanceAnalysis` | code → pack: absent from section 2 |
| `analysis.PerformanceAnalysis.add_production_data` | code → pack: absent from section 2 |
| `analysis.PerformanceAnalysis.calculate_eur_by_analogy` | code → pack: absent from section 2 |
| `analysis.CoreAnalysis` | code → pack: absent from section 2 |
| `analysis.CoreAnalysis.add_core_data` | code → pack: absent from section 2 |
| `analysis.CoreAnalysis.correlate_core_to_logs` | code → pack: absent from section 2 |
| `analysis.ReservoirAnalysis` | code → pack: absent from section 2 |
| `analysis.ReservoirAnalysis.add_well_analysis` | code → pack: absent from section 2 |
| `analysis.ReservoirAnalysis.run_comprehensive_analysis` | code → pack: absent from section 2 |
| `analysis.ReservoirAnalysis.export_analysis_report` | code → pack: absent from section 2 |
| `legacy_migration.migrate_legacy_stratigraphic_plot` | code → pack: absent from section 2 |
| `legacy_migration.extract_legacy_data_patterns` | code → pack: absent from section 2 |
| `legacy_migration.create_migration_report` | code → pack: absent from section 2 |
| `modeling.MaterialBalance` | code → pack: absent from section 2 |
| `modeling.MaterialBalance.add_production_data` | code → pack: absent from section 2 |
| `modeling.MaterialBalance.add_pressure_data` | code → pack: absent from section 2 |
| `modeling.MaterialBalance.calculate_cumulative_production` | code → pack: absent from section 2 |
| `modeling.MaterialBalance.tank_material_balance` | code → pack: absent from section 2 |
| `modeling.WellTestAnalysis` | code → pack: absent from section 2 |
| `modeling.WellTestAnalysis.add_test_data` | code → pack: absent from section 2 |
| `modeling.WellTestAnalysis.analyze_buildup_test` | code → pack: absent from section 2 |
| `modeling.ReservoirSimulation` | code → pack: absent from section 2 |
| `modeling.ReservoirSimulation.setup_grid` | code → pack: absent from section 2 |
| `modeling.ReservoirSimulation.run_depletion_simulation` | code → pack: absent from section 2 |
| `modeling.ProductionForecast` | code → pack: absent from section 2 |
| `modeling.ProductionForecast.add_production_data` | code → pack: absent from section 2 |
| `modeling.ProductionForecast.arps_decline_analysis` | code → pack: absent from section 2 |
| `modeling.ProductionForecast.forecast_production` | code → pack: absent from section 2 |
| `modeling.ReservoirCharacterization` | code → pack: absent from section 2 |
| `modeling.ReservoirCharacterization.add_well` | code → pack: absent from section 2 |
| `modeling.ReservoirCharacterization.add_log_data` | code → pack: absent from section 2 |
| `modeling.ReservoirCharacterization.calculate_net_pay` | code → pack: absent from section 2 |
| `modeling.ReservoirCharacterization.generate_property_maps` | code → pack: absent from section 2 |
| `modeling.ReservoirModel` | code → pack: absent from section 2 |
| `modeling.ReservoirModel.add_well` | code → pack: absent from section 2 |
| `modeling.ReservoirModel.add_well_test` | code → pack: absent from section 2 |
| `modeling.ReservoirModel.run_integrated_analysis` | code → pack: absent from section 2 |
| `modeling.ReservoirModel.export_summary` | code → pack: absent from section 2 |
| `properties.RockProperties.calculate_bulk_density` | code → pack: absent from section 2 |
| `properties.RockProperties.calculate_formation_factor` | code → pack: absent from section 2 |
| `properties.FluidProperties.api_to_specific_gravity` | code → pack: absent from section 2 |
| `properties.FluidProperties.calculate_oil_density_from_api` | code → pack: absent from section 2 |
| `properties.PVTProperties.calculate_gas_expansion_factor` | code → pack: absent from section 2 |
| `properties.ReservoirProperties.calculate_pore_volume` | code → pack: absent from section 2 |
| `properties.ReservoirProperties.calculate_hydrocarbon_pore_volume` | code → pack: absent from section 2 |
| `properties.ReservoirProperties.calculate_ooip` | code → pack: absent from section 2 |
| `properties.ReservoirProperties.calculate_ogip` | code → pack: absent from section 2 |
| `properties.calculate_porosity` | code → pack: absent from section 2 |
| `properties.calculate_permeability` | code → pack: absent from section 2 |
| `properties.calculate_water_saturation` | code → pack: absent from section 2 |
| `properties.create_sandstone_reservoir` | code → pack: absent from section 2 |
| `properties.create_carbonate_reservoir` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 36. `src/digitalmodel/marine_ops/vessel_db/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 50.

| Function | Direction |
| --- | --- |
| `audit.Conflict` | code → pack: absent from section 2 |
| `audit.AuditReport` | code → pack: absent from section 2 |
| `audit.audit_uncited` | code → pack: absent from section 2 |
| `audit.audit_cross_source_conflicts` | code → pack: absent from section 2 |
| `audit.audit_confidence_gaps` | code → pack: absent from section 2 |
| `audit.run_audit` | code → pack: absent from section 2 |
| `audit.build_audit_report` | code → pack: absent from section 2 |
| `confidence.ConfidenceTier` | code → pack: absent from section 2 |
| `confidence.ConfidenceTier.score` | code → pack: absent from section 2 |
| `confidence.field_confidence` | code → pack: absent from section 2 |
| `confidence.RecordConfidence` | code → pack: absent from section 2 |
| `confidence.record_confidence` | code → pack: absent from section 2 |
| `confidence.CapabilityScore` | code → pack: absent from section 2 |
| `confidence.capability_score` | code → pack: absent from section 2 |
| `gyradii.GyradiiEstimate` | code → pack: absent from section 2 |
| `gyradii.GyradiiEstimate.as_field` | code → pack: absent from section 2 |
| `gyradii.estimate_gyradii` | code → pack: absent from section 2 |
| `hulls_adapter.resolve_rao_library_dir` | code → pack: absent from section 2 |
| `hulls_adapter.parse_heading_block_rao` | code → pack: absent from section 2 |
| `hulls_adapter.read_rao_workbook` | code → pack: absent from section 2 |
| `hulls_adapter.real_rao_datasets` | code → pack: absent from section 2 |
| `hulls_adapter.library_summary` | code → pack: absent from section 2 |
| `loader.vessels_dir` | code → pack: absent from section 2 |
| `loader.parse_value` | code → pack: absent from section 2 |
| `loader.Record.vessel_id` | code → pack: absent from section 2 |
| `loader.Record.cited_fields` | code → pack: absent from section 2 |
| `loader.Record.dimension` | code → pack: absent from section 2 |
| `loader.load_dataset` | code → pack: absent from section 2 |
| `loader.iter_records` | code → pack: absent from section 2 |
| `loader.ProvenanceViolation` | code → pack: absent from section 2 |
| `loader.validate_provenance` | code → pack: absent from section 2 |
| `loader.load_crane_curves` | code → pack: absent from section 2 |
| `loader.normalize_vessel_name` | code → pack: absent from section 2 |
| `loader.installation_vessels` | code → pack: absent from section 2 |
| `promote.build_particulars` | code → pack: absent from section 2 |
| `promote.build_crane_summary` | code → pack: absent from section 2 |
| `promote.build_rao_index` | code → pack: absent from section 2 |
| `promote.build_metocean` | code → pack: absent from section 2 |
| `wed_adapter.resolve_wed_curated_dir` | code → pack: absent from section 2 |
| `wed_adapter.construction_vessels` | code → pack: absent from section 2 |
| `wed_adapter.drilling_rigs` | code → pack: absent from section 2 |
| `wed_adapter.construction_crane_vessels` | code → pack: absent from section 2 |
| `wed_adapter.fleet_summary` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 37. `src/digitalmodel/materials/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 64.

| Function | Direction |
| --- | --- |
| `database.MaterialDatabase` | code → pack: absent from section 2 |
| `database.MaterialDatabase.source_digest_matches` | code → pack: absent from section 2 |
| `database.MaterialDatabase.lookup` | code → pack: absent from section 2 |
| `database.normalised_source_digest` | code → pack: absent from section 2 |
| `database.load_database` | code → pack: absent from section 2 |
| `ffs_material_library.TrueStressStrainCurve.yield_strain` | code → pack: absent from section 2 |
| `ffs_material_library.TrueStressStrainCurve.sigma_u_true_mpa` | code → pack: absent from section 2 |
| `ffs_material_library.TrueStressStrainCurve.plastic_strain_at_ultimate` | code → pack: absent from section 2 |
| `ffs_material_library.TrueStressStrainCurve.stress_from_plastic_strain` | code → pack: absent from section 2 |
| `ffs_material_library.TrueStressStrainCurve.table` | code → pack: absent from section 2 |
| `ffs_material_library.TrueStressStrainCurve.plastic_table` | code → pack: absent from section 2 |
| `ffs_material_library.FFSMaterialInputs` | code → pack: absent from section 2 |
| `ffs_material_library.FFSMaterialInputs.with_properties` | code → pack: absent from section 2 |
| `ffs_material_library.FFSMaterialInputs.true_stress_strain_curve` | code → pack: absent from section 2 |
| `ffs_material_library.FFSMaterialInputs.lower_bound_toughness` | code → pack: absent from section 2 |
| `ffs_material_library.lookup_grade` | code → pack: absent from section 2 |
| `grades.MaterialGrade.smys_psi` | code → pack: absent from section 2 |
| `grades.MaterialGrade.smts_psi` | code → pack: absent from section 2 |
| `grades.all_grades` | code → pack: absent from section 2 |
| `grades.smys_mpa` | code → pack: absent from section 2 |
| `grades.smys_psi` | code → pack: absent from section 2 |
| `grades.smts_psi` | code → pack: absent from section 2 |
| `grades.legacy_smys_psi_dict` | code → pack: absent from section 2 |
| `grades.legacy_smts_psi_dict` | code → pack: absent from section 2 |
| `grades.smys_kn_m2` | code → pack: absent from section 2 |
| `grades.smts_kn_m2` | code → pack: absent from section 2 |
| `grades.smys_pa` | code → pack: absent from section 2 |
| `grades.smts_pa` | code → pack: absent from section 2 |
| `grades.legacy_smys_table_mpa` | code → pack: absent from section 2 |
| `grades.legacy_pipe_db_grades_mpa` | code → pack: absent from section 2 |
| `grades.legacy_pipe_db_grades_pa` | code → pack: absent from section 2 |
| `grades.legacy_riser_pipe_grades_mpa` | code → pack: absent from section 2 |
| `grades.legacy_pipeline_material_library_mpa` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.id_in` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.d_over_t` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.metal_area_in2` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.bore_area_in2` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.od_mm` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.wt_mm` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.id_mm` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.weight_kg_m` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.weight_lb_ft` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.internal_volume_m3_per_m` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.internal_volume_bbl_per_ft` | code → pack: absent from section 2 |
| `line_pipe.LinePipe.api_5l_reference` | code → pack: absent from section 2 |
| `line_pipe.list_nps` | code → pack: absent from section 2 |
| `line_pipe.od_for_nps` | code → pack: absent from section 2 |
| `line_pipe.available_schedules` | code → pack: absent from section 2 |
| `line_pipe.wt_for_schedule` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 38. `src/digitalmodel/mooring_resilience/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 8.

| Function | Direction |
| --- | --- |
| `screening.ResilienceFactors` | code → pack: absent from section 2 |
| `screening.ResilienceResult` | code → pack: absent from section 2 |
| `screening.safety_factor_citations` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 39. `src/digitalmodel/motion_forecast/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 83.

| Function | Direction |
| --- | --- |
| `conventions.RAOSource` | code → pack: absent from section 2 |
| `conventions.normalize_phase_deg` | code → pack: absent from section 2 |
| `conventions.heading_going_to_deg` | code → pack: absent from section 2 |
| `criteria.Criterion.warning_factor` | code → pack: absent from section 2 |
| `criteria.load_criteria` | code → pack: absent from section 2 |
| `decision.RollingDecision.display` | code → pack: absent from section 2 |
| `decision.rolling_decision` | code → pack: absent from section 2 |
| `derived.vertical_velocity_magnitude` | code → pack: absent from section 2 |
| `derived.vertical_excursion_magnitude` | code → pack: absent from section 2 |
| `derived.heave_velocity_magnitude` | code → pack: absent from section 2 |
| `derived.inclination_deg` | code → pack: absent from section 2 |
| `derived.available_governing` | code → pack: absent from section 2 |
| `derived.compute_governing` | code → pack: absent from section 2 |
| `feedback.operability_summary` | code → pack: absent from section 2 |
| `measured.MeasuredMotion` | code → pack: absent from section 2 |
| `measured.MeasuredMotion.now` | code → pack: absent from section 2 |
| `measured.MeasuredMotion.at_now` | code → pack: absent from section 2 |
| `measured.MeasuredMotion.window` | code → pack: absent from section 2 |
| `measured_source.MeasuredMotionSource` | code → pack: absent from section 2 |
| `measured_source.MeasuredMotionSource.read` | code → pack: absent from section 2 |
| `measured_source.SyntheticMMS` | code → pack: absent from section 2 |
| `measured_source.SyntheticMMS.read` | code → pack: absent from section 2 |
| `measured_source.from_csv` | code → pack: absent from section 2 |
| `models.WaveForecast.amplitudes` | code → pack: absent from section 2 |
| `models.MotionForecast.as_arrays` | code → pack: absent from section 2 |
| `rao_adapter.GridRAO` | code → pack: absent from section 2 |
| `rao_adapter.GridRAO.from_unified` | code → pack: absent from section 2 |
| `rao_adapter.GridRAO.from_file` | code → pack: absent from section 2 |
| `rao_adapter.GridRAO.H` | code → pack: absent from section 2 |
| `rao_adapter.AnalyticRAO` | code → pack: absent from section 2 |
| `rao_adapter.AnalyticRAO.H` | code → pack: absent from section 2 |
| `recalibrate.Correction` | code → pack: absent from section 2 |
| `recalibrate.Correction.apply` | code → pack: absent from section 2 |
| `recalibrate.fit_correction` | code → pack: absent from section 2 |
| `recalibrate.HoldoutResult` | code → pack: absent from section 2 |
| `recalibrate.holdout_report` | code → pack: absent from section 2 |
| `reconcile.seam_offset` | code → pack: absent from section 2 |
| `reconcile.DofError` | code → pack: absent from section 2 |
| `reconcile.overlap_residuals` | code → pack: absent from section 2 |
| `reconcile.overlap_error` | code → pack: absent from section 2 |
| `reconcile.MeasuredDecision.display` | code → pack: absent from section 2 |
| `reconcile.measured_status` | code → pack: absent from section 2 |
| `reconstruct.wavenumber` | code → pack: absent from section 2 |
| `reconstruct.reconstruct_motion` | code → pack: absent from section 2 |
| `reconstruct.vertical_motion_at` | code → pack: absent from section 2 |
| `reconstruct.time_derivative` | code → pack: absent from section 2 |
| `skill.SkillRecord` | code → pack: absent from section 2 |
| `skill.error_vs_lead_time` | code → pack: absent from section 2 |
| `skill.aggregate_skill` | code → pack: absent from section 2 |
| `top_tension.LumpedLine.k` | code → pack: absent from section 2 |
| `top_tension.LumpedLine.c` | code → pack: absent from section 2 |
| `top_tension.LumpedLine.wn` | code → pack: absent from section 2 |
| `top_tension.displacement_transmissibility` | code → pack: absent from section 2 |
| `top_tension.force_transmissibility` | code → pack: absent from section 2 |
| `top_tension.dynamic_tension` | code → pack: absent from section 2 |
| `wave_forecast.coherence_horizon` | code → pack: absent from section 2 |
| `wave_forecast.dpz_horizon` | code → pack: absent from section 2 |
| `wave_forecast.directional_spread` | code → pack: absent from section 2 |
| `wave_forecast.synthesize_directional_forecast` | code → pack: absent from section 2 |
| `wave_forecast.surface_elevation` | code → pack: absent from section 2 |
| `wave_forecast.WaveFieldSource` | code → pack: absent from section 2 |
| `wave_forecast.WaveFieldSource.read` | code → pack: absent from section 2 |
| `wave_forecast.SyntheticWaveField` | code → pack: absent from section 2 |
| `wave_forecast.SyntheticWaveField.read` | code → pack: absent from section 2 |
| `wave_source.jonswap_spectrum` | code → pack: absent from section 2 |
| `wave_source.synthesize_forecast` | code → pack: absent from section 2 |
| `workflow.generic_vessel_rao` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 40. `src/digitalmodel/naval_architecture/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 517.

| Function | Direction |
| --- | --- |
| `appendage_submergence.Attitude` | code → pack: absent from section 2 |
| `appendage_submergence.Attitude.draft_mean_m` | code → pack: absent from section 2 |
| `appendage_submergence.Attitude.trim_m` | code → pack: absent from section 2 |
| `appendage_submergence.Attitude.waterline_z` | code → pack: absent from section 2 |
| `appendage_submergence.Attitude.to_dict` | code → pack: absent from section 2 |
| `appendage_submergence.SubmergenceResult` | code → pack: absent from section 2 |
| `appendage_submergence.SubmergenceResult.fully_submerged` | code → pack: absent from section 2 |
| `appendage_submergence.SubmergenceResult.to_dict` | code → pack: absent from section 2 |
| `appendage_submergence.check_submergence` | code → pack: absent from section 2 |
| `appendage_submergence.submergence_report` | code → pack: absent from section 2 |
| `brep_tessellate.BrepTessellationError` | code → pack: absent from section 2 |
| `brep_tessellate.MissingCadKernelError` | code → pack: absent from section 2 |
| `brep_tessellate.NoBrepGeometryError` | code → pack: absent from section 2 |
| `brep_tessellate.has_cad_kernel` | code → pack: absent from section 2 |
| `brep_tessellate.require_cad_kernel` | code → pack: absent from section 2 |
| `brep_tessellate.knot_vector_to_occ` | code → pack: absent from section 2 |
| `brep_tessellate.nurbs_surface_to_occ` | code → pack: absent from section 2 |
| `brep_tessellate.nurbs_curve_to_occ` | code → pack: absent from section 2 |
| `brep_tessellate.TrimFidelity` | code → pack: absent from section 2 |
| `brep_tessellate.TrimFidelity.max_overshoot` | code → pack: absent from section 2 |
| `brep_tessellate.TrimFidelity.max_trim_deviation` | code → pack: absent from section 2 |
| `brep_tessellate.TrimFidelity.within_reference_box` | code → pack: absent from section 2 |
| `brep_tessellate.TrimFidelity.describe` | code → pack: absent from section 2 |
| `brep_tessellate.BrepTessellation` | code → pack: absent from section 2 |
| `brep_tessellate.BrepTessellation.triangle_count` | code → pack: absent from section 2 |
| `brep_tessellate.tessellate_patches` | code → pack: absent from section 2 |
| `brep_tessellate.tessellate_brep` | code → pack: absent from section 2 |
| `brep_tessellate.tessellate_3dm` | code → pack: absent from section 2 |
| `compliance.check_imo_intact` | code → pack: absent from section 2 |
| `compliance.check_min_freeboard` | code → pack: absent from section 2 |
| `compliance.check_beam_draft_ratio` | code → pack: absent from section 2 |
| `compliance.check_length_beam_ratio` | code → pack: absent from section 2 |
| `compliance.check_draft_depth_ratio` | code → pack: absent from section 2 |
| `compliance.check_section_modulus` | code → pack: absent from section 2 |
| `compliance.check_displacement_lwl_ratio` | code → pack: absent from section 2 |
| `compliance.check_propeller_immersion` | code → pack: absent from section 2 |
| `compliance.check_max_trim` | code → pack: absent from section 2 |
| `compliance.run_compliance_checks` | code → pack: absent from section 2 |
| `curves_of_form.displacement_at_draft` | code → pack: absent from section 2 |
| `damage_stability.interpolate_gz` | code → pack: absent from section 2 |
| `damage_stability.gz_area_under_curve` | code → pack: absent from section 2 |
| `damage_stability.angle_of_max_gz` | code → pack: absent from section 2 |
| `damage_stability.check_imo_intact_stability` | code → pack: absent from section 2 |
| `damage_stability.lost_buoyancy_sinkage` | code → pack: absent from section 2 |
| `damage_stability.flooded_gm` | code → pack: absent from section 2 |
| `damage_stability_screening.FloodedCompartment` | code → pack: absent from section 2 |
| `damage_stability_screening.FloodedCompartment.floodwater_t` | code → pack: absent from section 2 |
| `damage_stability_screening.DirectDamageCondition` | code → pack: absent from section 2 |
| `damage_stability_screening.DamagedEquilibrium` | code → pack: absent from section 2 |
| `damage_stability_screening.added_weight_equilibrium` | code → pack: absent from section 2 |
| `damage_stability_screening.DamageGZAnalysis` | code → pack: absent from section 2 |
| `damage_stability_screening.damage_gz_analysis` | code → pack: absent from section 2 |
| `damage_stability_screening.DamageCriteria` | code → pack: absent from section 2 |
| `damage_stability_screening.DamageCaseResult` | code → pack: absent from section 2 |
| `damage_stability_screening.DamageCaseResult.passed` | code → pack: absent from section 2 |
| `damage_stability_screening.evaluate_damage_case` | code → pack: absent from section 2 |
| `depth_validity.turning_depth_warning` | code → pack: absent from section 2 |
| `floating_platform_stability.PlatformType` | code → pack: absent from section 2 |
| `floating_platform_stability.StabilityCriteria` | code → pack: absent from section 2 |
| `floating_platform_stability.StabilityResult` | code → pack: absent from section 2 |
| `floating_platform_stability.WindHeelResult` | code → pack: absent from section 2 |
| `floating_platform_stability.compute_gm` | code → pack: absent from section 2 |
| `floating_platform_stability.compute_gz_curve` | code → pack: absent from section 2 |
| `floating_platform_stability.compute_area_under_gz` | code → pack: absent from section 2 |
| `floating_platform_stability.compute_wind_heel` | code → pack: absent from section 2 |
| `floating_platform_stability.check_intact_stability` | code → pack: absent from section 2 |
| `friction_scaling.Fluid` | code → pack: absent from section 2 |
| `friction_scaling.reynolds_number_si` | code → pack: absent from section 2 |
| `friction_scaling.froude_number` | code → pack: absent from section 2 |
| `friction_scaling.ittc57_cf` | code → pack: absent from section 2 |
| `friction_scaling.ittc57_cf_cited` | code → pack: absent from section 2 |
| `friction_scaling.AllowanceTerm` | code → pack: absent from section 2 |
| `friction_scaling.UnresolvedCitation` | code → pack: absent from section 2 |
| `friction_scaling.TransferResult` | code → pack: absent from section 2 |
| `friction_scaling.transfer_model_to_ship` | code → pack: absent from section 2 |
| `friction_scaling.transfer_ship_to_model` | code → pack: absent from section 2 |
| `fundamentals.mass_to_weight_cited` | code → pack: absent from section 2 |
| `fundamentals.displaced_volume_to_weight_lt` | code → pack: absent from section 2 |
| `fundamentals.interpolate_water_density` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult.kxx` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult.kyy` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult.kzz` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult.ixx` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult.iyy` | code → pack: absent from section 2 |
| `gyradius.GyradiusResult.izz` | code → pack: absent from section 2 |
| `gyradius.gyradius_for_platform_type` | code → pack: absent from section 2 |
| `gyradius.gyradius_from_inertia` | code → pack: absent from section 2 |
| `gyradius.estimate_inertia_from_gyradius` | code → pack: absent from section 2 |
| `gyradius.gyradius_for_fleet_vessel` | code → pack: absent from section 2 |
| `holtrop_coefficients.length_of_run` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c12` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c13` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c7` | code → pack: absent from section 2 |
| `holtrop_coefficients.half_angle_of_entrance` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c1` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c3` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c2` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c4` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c5` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c16` | code → pack: absent from section 2 |
| `holtrop_coefficients.coeff_c15` | code → pack: absent from section 2 |
| `holtrop_coefficients.exponent_m1` | code → pack: absent from section 2 |
| `holtrop_coefficients.lambda_wave` | code → pack: absent from section 2 |
| `holtrop_mennen.form_factor_k1` | code → pack: absent from section 2 |
| `holtrop_mennen.wetted_surface_holtrop` | code → pack: absent from section 2 |
| `holtrop_mennen.frictional_resistance` | code → pack: absent from section 2 |
| `holtrop_mennen.wave_resistance` | code → pack: absent from section 2 |
| `holtrop_mennen.appendage_resistance` | code → pack: absent from section 2 |
| `holtrop_mennen.bulbous_bow_resistance` | code → pack: absent from section 2 |
| `holtrop_mennen.transom_resistance` | code → pack: absent from section 2 |
| `holtrop_mennen.correlation_allowance` | code → pack: absent from section 2 |
| `holtrop_mennen.total_resistance` | code → pack: absent from section 2 |
| `holtrop_mennen.effective_power` | code → pack: absent from section 2 |
| `hull_cap.TornBoundaryError` | code → pack: absent from section 2 |
| `hull_cap.CapBelowWaterlineError` | code → pack: absent from section 2 |
| `hull_cap.CapTriangulationError` | code → pack: absent from section 2 |
| `hull_cap.BoundaryLoop` | code → pack: absent from section 2 |
| `hull_cap.BoundaryLoop.n_vertices` | code → pack: absent from section 2 |
| `hull_cap.BoundaryLoop.z_min` | code → pack: absent from section 2 |
| `hull_cap.BoundaryLoop.z_max` | code → pack: absent from section 2 |
| `hull_cap.BoundaryLoop.bbox` | code → pack: absent from section 2 |
| `hull_cap.BoundaryLoop.describe` | code → pack: absent from section 2 |
| `hull_cap.find_boundary_loops` | code → pack: absent from section 2 |
| `hull_cap.triangulate_loop` | code → pack: absent from section 2 |
| `hull_cap.CapResult` | code → pack: absent from section 2 |
| `hull_cap.CapResult.n_caps` | code → pack: absent from section 2 |
| `hull_cap.CapResult.n_cap_triangles` | code → pack: absent from section 2 |
| `hull_cap.CapResult.cap_area` | code → pack: absent from section 2 |
| `hull_cap.CapResult.loop_records` | code → pack: absent from section 2 |
| `hull_cap.cap_boundary_loops` | code → pack: absent from section 2 |
| `hull_fixtures.box_mesh` | code → pack: absent from section 2 |
| `hull_fixtures.wigley_half_breadth` | code → pack: absent from section 2 |
| `hull_fixtures.wigley_mesh` | code → pack: absent from section 2 |
| `hull_fixtures.wigley_wetted_area_reference` | code → pack: absent from section 2 |
| `hull_form.RigHullEstimate` | code → pack: absent from section 2 |
| `hull_form.RigHullValidation` | code → pack: absent from section 2 |
| `hull_form.rig_type_to_hull_form` | code → pack: absent from section 2 |
| `hull_form.estimate_rig_hull_dimensions` | code → pack: absent from section 2 |
| `hull_form.classify_rig_hull_geometry` | code → pack: absent from section 2 |
| `hull_form.validate_drilling_rig_hull_form` | code → pack: absent from section 2 |
| `hull_form.validate_drilling_rig_fleet` | code → pack: absent from section 2 |
| `hull_form.summarize_drilling_rig_hull_validation` | code → pack: absent from section 2 |
| `hull_form.block_coefficient` | code → pack: absent from section 2 |
| `hull_form.prismatic_coefficient` | code → pack: absent from section 2 |
| `hull_form.midship_coefficient` | code → pack: absent from section 2 |
| `hull_form.waterplane_coefficient` | code → pack: absent from section 2 |
| `hull_form.displacement_from_cb` | code → pack: absent from section 2 |
| `hull_form.froude_number` | code → pack: absent from section 2 |
| `hull_form.wetted_surface_denny_mumford` | code → pack: absent from section 2 |
| `hull_form.series_60_cr` | code → pack: absent from section 2 |
| `hull_form.lcb_from_cb` | code → pack: absent from section 2 |
| `hull_girder_screening.DistributedWeight` | code → pack: absent from section 2 |
| `hull_girder_screening.DistributedWeight.mid_m` | code → pack: absent from section 2 |
| `hull_girder_screening.DistributedWeight.density_at` | code → pack: absent from section 2 |
| `hull_girder_screening.PointWeight` | code → pack: absent from section 2 |
| `hull_girder_screening.HydroStation` | code → pack: absent from section 2 |
| `hull_girder_screening.HydroStation.area_at` | code → pack: absent from section 2 |
| `hull_girder_screening.AllowableCurve` | code → pack: absent from section 2 |
| `hull_girder_screening.AllowableCurve.limit_for` | code → pack: absent from section 2 |
| `hull_girder_screening.SectionElement` | code → pack: absent from section 2 |
| `hull_girder_screening.SectionElement.horizontal` | code → pack: absent from section 2 |
| `hull_girder_screening.SectionElement.vertical` | code → pack: absent from section 2 |
| `hull_girder_screening.StillWaterResult` | code → pack: absent from section 2 |
| `hull_girder_screening.FrameUtilization` | code → pack: absent from section 2 |
| `hull_girder_screening.WaveLoads` | code → pack: absent from section 2 |
| `hull_girder_screening.TotalFrameUtilization` | code → pack: absent from section 2 |
| `hull_girder_screening.SectionModulusScreen` | code → pack: absent from section 2 |
| `hull_girder_screening.build_weight_curve` | code → pack: absent from section 2 |
| `hull_girder_screening.condition_totals` | code → pack: absent from section 2 |
| `hull_girder_screening.buoyancy_box` | code → pack: absent from section 2 |
| `hull_girder_screening.solve_waterline` | code → pack: absent from section 2 |
| `hull_girder_screening.buoyancy_hydrostatics_table` | code → pack: absent from section 2 |
| `hull_girder_screening.buoyancy_direct` | code → pack: absent from section 2 |
| `hull_girder_screening.still_water_sf_bm` | code → pack: absent from section 2 |
| `hull_girder_screening.utilization_at_frames` | code → pack: absent from section 2 |
| `hull_girder_screening.rule_wave_loads` | code → pack: absent from section 2 |
| `hull_girder_screening.total_utilization_at_frames` | code → pack: absent from section 2 |
| `hull_girder_screening.section_properties` | code → pack: absent from section 2 |
| `hull_girder_screening.section_modulus_screen` | code → pack: absent from section 2 |
| `hull_girder_smith.HullGirderElement` | code → pack: absent from section 2 |
| `hull_girder_smith.HullGirderElement.buckles` | code → pack: absent from section 2 |
| `hull_girder_smith.HullGirderElement.compressive_capacity_mpa` | code → pack: absent from section 2 |
| `hull_girder_smith.HullGirderElement.stress` | code → pack: absent from section 2 |
| `hull_girder_smith.stiffened_plate_element` | code → pack: absent from section 2 |
| `hull_girder_smith.plate_element` | code → pack: absent from section 2 |
| `hull_girder_smith.hard_corner_element` | code → pack: absent from section 2 |
| `hull_girder_smith.section_centroid_m` | code → pack: absent from section 2 |
| `hull_girder_smith.section_modulus_m3` | code → pack: absent from section 2 |
| `hull_girder_smith.MomentCurvaturePoint` | code → pack: absent from section 2 |
| `hull_girder_smith.SmithResult` | code → pack: absent from section 2 |
| `hull_girder_smith.smith_ultimate` | code → pack: absent from section 2 |
| `hull_girder_strength.wave_coefficient` | code → pack: absent from section 2 |
| `hull_girder_strength.rule_block_coefficient` | code → pack: absent from section 2 |
| `hull_girder_strength.bending_distribution_factor` | code → pack: absent from section 2 |
| `hull_girder_strength.WaveBendingMoment` | code → pack: absent from section 2 |
| `hull_girder_strength.wave_bending_moment` | code → pack: absent from section 2 |
| `hull_girder_strength.shear_distribution_factor_f1` | code → pack: absent from section 2 |
| `hull_girder_strength.shear_distribution_factor_f2` | code → pack: absent from section 2 |
| `hull_girder_strength.WaveShearForce` | code → pack: absent from section 2 |
| `hull_girder_strength.wave_shear_force` | code → pack: absent from section 2 |
| `hull_girder_strength.CombinedEnvelope` | code → pack: absent from section 2 |
| `hull_girder_strength.combined_bending_moment` | code → pack: absent from section 2 |
| `hull_girder_strength.material_factor_k` | code → pack: absent from section 2 |
| `hull_girder_strength.permissible_stress` | code → pack: absent from section 2 |
| `hull_girder_strength.SectionModulusCheck` | code → pack: absent from section 2 |
| `hull_girder_strength.bending_stress_mpa` | code → pack: absent from section 2 |
| `hull_girder_strength.section_modulus_check` | code → pack: absent from section 2 |
| `hull_girder_strength.hull_girder_ultimate_moment` | code → pack: absent from section 2 |
| `hull_girder_strength.UltimateStrengthCheck` | code → pack: absent from section 2 |
| `hull_girder_strength.ultimate_strength_check` | code → pack: absent from section 2 |
| `hull_girder_strength.compression_flange_ultimate_stress` | code → pack: absent from section 2 |
| `hull_ingest.HullIngestError` | code → pack: absent from section 2 |
| `hull_ingest.MissingCadDependencyError` | code → pack: absent from section 2 |
| `hull_ingest.NoMeshGeometryError` | code → pack: absent from section 2 |
| `hull_ingest.AmbiguousUnitsError` | code → pack: absent from section 2 |
| `hull_ingest.ImplausibleScaleError` | code → pack: absent from section 2 |
| `hull_ingest.AmbiguousOrientationError` | code → pack: absent from section 2 |
| `hull_ingest.NotWatertightError` | code → pack: absent from section 2 |
| `hull_ingest.UnitDecision` | code → pack: absent from section 2 |
| `hull_ingest.infer_units` | code → pack: absent from section 2 |
| `hull_ingest.OrientationDecision` | code → pack: absent from section 2 |
| `hull_ingest.infer_orientation` | code → pack: absent from section 2 |
| `hull_ingest.weld_vertices` | code → pack: absent from section 2 |
| `hull_ingest.HullTransform` | code → pack: absent from section 2 |
| `hull_ingest.HullTransform.apply` | code → pack: absent from section 2 |
| `hull_ingest.HullTransform.to_dict` | code → pack: absent from section 2 |
| `hull_ingest.HullManifest` | code → pack: absent from section 2 |
| `hull_ingest.HullManifest.to_dict` | code → pack: absent from section 2 |
| `hull_ingest.HullManifest.write` | code → pack: absent from section 2 |
| `hull_ingest.Rhino3dmGeometry` | code → pack: absent from section 2 |
| `hull_ingest.read_3dm_triangles` | code → pack: absent from section 2 |
| `hull_ingest.ingest_triangles` | code → pack: absent from section 2 |
| `hull_ingest.ingest_3dm` | code → pack: absent from section 2 |
| `hull_properties.hull_hydrostatics` | code → pack: absent from section 2 |
| `hull_properties.capacity_plan` | code → pack: absent from section 2 |
| `hull_properties.stability_curve_estimate` | code → pack: absent from section 2 |
| `hull_properties.hull_weight_groups` | code → pack: absent from section 2 |
| `hull_regions.PreparedRegion` | code → pack: absent from section 2 |
| `hull_regions.PreparedRegion.watertight` | code → pack: absent from section 2 |
| `hull_regions.PreparedRegion.bounds` | code → pack: absent from section 2 |
| `hull_regions.prepare_appendage` | code → pack: absent from section 2 |
| `hull_regions.write_region_stls` | code → pack: absent from section 2 |
| `hull_regions.ingest_appendages` | code → pack: absent from section 2 |
| `hydrostatics.buoyant_force_cited` | code → pack: absent from section 2 |
| `hydrostatics.vertical_cg_after_weight_change` | code → pack: absent from section 2 |
| `hydrostatics.transverse_cg_after_weight_shift` | code → pack: absent from section 2 |
| `hydrostatics.gm_from_inclining_experiment` | code → pack: absent from section 2 |
| `integration.simpsons_first_rule` | code → pack: absent from section 2 |
| `kcs_geometry.workshop_grid_dir` | code → pack: absent from section 2 |
| `kcs_geometry.build_kcs_model_surface` | code → pack: absent from section 2 |
| `kcs_geometry.read_tecplot_zones` | code → pack: absent from section 2 |
| `kcs_geometry.orient_consistently` | code → pack: absent from section 2 |
| `kcs_geometry.SurfaceCheck` | code → pack: absent from section 2 |
| `kcs_geometry.SurfaceCheck.closed` | code → pack: absent from section 2 |
| `kcs_geometry.check_surface` | code → pack: absent from section 2 |
| `kcs_geometry.wetted_surface_area` | code → pack: absent from section 2 |
| `kcs_geometry.enclosed_volume` | code → pack: absent from section 2 |
| `kcs_geometry.KcsSurface` | code → pack: absent from section 2 |
| `kcs_geometry.KcsSurface.particulars` | code → pack: absent from section 2 |
| `kcs_geometry.build_kcs_surface` | code → pack: absent from section 2 |
| `kcs_geometry.transform_surface` | code → pack: absent from section 2 |
| `kcs_geometry.write_stl` | code → pack: absent from section 2 |
| `knowledge.find_module` | code → pack: absent from section 2 |
| `knowledge.find_ship_data` | code → pack: absent from section 2 |
| `knowledge.query_knowledge` | code → pack: absent from section 2 |
| `loading_computer.Equilibrium` | code → pack: absent from section 2 |
| `loading_computer.IntactStability` | code → pack: absent from section 2 |
| `loading_computer.DamageStability` | code → pack: absent from section 2 |
| `loading_computer.LongitudinalStrength` | code → pack: absent from section 2 |
| `loading_computer.total_weight_and_centroids` | code → pack: absent from section 2 |
| `loading_computer.solve_equilibrium` | code → pack: absent from section 2 |
| `loading_computer.gz_wall_sided` | code → pack: absent from section 2 |
| `loading_computer.intact_stability` | code → pack: absent from section 2 |
| `loading_computer.longitudinal_strength` | code → pack: absent from section 2 |
| `loading_computer.run_loading_computer` | code → pack: absent from section 2 |
| `loading_computer.router` | code → pack: absent from section 2 |
| `maneuverability.rudder_lift_coefficient` | code → pack: absent from section 2 |
| `maneuverability.rudder_normal_force` | code → pack: absent from section 2 |
| `maneuverability.nomoto_steady_yaw_rate` | code → pack: absent from section 2 |
| `maneuverability.steady_turning_radius` | code → pack: absent from section 2 |
| `maneuverability.directional_stability_criterion` | code → pack: absent from section 2 |
| `maneuverability.is_directionally_stable` | code → pack: absent from section 2 |
| `maneuverability.speed_in_turn` | code → pack: absent from section 2 |
| `maneuverability.drift_angle` | code → pack: absent from section 2 |
| `maneuvering_envelope.LoadingCondition` | code → pack: absent from section 2 |
| `maneuvering_envelope.clarke_derivatives` | code → pack: absent from section 2 |
| `maneuvering_envelope.course_stability_discriminant` | code → pack: absent from section 2 |
| `maneuvering_envelope.rudder_lift_slope_per_rad` | code → pack: absent from section 2 |
| `maneuvering_envelope.steady_turning_radius_over_L` | code → pack: absent from section 2 |
| `maneuvering_envelope.tactical_diameter_over_L` | code → pack: absent from section 2 |
| `maneuvering_envelope.IMOTurningVerdict` | code → pack: absent from section 2 |
| `maneuvering_envelope.imo_turning_compliance` | code → pack: absent from section 2 |
| `maneuvering_envelope.threshold_speed_for_steerage` | code → pack: absent from section 2 |
| `maneuvering_envelope.current_yaw_moment_ocimf` | code → pack: absent from section 2 |
| `maneuvering_envelope.current_lateral_force_ocimf` | code → pack: absent from section 2 |
| `maneuvering_envelope.HeadingHoldResult` | code → pack: absent from section 2 |
| `maneuvering_envelope.rudder_angle_to_hold_heading` | code → pack: absent from section 2 |
| `maneuvering_envelope.critical_current_speed_for_heading` | code → pack: absent from section 2 |
| `maneuvering_envelope.engine_off_rudder_yaw_moment` | code → pack: absent from section 2 |
| `maneuvering_envelope_workflow.ManeuveringEnvelopeInput` | code → pack: absent from section 2 |
| `maneuvering_envelope_workflow.validate_maneuvering_envelope_input` | code → pack: absent from section 2 |
| `maneuvering_envelope_workflow.run_maneuvering_envelope_sweep` | code → pack: absent from section 2 |
| `maneuvering_envelope_workflow.write_maneuvering_envelope_results` | code → pack: absent from section 2 |
| `maneuvering_envelope_workflow.load_packaged_maneuvering_envelope_yaml` | code → pack: absent from section 2 |
| `mesh_clipping.ClippedHull` | code → pack: absent from section 2 |
| `mesh_clipping.ClippedHull.face_areas` | code → pack: absent from section 2 |
| `mesh_clipping.clip_at_waterline` | code → pack: absent from section 2 |
| `mesh_geometry.MeshContractError` | code → pack: absent from section 2 |
| `mesh_geometry.volume_divergence` | code → pack: absent from section 2 |
| `mesh_geometry.volume_tetra` | code → pack: absent from section 2 |
| `mesh_hydrostatics.compute_hydrostatics` | code → pack: absent from section 2 |
| `mesh_results.Quantity` | code → pack: absent from section 2 |
| `mesh_results.Quantity.to_dict` | code → pack: absent from section 2 |
| `mesh_results.HydrostaticsResult` | code → pack: absent from section 2 |
| `mesh_results.HydrostaticsResult.to_dict` | code → pack: absent from section 2 |
| `mesh_sections.SectionIndex` | code → pack: absent from section 2 |
| `mesh_sections.SectionIndex.section` | code → pack: absent from section 2 |
| `mesh_validation.TriMesh.vertices` | code → pack: absent from section 2 |
| `mesh_validation.TriMesh.faces` | code → pack: absent from section 2 |
| `mesh_validation.TriMesh.source_digest` | code → pack: absent from section 2 |
| `mesh_validation.TriMesh.axes` | code → pack: absent from section 2 |
| `mesh_validation.TriMesh.scale_diag` | code → pack: absent from section 2 |
| `mesh_validation.TriMesh.bounds` | code → pack: absent from section 2 |
| `prohaska.ProhaskaFit` | code → pack: absent from section 2 |
| `prohaska.prohaska_form_factor` | code → pack: absent from section 2 |
| `propeller.wageningen_kt` | code → pack: absent from section 2 |
| `propeller.wageningen_kq` | code → pack: absent from section 2 |
| `propeller.advance_angle` | code → pack: absent from section 2 |
| `propeller.reference_velocity` | code → pack: absent from section 2 |
| `propeller.ct_star_from_kt` | code → pack: absent from section 2 |
| `propeller.cq_star_from_kq` | code → pack: absent from section 2 |
| `propeller.kt_from_ct_star` | code → pack: absent from section 2 |
| `propeller.kq_from_cq_star` | code → pack: absent from section 2 |
| `propeller.FourQuadrantPropeller` | code → pack: absent from section 2 |
| `propeller.FourQuadrantPropeller.load_fourier` | code → pack: absent from section 2 |
| `propeller.FourQuadrantPropeller.ct_star` | code → pack: absent from section 2 |
| `propeller.FourQuadrantPropeller.cq_star` | code → pack: absent from section 2 |
| `propeller.FourQuadrantPropeller.thrust` | code → pack: absent from section 2 |
| `propeller.FourQuadrantPropeller.torque` | code → pack: absent from section 2 |
| `propeller.four_quadrant_thrust` | code → pack: absent from section 2 |
| `propeller.four_quadrant_torque` | code → pack: absent from section 2 |
| `region_report.RegionSet` | code → pack: absent from section 2 |
| `region_report.RegionSet.naive_sum_m2` | code → pack: absent from section 2 |
| `region_report.RegionSet.external_m2` | code → pack: absent from section 2 |
| `region_report.RegionSet.uncertainty_m2` | code → pack: absent from section 2 |
| `region_report.RegionSet.double_counted_m2` | code → pack: absent from section 2 |
| `region_report.RegionSet.to_dict` | code → pack: absent from section 2 |
| `region_report.region_set` | code → pack: absent from section 2 |
| `resistance.shaft_horsepower` | code → pack: absent from section 2 |
| `resistance.ittc_1957_cf_cited` | code → pack: absent from section 2 |
| `rudder_stock_torque.RudderStockTorqueResult` | code → pack: absent from section 2 |
| `rudder_stock_torque.RudderStockTorqueInput` | code → pack: absent from section 2 |
| `rudder_stock_torque.rudder_stock_torque` | code → pack: absent from section 2 |
| `rudder_stock_torque.load_packaged_rudder_stock_torque_yaml` | code → pack: absent from section 2 |
| `rudder_stock_torque.load_rudder_stock_torque_input` | code → pack: absent from section 2 |
| `rudder_stock_torque.validate_rudder_stock_torque_input` | code → pack: absent from section 2 |
| `rudder_stock_torque.run_rudder_stock_torque_sweep` | code → pack: absent from section 2 |
| `rudder_stock_torque.write_rudder_stock_torque_results` | code → pack: absent from section 2 |
| `rudder_stock_torque.write_rudder_stock_torque_charts` | code → pack: absent from section 2 |
| `rudder_stock_torque.build_rudder_stock_torque_heatmap_grid` | code → pack: absent from section 2 |
| `scantlings.material_factor_f1` | code → pack: absent from section 2 |
| `scantlings.minimum_plate_thickness` | code → pack: absent from section 2 |
| `scantlings.plate_aspect_factor` | code → pack: absent from section 2 |
| `scantlings.plate_thickness_for_pressure` | code → pack: absent from section 2 |
| `scantlings.required_section_modulus` | code → pack: absent from section 2 |
| `scantlings.check_minimum_plate_thickness` | code → pack: absent from section 2 |
| `scantlings.check_plate_thickness_pressure` | code → pack: absent from section 2 |
| `scantlings.check_stiffener_section_modulus` | code → pack: absent from section 2 |
| `seakeeping.natural_roll_period` | code → pack: absent from section 2 |
| `seakeeping.natural_heave_period` | code → pack: absent from section 2 |
| `seakeeping.natural_heave_period_cited` | code → pack: absent from section 2 |
| `seakeeping.natural_pitch_period` | code → pack: absent from section 2 |
| `seakeeping.encounter_frequency` | code → pack: absent from section 2 |
| `seakeeping.simple_heave_rao` | code → pack: absent from section 2 |
| `seakeeping.motion_sickness_incidence` | code → pack: absent from section 2 |
| `seakeeping.significant_motion` | code → pack: absent from section 2 |
| `ship_data.get_ship` | code → pack: absent from section 2 |
| `ship_data.list_ships` | code → pack: absent from section 2 |
| `ship_data.get_cross_curves` | code → pack: absent from section 2 |
| `ship_data.get_curves_of_form` | code → pack: absent from section 2 |
| `ship_data.normalize_fleet_record` | code → pack: absent from section 2 |
| `ship_data.register_fleet_vessels` | code → pack: absent from section 2 |
| `ship_data.normalize_drilling_rig_record` | code → pack: absent from section 2 |
| `ship_data.register_drilling_rigs` | code → pack: absent from section 2 |
| `ship_data.estimate_vessel_hydrostatics` | code → pack: absent from section 2 |
| `ship_dimensions.default_dimension_template_path` | code → pack: absent from section 2 |
| `ship_dimensions.load_dimension_template` | code → pack: absent from section 2 |
| `ship_dimensions.validate_vessel_entry` | code → pack: absent from section 2 |
| `ship_dimensions.merge_template_into_registry` | code → pack: absent from section 2 |
| `solid_occlusion.SolidIndex` | code → pack: absent from section 2 |
| `solid_occlusion.SolidIndex.bbox_contains` | code → pack: absent from section 2 |
| `solid_occlusion.SolidIndex.contains` | code → pack: absent from section 2 |
| `solid_occlusion.AreaSplit` | code → pack: absent from section 2 |
| `solid_occlusion.AreaSplit.occluded_fraction` | code → pack: absent from section 2 |
| `solid_occlusion.AreaSplit.to_dict` | code → pack: absent from section 2 |
| `solid_occlusion.classify_wetted_area` | code → pack: absent from section 2 |
| `stability.gz_from_cross_curves_cited` | code → pack: absent from section 2 |
| `stability.gz_with_tcg_correction` | code → pack: absent from section 2 |
| `stability.free_surface_correction` | code → pack: absent from section 2 |
| `submarines.mbt_capacity` | code → pack: absent from section 2 |
| `submarines.density_compensation` | code → pack: absent from section 2 |
| `submarines.submerged_bg_after_weight_shift` | code → pack: absent from section 2 |
| `symmetry_cut.HalfBody` | code → pack: absent from section 2 |
| `symmetry_cut.HalfBody.volume_m3` | code → pack: absent from section 2 |
| `symmetry_cut.HalfBody.consistency` | code → pack: absent from section 2 |
| `symmetry_cut.HalfBody.to_dict` | code → pack: absent from section 2 |
| `symmetry_cut.half_body` | code → pack: absent from section 2 |
| `symmetry_cut.PlaneSection` | code → pack: absent from section 2 |
| `symmetry_cut.PlaneSection.closure_error` | code → pack: absent from section 2 |
| `symmetry_cut.PlaneSection.symmetry_error` | code → pack: absent from section 2 |
| `symmetry_cut.PlaneSection.clean` | code → pack: absent from section 2 |
| `symmetry_cut.PlaneSection.to_dict` | code → pack: absent from section 2 |
| `symmetry_cut.plane_section` | code → pack: absent from section 2 |
| `symmetry_cut.SymmetryCutReport` | code → pack: absent from section 2 |
| `symmetry_cut.SymmetryCutReport.ok` | code → pack: absent from section 2 |
| `symmetry_cut.SymmetryCutReport.failures` | code → pack: absent from section 2 |
| `symmetry_cut.SymmetryCutReport.to_dict` | code → pack: absent from section 2 |
| `symmetry_cut.check_symmetry_cut` | code → pack: absent from section 2 |
| `trim_hydrostatics.HullFrame` | code → pack: absent from section 2 |
| `trim_hydrostatics.HullFrame.from_triangles` | code → pack: absent from section 2 |
| `trim_hydrostatics.HullFrame.x_midship` | code → pack: absent from section 2 |
| `trim_hydrostatics.HullFrame.length_m` | code → pack: absent from section 2 |
| `trim_hydrostatics.end_drafts` | code → pack: absent from section 2 |
| `trim_hydrostatics.trim_angle_rad` | code → pack: absent from section 2 |
| `trim_hydrostatics.attitude_for` | code → pack: absent from section 2 |
| `trim_hydrostatics.rotate_for_trim` | code → pack: absent from section 2 |
| `trim_hydrostatics.waterplane_extent` | code → pack: absent from section 2 |
| `trim_hydrostatics.FloatingCondition` | code → pack: absent from section 2 |
| `trim_hydrostatics.FloatingCondition.displacement_t` | code → pack: absent from section 2 |
| `trim_hydrostatics.FloatingCondition.block_coefficient` | code → pack: absent from section 2 |
| `trim_hydrostatics.FloatingCondition.to_dict` | code → pack: absent from section 2 |
| `trim_hydrostatics.hydrostatics_at` | code → pack: absent from section 2 |
| `trim_hydrostatics.hydrostatic_table` | code → pack: absent from section 2 |
| `trim_hydrostatics.condition_matrix` | code → pack: absent from section 2 |
| `turning_circle.TurningCircleInput` | code → pack: absent from section 2 |
| `turning_circle.simulate_nomoto_turning_circle` | code → pack: absent from section 2 |
| `turning_circle.load_packaged_turning_circle_yaml` | code → pack: absent from section 2 |
| `turning_circle.validate_turning_circle_input` | code → pack: absent from section 2 |
| `turning_circle.run_turning_circle_sweep` | code → pack: absent from section 2 |
| `turning_circle_artifacts.units_metadata` | code → pack: absent from section 2 |
| `turning_circle_artifacts.provenance_metadata` | code → pack: absent from section 2 |
| `turning_circle_artifacts.write_turning_circle_results` | code → pack: absent from section 2 |
| `turning_circle_artifacts.write_turning_circle_charts` | code → pack: absent from section 2 |
| `vessel_stability_screening.Citation` | code → pack: absent from section 2 |
| `vessel_stability_screening.Citation.label` | code → pack: absent from section 2 |
| `vessel_stability_screening.HydroRow` | code → pack: absent from section 2 |
| `vessel_stability_screening.HydrostaticProperties` | code → pack: absent from section 2 |
| `vessel_stability_screening.HydrostaticTable` | code → pack: absent from section 2 |
| `vessel_stability_screening.HydrostaticTable.at_displacement` | code → pack: absent from section 2 |
| `vessel_stability_screening.HydrostaticTable.at_draft` | code → pack: absent from section 2 |
| `vessel_stability_screening.rectangular_fsm_t_m` | code → pack: absent from section 2 |
| `vessel_stability_screening.LoadingCondition` | code → pack: absent from section 2 |
| `vessel_stability_screening.build_loading_condition` | code → pack: absent from section 2 |
| `vessel_stability_screening.Equilibrium` | code → pack: absent from section 2 |
| `vessel_stability_screening.solve_equilibrium` | code → pack: absent from section 2 |
| `vessel_stability_screening.GZCurve` | code → pack: absent from section 2 |
| `vessel_stability_screening.GZCurve.gz_at` | code → pack: absent from section 2 |
| `vessel_stability_screening.GZCurve.area_m_rad` | code → pack: absent from section 2 |
| `vessel_stability_screening.GZCurve.max_gz` | code → pack: absent from section 2 |
| `vessel_stability_screening.gz_from_kn` | code → pack: absent from section 2 |
| `vessel_stability_screening.kn_at_displacement` | code → pack: absent from section 2 |
| `vessel_stability_screening.CriterionResult` | code → pack: absent from section 2 |
| `vessel_stability_screening.CriterionResult.margin` | code → pack: absent from section 2 |
| `vessel_stability_screening.ImoIntactCriteria` | code → pack: absent from section 2 |
| `vessel_stability_screening.imo_intact_criteria` | code → pack: absent from section 2 |
| `vessel_stability_screening.WeatherCriterion` | code → pack: absent from section 2 |
| `vessel_stability_screening.weather_criterion_cfr_170_170` | code → pack: absent from section 2 |
| `vessel_stability_screening.CraneHeelingCase` | code → pack: absent from section 2 |
| `vessel_stability_screening.CraneHeelingCase.from_hook_load` | code → pack: absent from section 2 |
| `vessel_stability_screening.CraneHeelingCase.heeling_arm_m` | code → pack: absent from section 2 |
| `vessel_stability_screening.LiftingCriteria` | code → pack: absent from section 2 |
| `vessel_stability_screening.LiftingResult` | code → pack: absent from section 2 |
| `vessel_stability_screening.LiftingResult.passed` | code → pack: absent from section 2 |
| `vessel_stability_screening.equilibrium_heel_deg` | code → pack: absent from section 2 |
| `vessel_stability_screening.lifting_check` | code → pack: absent from section 2 |
| `vessel_stability_screening.KgLimit` | code → pack: absent from section 2 |
| `vessel_stability_screening.max_kg_screening` | code → pack: absent from section 2 |
| `vessel_stability_screening.governing_kg_limit` | code → pack: absent from section 2 |
| `wigley_geometry.WigleyHull` | code → pack: absent from section 2 |
| `wigley_geometry.WigleyHull.height` | code → pack: absent from section 2 |
| `wigley_geometry.WigleyHull.wetted_surface` | code → pack: absent from section 2 |
| `wigley_geometry.WigleyHull.displacement` | code → pack: absent from section 2 |
| `wigley_geometry.wigley_offsets` | code → pack: absent from section 2 |
| `wigley_geometry.wigley_stl` | code → pack: absent from section 2 |
| `wigley_geometry.wigley_manifest` | code → pack: absent from section 2 |
| `wigley_geometry.write_wigley` | code → pack: absent from section 2 |
| `workflow.NavalArchitectureWorkflow` | code → pack: absent from section 2 |
| `workflow.NavalArchitectureWorkflow.router` | code → pack: absent from section 2 |
| `workflow_calculations.run_damage_stability` | code → pack: absent from section 2 |
| `workflow_calculations.run_platform_stability` | code → pack: absent from section 2 |
| `workflow_calculations.run_hull_resistance` | code → pack: absent from section 2 |
| `workflow_calculations.run_hull_seakeeping` | code → pack: absent from section 2 |
| `yaw_moment.YawMomentResult` | code → pack: absent from section 2 |
| `yaw_moment.YawMomentInput` | code → pack: absent from section 2 |
| `yaw_moment.rudder_yaw_moment` | code → pack: absent from section 2 |
| `yaw_moment.load_packaged_typical_ship_yaml` | code → pack: absent from section 2 |
| `yaw_moment.load_yaw_moment_input` | code → pack: absent from section 2 |
| `yaw_moment.validate_yaw_moment_input` | code → pack: absent from section 2 |
| `yaw_moment.run_yaw_moment_sweep` | code → pack: absent from section 2 |
| `yaw_moment.write_yaw_moment_results` | code → pack: absent from section 2 |
| `yaw_moment.write_yaw_moment_charts` | code → pack: absent from section 2 |
| `yaw_moment.build_yaw_moment_heatmap_grid` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 41. `src/digitalmodel/naval_architecture/tank_gauging/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 17.

Section 2 is absent; reconcile the legacy format.

| Function | Direction |
| --- | --- |
| `calibration.finite` | code → pack: absent from section 2 |
| `calibration.positive` | code → pack: absent from section 2 |
| `calibration.bounded_depth` | code → pack: absent from section 2 |
| `calibration.sounding_to_ullage` | code → pack: absent from section 2 |
| `calibration.ullage_to_sounding` | code → pack: absent from section 2 |
| `calibration.CalibrationTable` | code → pack: absent from section 2 |
| `calibration.CalibrationTable.volume_at` | code → pack: absent from section 2 |
| `calibration.CalibrationTable.depth_at_volume` | code → pack: absent from section 2 |
| `calibration.CalibrationTable.depths` | code → pack: absent from section 2 |
| `calibration.CalibrationTable.volumes` | code → pack: absent from section 2 |
| `calibration.CalibrationTable.from_csv` | code → pack: absent from section 2 |
| `calibration.CalibrationTable.from_yaml` | code → pack: absent from section 2 |
| `contents.centre_sounding` | code → pack: absent from section 2 |
| `contents.density_at_temperature` | code → pack: absent from section 2 |
| `contents.liquid_mass_kg` | code → pack: absent from section 2 |
| `free_surface.rectangular_free_surface_moment_kg_m` | code → pack: absent from section 2 |
| `free_surface.gm_loss_m` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 42. `src/digitalmodel/orcaflex/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 254.

| Function | Direction |
| --- | --- |
| `batch_parametric.ParameterSweep` | code → pack: absent from section 2 |
| `batch_parametric.ParameterSweep.get_values` | code → pack: absent from section 2 |
| `batch_parametric.ParametricStudy` | code → pack: absent from section 2 |
| `batch_parametric.ParametricStudy.num_cases` | code → pack: absent from section 2 |
| `batch_parametric.ParametricStudy.generate_case_matrix` | code → pack: absent from section 2 |
| `batch_parametric.ParametricStudy.generate_case_configs` | code → pack: absent from section 2 |
| `batch_parametric.ParametricStudy.create_directory_structure` | code → pack: absent from section 2 |
| `batch_parametric.ParametricStudy.export_case_matrix_csv` | code → pack: absent from section 2 |
| `batch_parametric.CaseResult` | code → pack: absent from section 2 |
| `batch_parametric.ParametricResultsSummary` | code → pack: absent from section 2 |
| `batch_parametric.ParametricResultsSummary.to_dataframe` | code → pack: absent from section 2 |
| `batch_parametric.ParametricResultsSummary.get_critical_case` | code → pack: absent from section 2 |
| `batch_parametric.ParametricResultsSummary.summary_statistics` | code → pack: absent from section 2 |
| `code_check_engine.CodeStandard` | code → pack: absent from section 2 |
| `code_check_engine.SafetyClass` | code → pack: absent from section 2 |
| `code_check_engine.LoadCondition` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2RDInput` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2RDInput.inner_diameter` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2RDInput.steel_area` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2RDInput.plastic_moment` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2RDInput.yield_tension` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2RDResult` | code → pack: absent from section 2 |
| `code_check_engine.check_api_rp_2rd` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input.inner_diameter` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input.t2` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input.characteristic_yield` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input.plastic_moment_resistance` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input.plastic_axial_resistance` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Input.safety_class_factor` | code → pack: absent from section 2 |
| `code_check_engine.DNVOSF201Result` | code → pack: absent from section 2 |
| `code_check_engine.check_dnv_os_f201` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2SKInput` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2SKInput.required_safety_factor` | code → pack: absent from section 2 |
| `code_check_engine.APIRP2SKInput.max_allowable_tension` | code → pack: absent from section 2 |
| `code_check_engine.MooringCodeCheckResult` | code → pack: absent from section 2 |
| `code_check_engine.check_mooring_api_2sk` | code → pack: absent from section 2 |
| `code_check_engine.UtilisationEnvelope` | code → pack: absent from section 2 |
| `code_check_engine.UtilisationEnvelope.max_utilisation` | code → pack: absent from section 2 |
| `code_check_engine.UtilisationEnvelope.max_utilisation_location` | code → pack: absent from section 2 |
| `code_check_engine.UtilisationEnvelope.passes` | code → pack: absent from section 2 |
| `code_check_engine.UtilisationEnvelope.to_dict` | code → pack: absent from section 2 |
| `environment.CurrentProfileType` | code → pack: absent from section 2 |
| `environment.WaveSpectrumType` | code → pack: absent from section 2 |
| `environment.WindProfileType` | code → pack: absent from section 2 |
| `environment.CurrentProfilePoint` | code → pack: absent from section 2 |
| `environment.CurrentProfile.generate_profile` | code → pack: absent from section 2 |
| `environment.CurrentProfile.to_orcaflex_dict` | code → pack: absent from section 2 |
| `environment.WaveSpectrumParams.fp` | code → pack: absent from section 2 |
| `environment.WaveSpectrumParams.tz` | code → pack: absent from section 2 |
| `environment.WaveSpectrumParams.spectral_density` | code → pack: absent from section 2 |
| `environment.WaveSpectrumParams.to_orcaflex_dict` | code → pack: absent from section 2 |
| `environment.WindProfile.speed_at_height` | code → pack: absent from section 2 |
| `environment.WindProfile.generate_profile` | code → pack: absent from section 2 |
| `environment.EnvironmentalLoadCase` | code → pack: absent from section 2 |
| `environment.EnvironmentalMatrix` | code → pack: absent from section 2 |
| `environment.EnvironmentalMatrix.generate_uls_cases` | code → pack: absent from section 2 |
| `environment.EnvironmentalMatrix.generate_als_cases` | code → pack: absent from section 2 |
| `environment.EnvironmentalMatrix.generate_all_cases` | code → pack: absent from section 2 |
| `fowt_mooring_workflow.FOWTMooringWorkflow` | code → pack: absent from section 2 |
| `fowt_mooring_workflow.FOWTMooringWorkflow.router` | code → pack: absent from section 2 |
| `installation_analysis.VesselType` | code → pack: absent from section 2 |
| `installation_analysis.LiftPhase` | code → pack: absent from section 2 |
| `installation_analysis.VesselRAO` | code → pack: absent from section 2 |
| `installation_analysis.VesselRAO.crane_tip_heave` | code → pack: absent from section 2 |
| `installation_analysis.VesselRAO.crane_tip_velocity` | code → pack: absent from section 2 |
| `installation_analysis.DAFInput.natural_period` | code → pack: absent from section 2 |
| `installation_analysis.DAFInput.calculate_daf` | code → pack: absent from section 2 |
| `installation_analysis.SlingConfig.effective_skew_load_factor` | code → pack: absent from section 2 |
| `installation_analysis.SlingConfig.calculate_sling_tension` | code → pack: absent from section 2 |
| `installation_analysis.SplashZoneInput.calculate_splash_zone_loads` | code → pack: absent from section 2 |
| `installation_analysis.WeightItem` | code → pack: absent from section 2 |
| `installation_analysis.WeightManagement` | code → pack: absent from section 2 |
| `installation_analysis.WeightManagement.total_dry_weight` | code → pack: absent from section 2 |
| `installation_analysis.WeightManagement.total_submerged_weight` | code → pack: absent from section 2 |
| `installation_analysis.WeightManagement.total_with_contingency` | code → pack: absent from section 2 |
| `installation_analysis.WeightManagement.calculate_cog` | code → pack: absent from section 2 |
| `installation_analysis.WeightManagement.generate_summary` | code → pack: absent from section 2 |
| `model_builder.ObjectType` | code → pack: absent from section 2 |
| `model_builder.LineEndCondition` | code → pack: absent from section 2 |
| `model_builder.LineType` | code → pack: absent from section 2 |
| `model_builder.LineSectionProperties` | code → pack: absent from section 2 |
| `model_builder.LineSectionProperties.weight_in_water_per_m` | code → pack: absent from section 2 |
| `model_builder.LineSection` | code → pack: absent from section 2 |
| `model_builder.VesselConfig` | code → pack: absent from section 2 |
| `model_builder.VesselConfig.to_orcaflex_dict` | code → pack: absent from section 2 |
| `model_builder.Buoy6DConfig` | code → pack: absent from section 2 |
| `model_builder.Buoy6DConfig.net_buoyancy` | code → pack: absent from section 2 |
| `model_builder.Buoy6DConfig.to_orcaflex_dict` | code → pack: absent from section 2 |
| `model_builder.LineConfig` | code → pack: absent from section 2 |
| `model_builder.LineConfig.total_length` | code → pack: absent from section 2 |
| `model_builder.LineConfig.to_orcaflex_dict` | code → pack: absent from section 2 |
| `model_builder.build_scr_model` | code → pack: absent from section 2 |
| `model_builder.build_lazy_wave_model` | code → pack: absent from section 2 |
| `mooring_design.MooringPattern` | code → pack: absent from section 2 |
| `mooring_design.SegmentMaterial` | code → pack: absent from section 2 |
| `mooring_design.MooringMaterialProperties` | code → pack: absent from section 2 |
| `mooring_design.SyntheticRopeStiffness` | code → pack: absent from section 2 |
| `mooring_design.SyntheticRopeProperties` | code → pack: absent from section 2 |
| `mooring_design.MooringLineSegment` | code → pack: absent from section 2 |
| `mooring_design.MooringLineDesign.total_length` | code → pack: absent from section 2 |
| `mooring_design.MooringLineDesign.estimate_catenary` | code → pack: absent from section 2 |
| `mooring_design.MooringLineDesign.check_mbl` | code → pack: absent from section 2 |
| `mooring_design.MooringLineDesign.get_intact_safety_factor` | code → pack: absent from section 2 |
| `mooring_design.MooringLineDesign.get_damaged_safety_factor` | code → pack: absent from section 2 |
| `mooring_design.MooringLineDesign.check_mbl_with_safety_factor` | code → pack: absent from section 2 |
| `mooring_design.SpreadMooringConfig` | code → pack: absent from section 2 |
| `mooring_design.SpreadMooringConfig.generate_layout` | code → pack: absent from section 2 |
| `mooring_design.TurretMooringConfig` | code → pack: absent from section 2 |
| `mooring_design.TurretMooringConfig.generate_layout` | code → pack: absent from section 2 |
| `mooring_design.estimate_line_length` | code → pack: absent from section 2 |
| `mooring_design.calculate_pretension` | code → pack: absent from section 2 |
| `mooring_design_fowt.MooringType` | code → pack: absent from section 2 |
| `mooring_design_fowt.FloaterType` | code → pack: absent from section 2 |
| `mooring_design_fowt.DynamicCableConfig` | code → pack: absent from section 2 |
| `mooring_design_fowt.WatchCircleResult` | code → pack: absent from section 2 |
| `mooring_design_fowt.WatchCircleConstraint` | code → pack: absent from section 2 |
| `mooring_design_fowt.WatchCircleConstraint.check` | code → pack: absent from section 2 |
| `mooring_design_fowt.check_watch_circle_vs_cable` | code → pack: absent from section 2 |
| `mooring_workflow.MooringMBLWorkflow` | code → pack: absent from section 2 |
| `mooring_workflow.MooringMBLWorkflow.router` | code → pack: absent from section 2 |
| `pipelay_analysis.LayMethod` | code → pack: absent from section 2 |
| `pipelay_analysis.PipeEndCondition` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.validate_annulus` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.inner_diameter` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.total_od` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.moment_of_inertia` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.bending_stiffness` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.steel_area` | code → pack: absent from section 2 |
| `pipelay_analysis.PipelayPipeProperties.submerged_weight_per_m` | code → pack: absent from section 2 |
| `pipelay_analysis.SLayConfig.calculate_overbend_stress` | code → pack: absent from section 2 |
| `pipelay_analysis.SLayConfig.calculate_sagbend_stress` | code → pack: absent from section 2 |
| `pipelay_analysis.SLayConfig.calculate_departure_angle_table` | code → pack: absent from section 2 |
| `pipelay_analysis.JLayConfig` | code → pack: absent from section 2 |
| `pipelay_analysis.JLayConfig.calculate_sagbend` | code → pack: absent from section 2 |
| `pipelay_analysis.LayOperability` | code → pack: absent from section 2 |
| `pipelay_analysis.LayOperability.lay_rate_at_hs` | code → pack: absent from section 2 |
| `pipelay_analysis.LayOperability.generate_operability_table` | code → pack: absent from section 2 |
| `pipelay_analysis.calculate_stinger_radius` | code → pack: absent from section 2 |
| `postprocessor.TimeSeriesStats` | code → pack: absent from section 2 |
| `postprocessor.compute_time_series_stats` | code → pack: absent from section 2 |
| `postprocessor.RangeGraphData` | code → pack: absent from section 2 |
| `postprocessor.RangeGraphData.max_of_max` | code → pack: absent from section 2 |
| `postprocessor.RangeGraphData.min_of_min` | code → pack: absent from section 2 |
| `postprocessor.generate_range_graph` | code → pack: absent from section 2 |
| `postprocessor.ExtremeValueResult` | code → pack: absent from section 2 |
| `postprocessor.fit_gumbel` | code → pack: absent from section 2 |
| `postprocessor.fit_weibull` | code → pack: absent from section 2 |
| `postprocessor.MultiSeedSummary` | code → pack: absent from section 2 |
| `postprocessor.process_multi_seed` | code → pack: absent from section 2 |
| `qa.run_orcaflex_qa` | code → pack: absent from section 2 |
| `riser_config.RiserType` | code → pack: absent from section 2 |
| `riser_config.PipeGrade` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.inner_diameter` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.nominal_wt` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.steel_area` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.internal_area` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.total_od` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.displaced_volume_per_m` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.steel_mass_per_m` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.coating_mass_per_m` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.contents_mass_per_m` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.total_mass_per_m` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.submerged_weight_per_m` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.bending_stiffness` | code → pack: absent from section 2 |
| `riser_config.RiserPipeProperties.axial_stiffness` | code → pack: absent from section 2 |
| `riser_config.SCRDesignInput` | code → pack: absent from section 2 |
| `riser_config.SCRDesignInput.calculate_geometry` | code → pack: absent from section 2 |
| `riser_config.LazyWaveDesignInput` | code → pack: absent from section 2 |
| `riser_config.LazyWaveDesignInput.buoyancy_volume_per_m` | code → pack: absent from section 2 |
| `riser_config.LazyWaveDesignInput.buoyancy_mass_per_m` | code → pack: absent from section 2 |
| `riser_config.LazyWaveDesignInput.net_uplift_per_m` | code → pack: absent from section 2 |
| `riser_config.LazyWaveDesignInput.size_buoyancy_section` | code → pack: absent from section 2 |
| `riser_config.TTRDesignInput` | code → pack: absent from section 2 |
| `riser_config.TTRDesignInput.calculate_stroke` | code → pack: absent from section 2 |
| `riser_config.TTRDesignInput.calculate_top_tension` | code → pack: absent from section 2 |
| `riser_config.calculate_weight_in_water` | code → pack: absent from section 2 |
| `riser_config.estimate_scr_hang_off_angle` | code → pack: absent from section 2 |
| `riser_input_schema.UnifiedRiserType` | code → pack: absent from section 2 |
| `riser_input_schema.GeometryBlock` | code → pack: absent from section 2 |
| `riser_input_schema.MaterialBlock` | code → pack: absent from section 2 |
| `riser_input_schema.MaterialBlock.to_pipe_properties` | code → pack: absent from section 2 |
| `riser_input_schema.EnvironmentBlock` | code → pack: absent from section 2 |
| `riser_input_schema.AnalysisBlock` | code → pack: absent from section 2 |
| `riser_input_schema.SCRInput` | code → pack: absent from section 2 |
| `riser_input_schema.SCRInput.to_legacy` | code → pack: absent from section 2 |
| `riser_input_schema.SLWRInput` | code → pack: absent from section 2 |
| `riser_input_schema.FlexibleInput` | code → pack: absent from section 2 |
| `riser_input_schema.TTRInput` | code → pack: absent from section 2 |
| `riser_input_schema.DrillingInput` | code → pack: absent from section 2 |
| `synthetic_rope_design.CompressionFatigueSeverity` | code → pack: absent from section 2 |
| `synthetic_rope_design.CompressionFatigueVerdict` | code → pack: absent from section 2 |
| `synthetic_rope_design.ManufacturerSpec` | code → pack: absent from section 2 |
| `synthetic_rope_design.QAItem` | code → pack: absent from section 2 |
| `synthetic_rope_design.RopeSelectionResult` | code → pack: absent from section 2 |
| `synthetic_rope_design.select_rope_material` | code → pack: absent from section 2 |
| `synthetic_rope_design.select_rope_material_result` | code → pack: absent from section 2 |
| `synthetic_rope_design.axial_stiffness_for_load_history` | code → pack: absent from section 2 |
| `synthetic_rope_design.creep_elongation` | code → pack: absent from section 2 |
| `synthetic_rope_design.axial_compression_fatigue_check` | code → pack: absent from section 2 |
| `synthetic_rope_design.qa_program` | code → pack: absent from section 2 |
| `viv_screening.VIVDirection` | code → pack: absent from section 2 |
| `viv_screening.BoundaryCondition` | code → pack: absent from section 2 |
| `viv_screening.strouhal_number` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningInput` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningInput.reynolds_number` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningInput.st` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningInput.vortex_shedding_frequency` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningInput.stability_parameter` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningInput.check_reduced_velocity` | code → pack: absent from section 2 |
| `viv_screening.BeamProperties` | code → pack: absent from section 2 |
| `viv_screening.BeamProperties.moment_of_inertia` | code → pack: absent from section 2 |
| `viv_screening.BeamProperties.bending_stiffness` | code → pack: absent from section 2 |
| `viv_screening.BeamProperties.boundary_constants` | code → pack: absent from section 2 |
| `viv_screening.BeamProperties.natural_frequency` | code → pack: absent from section 2 |
| `viv_screening.BeamProperties.natural_frequencies` | code → pack: absent from section 2 |
| `viv_screening.VIVScreeningResult` | code → pack: absent from section 2 |
| `viv_screening.viv_screening` | code → pack: absent from section 2 |
| `viv_screening.estimate_response_amplitude` | code → pack: absent from section 2 |
| `viv_screening_workflow.VIVParametricScreeningWorkflow` | code → pack: absent from section 2 |
| `viv_screening_workflow.VIVParametricScreeningWorkflow.router` | code → pack: absent from section 2 |
| `viv_screening_workflow.write_viv_outputs` | code → pack: absent from section 2 |
| `weather_window.ScatterDiagram` | code → pack: absent from section 2 |
| `weather_window.ScatterDiagram.generate_default_scatter` | code → pack: absent from section 2 |
| `weather_window.ScatterDiagram.get_occurrences` | code → pack: absent from section 2 |
| `weather_window.ScatterDiagram.total_probability_below` | code → pack: absent from section 2 |
| `weather_window.OperabilityInput` | code → pack: absent from section 2 |
| `weather_window.OperabilityInput.calculate_operability` | code → pack: absent from section 2 |
| `weather_window.OperabilityTable` | code → pack: absent from section 2 |
| `weather_window.OperabilityTable.generate_table` | code → pack: absent from section 2 |
| `weather_window.PersistenceResult` | code → pack: absent from section 2 |
| `weather_window.analyse_persistence` | code → pack: absent from section 2 |
| `weather_window.SeasonalOperability` | code → pack: absent from section 2 |
| `weather_window.SeasonalOperability.best_months` | code → pack: absent from section 2 |
| `weather_window.SeasonalOperability.annual_average` | code → pack: absent from section 2 |
| `weather_window.WoWEstimate` | code → pack: absent from section 2 |
| `weather_window.WoWEstimate.estimate_wow_hours` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 43. `src/digitalmodel/orcawave/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 89.

| Function | Direction |
| --- | --- |
| `drift_forces.MeanDriftCoefficients` | code → pack: absent from section 2 |
| `drift_forces.QTFMatrix` | code → pack: absent from section 2 |
| `drift_forces.DriftForceResult` | code → pack: absent from section 2 |
| `drift_forces.WindCurrentDrift` | code → pack: absent from section 2 |
| `drift_forces.compute_mean_drift_force` | code → pack: absent from section 2 |
| `drift_forces.newman_approximation` | code → pack: absent from section 2 |
| `drift_forces.full_qtf_slowly_varying` | code → pack: absent from section 2 |
| `drift_forces.compute_wind_current_drift` | code → pack: absent from section 2 |
| `hydro_coefficients.HydroMatrix6x6.finite_matrix` | code → pack: absent from section 2 |
| `hydro_coefficients.HydroMatrix6x6.to_numpy` | code → pack: absent from section 2 |
| `hydro_coefficients.interpolate_matrix_at_frequency` | code → pack: absent from section 2 |
| `hydro_coefficients.interpolate_excitation_at_frequency` | code → pack: absent from section 2 |
| `hydro_coefficients.to_native_added_mass` | code → pack: absent from section 2 |
| `hydro_coefficients.to_native_damping` | code → pack: absent from section 2 |
| `hydro_coefficients.from_native_added_mass` | code → pack: absent from section 2 |
| `hydro_coefficients.to_wamit_added_mass` | code → pack: absent from section 2 |
| `hydro_coefficients.to_wamit_damping` | code → pack: absent from section 2 |
| `hydro_coefficients.from_wamit_added_mass` | code → pack: absent from section 2 |
| `hydro_coefficients.create_hydrostatic_restoring` | code → pack: absent from section 2 |
| `motion_statistics.ResponseSpectrum` | code → pack: absent from section 2 |
| `motion_statistics.ShortTermStatistics` | code → pack: absent from section 2 |
| `motion_statistics.ShortTermStatistics.significant` | code → pack: absent from section 2 |
| `motion_statistics.LongTermResult` | code → pack: absent from section 2 |
| `motion_statistics.MSIResult` | code → pack: absent from section 2 |
| `motion_statistics.compute_response_spectrum` | code → pack: absent from section 2 |
| `motion_statistics.short_term_statistics` | code → pack: absent from section 2 |
| `motion_statistics.rayleigh_exceedance` | code → pack: absent from section 2 |
| `motion_statistics.rayleigh_quantile` | code → pack: absent from section 2 |
| `motion_statistics.long_term_extreme` | code → pack: absent from section 2 |
| `motion_statistics.wf_weighting` | code → pack: absent from section 2 |
| `panel_mesh.MeshQualityMetrics` | code → pack: absent from section 2 |
| `panel_mesh.MeshSummary` | code → pack: absent from section 2 |
| `panel_mesh.write_gdf` | code → pack: absent from section 2 |
| `panel_mesh.compute_panel_quality` | code → pack: absent from section 2 |
| `panel_mesh.compute_mesh_summary` | code → pack: absent from section 2 |
| `panel_mesh.compute_waterplane_area` | code → pack: absent from section 2 |
| `panel_mesh.compute_displaced_volume` | code → pack: absent from section 2 |
| `qa_gates.native_gate_record` | code → pack: absent from section 2 |
| `qa_gates.require_native_gate` | code → pack: absent from section 2 |
| `qa_gates.implementation_map` | code → pack: absent from section 2 |
| `qa_gates.check_balance` | code → pack: absent from section 2 |
| `qa_gates.compute_buoyancy_centroid` | code → pack: absent from section 2 |
| `qa_gates.check_mesh_resolution` | code → pack: absent from section 2 |
| `qa_gates.check_waterline` | code → pack: absent from section 2 |
| `qa_gates.check_normals` | code → pack: absent from section 2 |
| `qa_gates.check_hydrodynamics` | code → pack: absent from section 2 |
| `qa_gates.build_trace` | code → pack: absent from section 2 |
| `rao_processing.RAOComparisonResult` | code → pack: absent from section 2 |
| `rao_processing.amplitude_phase_to_complex` | code → pack: absent from section 2 |
| `rao_processing.complex_to_amplitude_phase` | code → pack: absent from section 2 |
| `rao_processing.interpolate_rao` | code → pack: absent from section 2 |
| `rao_processing.combine_raos_multi_body` | code → pack: absent from section 2 |
| `rao_processing.compare_raos` | code → pack: absent from section 2 |
| `rao_processing.rao_table_to_csv` | code → pack: absent from section 2 |
| `vessel_database.VesselParameters` | code → pack: absent from section 2 |
| `vessel_database.VesselParameters.displacement_consistency` | code → pack: absent from section 2 |
| `vessel_database.VesselRAOSet` | code → pack: absent from section 2 |
| `vessel_database.ParametricHull` | code → pack: absent from section 2 |
| `vessel_database.list_vessels` | code → pack: absent from section 2 |
| `vessel_database.get_vessel` | code → pack: absent from section 2 |
| `vessel_database.get_vessels_by_type` | code → pack: absent from section 2 |
| `vessel_database.get_representative_raos` | code → pack: absent from section 2 |
| `vessel_database.generate_parametric_hull` | code → pack: absent from section 2 |
| `wave_spectrum.SpectrumParameters.explicit_components` | code → pack: absent from section 2 |
| `wave_spectrum.SpectrumResult` | code → pack: absent from section 2 |
| `wave_spectrum.SpectralMoments` | code → pack: absent from section 2 |
| `wave_spectrum.compute_spectral_moments` | code → pack: absent from section 2 |
| `wave_spectrum.spectral_periods` | code → pack: absent from section 2 |
| `wave_spectrum.pierson_moskowitz` | code → pack: absent from section 2 |
| `wave_spectrum.jonswap` | code → pack: absent from section 2 |
| `wave_spectrum.bretschneider` | code → pack: absent from section 2 |
| `wave_spectrum.issc_spectrum` | code → pack: absent from section 2 |
| `wave_spectrum.ochi_hubble` | code → pack: absent from section 2 |
| `wave_spectrum.heuristic_two_peak` | code → pack: absent from section 2 |
| `wave_spectrum.torsethaugen` | code → pack: absent from section 2 |
| `wave_spectrum.validate_spectral_grid` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 44. `src/digitalmodel/parametric/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 40.

| Function | Direction |
| --- | --- |
| `atlas.Axis.is_categorical` | code → pack: absent from section 2 |
| `atlas.Axis.derived_value` | code → pack: absent from section 2 |
| `atlas.Axis.to_dict` | code → pack: absent from section 2 |
| `atlas.Axis.from_dict` | code → pack: absent from section 2 |
| `atlas.Atlas.continuous_axes` | code → pack: absent from section 2 |
| `atlas.Atlas.categorical_axis` | code → pack: absent from section 2 |
| `atlas.Atlas.max_rel_error` | code → pack: absent from section 2 |
| `atlas.Atlas.predict` | code → pack: absent from section 2 |
| `atlas.Atlas.local_error` | code → pack: absent from section 2 |
| `atlas.Atlas.surrogate_spec` | code → pack: absent from section 2 |
| `atlas.Atlas.save` | code → pack: absent from section 2 |
| `atlas.Atlas.load` | code → pack: absent from section 2 |
| `build.build_atlas_from_registry` | code → pack: absent from section 2 |
| `catalog.build_catalog` | code → pack: absent from section 2 |
| `catalog.render_markdown` | code → pack: absent from section 2 |
| `catalog.main` | code → pack: absent from section 2 |
| `generate.generate_atlas` | code → pack: absent from section 2 |
| `generate.generate_atlas_adaptive` | code → pack: absent from section 2 |
| `historical_provenance.EvidenceError` | code → pack: absent from section 2 |
| `historical_provenance.FingerprintMismatch` | code → pack: absent from section 2 |
| `historical_provenance.sha256` | code → pack: absent from section 2 |
| `historical_provenance.archive_reader` | code → pack: absent from section 2 |
| `historical_provenance.git_reader` | code → pack: absent from section 2 |
| `historical_provenance.legacy_fingerprint` | code → pack: absent from section 2 |
| `historical_provenance.source_record` | code → pack: absent from section 2 |
| `historical_provenance.verify_sources` | code → pack: absent from section 2 |
| `historical_provenance.verify_artifacts` | code → pack: absent from section 2 |
| `historical_provenance.verify_git` | code → pack: absent from section 2 |
| `historical_provenance.main` | code → pack: absent from section 2 |
| `library.build_library_atlas` | code → pack: absent from section 2 |
| `query.router` | code → pack: absent from section 2 |
| `refresh.parametric_workflow_ids` | code → pack: absent from section 2 |
| `refresh.content_fingerprint` | code → pack: absent from section 2 |
| `refresh.refresh_status` | code → pack: absent from section 2 |
| `refresh.library_drift` | code → pack: absent from section 2 |
| `refresh.library_status` | code → pack: absent from section 2 |
| `refresh.main` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 45. `src/digitalmodel/pipelay/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 7.

| Function | Direction |
| --- | --- |
| `validation.check_keys` | code → pack: absent from section 2 |
| `validation.require` | code → pack: absent from section 2 |
| `validation.finite_number` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 46. `src/digitalmodel/power/analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 7.

| Function | Direction |
| --- | --- |
| `load_flow.BusType` | code → pack: absent from section 2 |
| `load_flow.Network` | code → pack: absent from section 2 |
| `load_flow.PowerFlowResult` | code → pack: absent from section 2 |
| `load_flow.solve_power_flow` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 47. `src/digitalmodel/power/datacenter/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 16.

| Function | Direction |
| --- | --- |
| `models.RedundancyScheme` | code → pack: absent from section 2 |
| `models.PowerComponent` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath.add_component` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath.cascaded_efficiency` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath.bottleneck_kva` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath.effective_capacity_kva` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath.has_ups` | code → pack: absent from section 2 |
| `topology.DataCenterPowerPath.has_sts` | code → pack: absent from section 2 |
| `validator.TopologyValidator` | code → pack: absent from section 2 |
| `validator.TopologyValidator.path_count` | code → pack: absent from section 2 |
| `validator.TopologyValidator.find_spofs` | code → pack: absent from section 2 |
| `validator.TopologyValidator.classify_tier` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 48. `src/digitalmodel/power/microgrid/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 27.

| Function | Direction |
| --- | --- |
| `island_detector.IslandDetector.compute_rocof` | code → pack: absent from section 2 |
| `island_detector.IslandDetector.compute_vector_shift` | code → pack: absent from section 2 |
| `island_detector.IslandDetector.detect` | code → pack: absent from section 2 |
| `microgrid_ems.MicrogridEMS` | code → pack: absent from section 2 |
| `microgrid_ems.MicrogridEMS.merit_order_dispatch` | code → pack: absent from section 2 |
| `microgrid_ems.MicrogridEMS.update` | code → pack: absent from section 2 |
| `microgrid_ems.MicrogridEMS.black_start` | code → pack: absent from section 2 |
| `microgrid_ems.MicrogridEMS.reconnect` | code → pack: absent from section 2 |
| `models.MicrogridMode` | code → pack: absent from section 2 |
| `models.DERType` | code → pack: absent from section 2 |
| `models.MicrogridState.net_load_kw` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 49. `src/digitalmodel/power/protocols/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 11.

| Function | Direction |
| --- | --- |
| `iec61850_goose.LogicalNodeType` | code → pack: absent from section 2 |
| `iec61850_goose.DAType` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L6` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L345-L26502` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L42693-L42706` | pack → code: file missing or range invalid |

### 50. `src/digitalmodel/rao_spectral_fatigue/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 1.

| Function | Direction |
| --- | --- |
| None detected by lexical comparison | No code → pack finding |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 51. `src/digitalmodel/reservoir/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 9.

| Function | Direction |
| --- | --- |
| `stratigraphic.plot_gr_track` | code → pack: absent from section 2 |
| `stratigraphic.plot_rt_track` | code → pack: absent from section 2 |
| `stratigraphic.plot_rhob_nphi_track` | code → pack: absent from section 2 |
| `stratigraphic.plot_facies_track` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@3256bab9:L1-L4` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab9:L346-L26502` | pack → code: file missing or range invalid |

### 52. `src/digitalmodel/riser_database/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 28.

| Function | Direction |
| --- | --- |
| `getters.AmjigEnvelopeCriteria` | code → pack: absent from section 2 |
| `getters.get_riser_dff` | code → pack: absent from section 2 |
| `getters.get_riser_scf` | code → pack: absent from section 2 |
| `getters.get_tension_weight_factor` | code → pack: absent from section 2 |
| `getters.get_buoyancy_tension_factor` | code → pack: absent from section 2 |
| `getters.get_von_mises_design_factor` | code → pack: absent from section 2 |
| `getters.get_flexjoint_angle_limit` | code → pack: absent from section 2 |
| `getters.get_amjig_envelope_criteria` | code → pack: absent from section 2 |
| `loader.RiserConfigRow` | code → pack: absent from section 2 |
| `loader.StandardsCrosswalkRow` | code → pack: absent from section 2 |
| `loader.MaterialSnScfDffRow` | code → pack: absent from section 2 |
| `loader.RiserStackupRow` | code → pack: absent from section 2 |
| `loader.RigRiserInterfaceRow` | code → pack: absent from section 2 |
| `loader.RiserDatabase.configs` | code → pack: absent from section 2 |
| `loader.RiserDatabase.crosswalk_rows` | code → pack: absent from section 2 |
| `loader.RiserDatabase.materials` | code → pack: absent from section 2 |
| `loader.RiserDatabase.stackups` | code → pack: absent from section 2 |
| `loader.RiserDatabase.rigs` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 53. `src/digitalmodel/riser_fatigue/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 8.

| Function | Direction |
| --- | --- |
| `touchdown.TouchdownFatigueInput` | code → pack: absent from section 2 |
| `touchdown.assess_touchdown_fatigue` | code → pack: absent from section 2 |
| `workflow.resolve_dff` | code → pack: absent from section 2 |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 54. `src/digitalmodel/signal_processing/signal_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 19.

| Function | Direction |
| --- | --- |
| `adapters.TimeSeriesComponentsAdapter` | code → pack: absent from section 2 |
| `adapters.TimeSeriesComponentsAdapter.get_rainflow_count_from_time_series` | code → pack: absent from section 2 |
| `adapters.TimeSeriesComponentsAdapter.window_average_fft` | code → pack: absent from section 2 |
| `adapters.TimeSeriesComponentsAdapter.fft_window_analysis` | code → pack: absent from section 2 |
| `adapters.TimeSeriesComponentsAdapter.get_filtered_fft` | code → pack: absent from section 2 |
| `adapters.TimeSeriesComponentsAdapter.fft_analysis` | code → pack: absent from section 2 |
| `adapters.TimeSeriesComponentsAdapter.get_filtered_signal` | code → pack: absent from section 2 |
| `adapters.FatigueAnalysisAdapter` | code → pack: absent from section 2 |
| `adapters.FatigueAnalysisAdapter.get_rainflow_from_timetrace` | code → pack: absent from section 2 |
| `adapters.FatigueAnalysisAdapter.damage_from_rainflow_cycles` | code → pack: absent from section 2 |
| `adapters.OrcaFlexAdapter` | code → pack: absent from section 2 |
| `adapters.OrcaFlexAdapter.RainflowHalfCycles` | code → pack: absent from section 2 |
| `adapters.create_migration_guide` | code → pack: absent from section 2 |
| `cli.cli` | code → pack: absent from section 2 |
| `cli.rainflow_cmd` | code → pack: absent from section 2 |
| `cli.fft_cmd` | code → pack: absent from section 2 |
| `cli.psd_cmd` | code → pack: absent from section 2 |
| `cli.filter_cmd` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 55. `src/digitalmodel/solvers/blender_automation/docs/agent-pack.md`

Strict checker exit: 0; package definitions: 0.

| Function | Direction |
| --- | --- |
| None detected by lexical comparison | No code → pack finding |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 56. `src/digitalmodel/solvers/calculix/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 23.

| Function | Direction |
| --- | --- |
| `fem_chain.is_calculix_available` | code → pack: absent from section 2 |
| `fem_chain.FEMChain` | code → pack: absent from section 2 |
| `fem_chain.FEMChain.create_plate_with_hole` | code → pack: absent from section 2 |
| `fem_chain.FEMChain.setup_analysis` | code → pack: absent from section 2 |
| `fem_chain.FEMChain.solve` | code → pack: absent from section 2 |
| `fem_chain.FEMChain.extract_results` | code → pack: absent from section 2 |
| `fem_chain.FEMChain.run_plate_validation` | code → pack: absent from section 2 |
| `inp_writer.INPWriter` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_material` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_node_set` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_element_set` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_boundary_condition` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_load` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_beam_section` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.add_step` | code → pack: absent from section 2 |
| `inp_writer.INPWriter.write` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser.parse_frd_file` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser.parse_dat_file` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser.get_max_von_mises` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser.get_max_displacement` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser.get_max_stress_component` | code → pack: absent from section 2 |
| `result_parser.CalculiXResultParser.get_stress_at_node` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 57. `src/digitalmodel/solvers/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 7.

| Function | Direction |
| --- | --- |
| `parametric_spec_generator.FrequencySweep.generate_ranges` | code → pack: absent from section 2 |
| `parametric_spec_generator.HeadingSweep` | code → pack: absent from section 2 |
| `parametric_spec_generator.HullParameterSweep` | code → pack: absent from section 2 |
| `parametric_spec_generator.ParametricSpecGenerator` | code → pack: absent from section 2 |
| `parametric_spec_generator.ParametricSpecGenerator.generate` | code → pack: absent from section 2 |
| `parametric_spec_generator.ParametricSpecGenerator.generate_to_yaml` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 58. `src/digitalmodel/solvers/ghs/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 95.

| Function | Direction |
| --- | --- |
| `_canonical.fail` | code → pack: absent from section 2 |
| `_canonical.shape` | code → pack: absent from section 2 |
| `_canonical.text` | code → pack: absent from section 2 |
| `_canonical.sha` | code → pack: absent from section 2 |
| `_canonical.integer` | code → pack: absent from section 2 |
| `_canonical.digest` | code → pack: absent from section 2 |
| `_canonical.bytes_hash` | code → pack: absent from section 2 |
| `_owned_process.native_image` | code → pack: absent from section 2 |
| `_owned_process.trusted_interpreter` | code → pack: absent from section 2 |
| `_owned_process.source_identity` | code → pack: absent from section 2 |
| `_owned_process.launch` | code → pack: absent from section 2 |
| `_owned_process.cleanup_failed_launch` | code → pack: absent from section 2 |
| `_owned_process.Owned` | code → pack: absent from section 2 |
| `_owned_process.Owned.resume` | code → pack: absent from section 2 |
| `_owned_process.Owned.close_job` | code → pack: absent from section 2 |
| `_owned_process.Owned.cleanup` | code → pack: absent from section 2 |
| `_owned_process.retain` | code → pack: absent from section 2 |
| `_owned_process.duplicate_job` | code → pack: absent from section 2 |
| `_qualification_scenarios.ObservationFailure` | code → pack: absent from section 2 |
| `_qualification_scenarios.ready` | code → pack: absent from section 2 |
| `_qualification_scenarios.check_timing` | code → pack: absent from section 2 |
| `_qualification_scenarios.alive` | code → pack: absent from section 2 |
| `_qualification_scenarios.terminated` | code → pack: absent from section 2 |
| `_qualification_scenarios.Resources` | code → pack: absent from section 2 |
| `_qualification_scenarios.Resources.start` | code → pack: absent from section 2 |
| `_qualification_scenarios.Resources.retain` | code → pack: absent from section 2 |
| `_qualification_scenarios.Resources.close_all` | code → pack: absent from section 2 |
| `_qualification_scenarios.parent_members` | code → pack: absent from section 2 |
| `_qualification_scenarios.ordinary` | code → pack: absent from section 2 |
| `_qualification_scenarios.observe_timeout` | code → pack: absent from section 2 |
| `_qualification_scenarios.second_handle` | code → pack: absent from section 2 |
| `_qualification_scenarios.crash` | code → pack: absent from section 2 |
| `_qualification_scenarios.run_scenario` | code → pack: absent from section 2 |
| `_qualification_state.no_reparse` | code → pack: absent from section 2 |
| `_qualification_state.publish` | code → pack: absent from section 2 |
| `_qualification_state.read_record` | code → pack: absent from section 2 |
| `_qualification_state.state_root` | code → pack: absent from section 2 |
| `_qualification_state.Attempt` | code → pack: absent from section 2 |
| `_qualification_state.Attempt.reserve` | code → pack: absent from section 2 |
| `_qualification_state.Attempt.stage` | code → pack: absent from section 2 |
| `_qualification_state.Attempt.finish` | code → pack: absent from section 2 |
| `_sentinel.watchdog` | code → pack: absent from section 2 |
| `_sentinel.await_file` | code → pack: absent from section 2 |
| `_sentinel.own_identity` | code → pack: absent from section 2 |
| `_sentinel.fixed_child` | code → pack: absent from section 2 |
| `_sentinel.parent` | code → pack: absent from section 2 |
| `_sentinel.controller` | code → pack: absent from section 2 |
| `_sentinel.main` | code → pack: absent from section 2 |
| `_windows_job.FILETIME` | code → pack: absent from section 2 |
| `_windows_job.STARTUPINFO` | code → pack: absent from section 2 |
| `_windows_job.STARTUPINFOEX` | code → pack: absent from section 2 |
| `_windows_job.PROCESS_INFORMATION` | code → pack: absent from section 2 |
| `_windows_job.BASIC_LIMITS` | code → pack: absent from section 2 |
| `_windows_job.IO_COUNTERS` | code → pack: absent from section 2 |
| `_windows_job.JOBOBJECT_EXTENDED_LIMIT_INFORMATION` | code → pack: absent from section 2 |
| `_windows_job.kernel_api` | code → pack: absent from section 2 |
| `_windows_job.checked` | code → pack: absent from section 2 |
| `_windows_job.close` | code → pack: absent from section 2 |
| `_windows_job.new_job` | code → pack: absent from section 2 |
| `_windows_job.Attributes` | code → pack: absent from section 2 |
| `_windows_job.member` | code → pack: absent from section 2 |
| `_windows_job.wait` | code → pack: absent from section 2 |
| `_windows_job.census` | code → pack: absent from section 2 |
| `comparison.box_oracle` | code → pack: absent from section 2 |
| `comparison.oracle_material` | code → pack: absent from section 2 |
| `contracts.path_text` | code → pack: absent from section 2 |
| `contracts.output_record` | code → pack: absent from section 2 |
| `contracts.build_argv` | code → pack: absent from section 2 |
| `contracts.preview_runfile` | code → pack: absent from section 2 |
| `contracts.prepare_canary` | code → pack: absent from section 2 |
| `contracts.validate_prepared` | code → pack: absent from section 2 |
| `contracts.validate_packet` | code → pack: absent from section 2 |
| `contracts.timestamp` | code → pack: absent from section 2 |
| `contracts.preview_approval` | code → pack: absent from section 2 |
| `qualification.runtime_identity` | code → pack: absent from section 2 |
| `qualification.account_name` | code → pack: absent from section 2 |
| `qualification.public_summary` | code → pack: absent from section 2 |
| `qualification.preserve_failed_records` | code → pack: absent from section 2 |
| `qualification.qualify` | code → pack: absent from section 2 |
| `runner.run_approved_capture` | code → pack: absent from section 2 |
| `runner.stage_synthetic` | code → pack: absent from section 2 |
| `runner.inspect_synthetic_events` | code → pack: absent from section 2 |
| `runner.run_synthetic_capture` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml:L1-L53690` | pack → code: file missing or range invalid |

### 59. `src/digitalmodel/solvers/gmsh_meshing/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 68.

| Function | Direction |
| --- | --- |
| `_msh2_contract.Msh2Contract` | code → pack: absent from section 2 |
| `_msh2_contract.inspect_msh2` | code → pack: absent from section 2 |
| `_msh2_contract.validate_msh2_contract` | code → pack: absent from section 2 |
| `_tank_fixture_gmsh.PerforatedMemberGeometry` | code → pack: absent from section 2 |
| `_tank_fixture_gmsh.FixtureBuildSummary` | code → pack: absent from section 2 |
| `_tank_fixture_gmsh.FixtureBuildSummary.member_count` | code → pack: absent from section 2 |
| `_tank_fixture_gmsh.FixtureBuildSummary.opening_count` | code → pack: absent from section 2 |
| `_tank_fixture_gmsh.build_tank_fixture` | code → pack: absent from section 2 |
| `cli.box_cmd` | code → pack: absent from section 2 |
| `cli.cylinder_cmd` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.initialize` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.finalize` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.generate_simple_box_mesh` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.generate_cylinder_mesh` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.generate_surface_mesh_from_stl` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.generate_mesh_from_step` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.save_mesh_vtk` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.save_mesh_msh` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.get_mesh_statistics` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.export_mesh_inp` | code → pack: absent from section 2 |
| `mesh_generator.GMSHMeshGenerator.create_step_geometry` | code → pack: absent from section 2 |
| `models.GeometryType` | code → pack: absent from section 2 |
| `models.ElementType` | code → pack: absent from section 2 |
| `models.MeshAlgorithm` | code → pack: absent from section 2 |
| `models.MeshQuality` | code → pack: absent from section 2 |
| `models.MeshQuality.is_acceptable` | code → pack: absent from section 2 |
| `models.MeshQuality.is_good` | code → pack: absent from section 2 |
| `models.MeshQuality.quality_score` | code → pack: absent from section 2 |
| `models.MeshQuality.to_dict` | code → pack: absent from section 2 |
| `models.MeshStatistics` | code → pack: absent from section 2 |
| `models.MeshStatistics.elements_per_node` | code → pack: absent from section 2 |
| `models.MeshStatistics.to_dict` | code → pack: absent from section 2 |
| `models.INPExportConfig` | code → pack: absent from section 2 |
| `quality_analyzer.MeshQualityAnalyzer` | code → pack: absent from section 2 |
| `quality_analyzer.MeshQualityAnalyzer.analyze_tetrahedral_mesh` | code → pack: absent from section 2 |
| `quality_analyzer.MeshQualityAnalyzer.compute_mesh_statistics` | code → pack: absent from section 2 |
| `spool_pilot.SpoolParameters` | code → pack: absent from section 2 |
| `spool_pilot.SpoolParameters.total_length` | code → pack: absent from section 2 |
| `spool_pilot.SpoolParameters.dimensions` | code → pack: absent from section 2 |
| `spool_pilot.HandCalculation` | code → pack: absent from section 2 |
| `spool_pilot.hand_calculation` | code → pack: absent from section 2 |
| `spool_pilot.SolidMeasurements` | code → pack: absent from section 2 |
| `spool_pilot.MeshMeasurements` | code → pack: absent from section 2 |
| `spool_pilot.MeshMeasurements.element_count` | code → pack: absent from section 2 |
| `spool_pilot.Verdict` | code → pack: absent from section 2 |
| `spool_pilot.Verdict.passed` | code → pack: absent from section 2 |
| `spool_pilot.Verdict.assert_passed` | code → pack: absent from section 2 |
| `spool_pilot.judge_solid` | code → pack: absent from section 2 |
| `spool_pilot.judge_mesh` | code → pack: absent from section 2 |
| `spool_pilot.measure_mesh` | code → pack: absent from section 2 |
| `spool_pilot.mesh_step` | code → pack: absent from section 2 |
| `spool_pilot.run_pilot` | code → pack: absent from section 2 |
| `tank_fixture.TankDimensions` | code → pack: absent from section 2 |
| `tank_fixture.MemberDimensions` | code → pack: absent from section 2 |
| `tank_fixture.LongitudinalOpening` | code → pack: absent from section 2 |
| `tank_fixture.TransverseOpening` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.from_mapping` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.frame` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.longitudinal_member_span` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.transverse_member_span` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.gross_volume` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.expected_fluid_volume` | code → pack: absent from section 2 |
| `tank_fixture.TankFixtureSpec.scaled` | code → pack: absent from section 2 |
| `tank_fixture.load_tank_fixture_spec` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 60. `src/digitalmodel/solvers/mystran/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 47.

| Function | Direction |
| --- | --- |
| `bdf_writer.BDFWriter` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_material` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_bar_section` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_solid_section` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_shell_section` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_spc` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_force` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_moment` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.set_outputs` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.add_param` | code → pack: absent from section 2 |
| `bdf_writer.BDFWriter.write` | code → pack: absent from section 2 |
| `convergence.ConvergenceLevel` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy.run` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy.converged` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy.converged_at` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy.is_monotone` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy.summary_table` | code → pack: absent from section 2 |
| `convergence.MeshConvergenceStudy.to_records` | code → pack: absent from section 2 |
| `convergence.richardson_extrapolation` | code → pack: absent from section 2 |
| `fem_chain.find_mystran` | code → pack: absent from section 2 |
| `fem_chain.is_mystran_available` | code → pack: absent from section 2 |
| `fem_chain.MystranChain` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.load_mesh` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.create_cantilever_hex_mesh` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.create_cantilever_bar_mesh` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.mesh_stats` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.node_sets` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.setup_analysis` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.solve` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.extract_results` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.cantilever_tip_deflection` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.run_cantilever_bar_validation` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.run_cantilever_hex_validation` | code → pack: absent from section 2 |
| `fem_chain.MystranChain.run_mesh_convergence` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.find_output` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.f06_path` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.parse_f06` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.parse_f06_text` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.summary` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.has_errors` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.get_displacement` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.get_max_displacement` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.get_reaction_sum` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.get_max_von_mises` | code → pack: absent from section 2 |
| `result_parser.MystranResultParser.read_op2` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 61. `src/digitalmodel/solvers/openfoam/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 510.

| Function | Direction |
| --- | --- |
| `_case_definition_validation.ValidationError` | code → pack: absent from section 2 |
| `_case_definition_validation.require_mapping` | code → pack: absent from section 2 |
| `_case_definition_validation.check_keys` | code → pack: absent from section 2 |
| `_case_definition_validation.require_string` | code → pack: absent from section 2 |
| `_case_definition_validation.require_bool` | code → pack: absent from section 2 |
| `_case_definition_validation.require_int` | code → pack: absent from section 2 |
| `_case_definition_validation.require_number` | code → pack: absent from section 2 |
| `_case_definition_validation.require_vector` | code → pack: absent from section 2 |
| `_case_definition_validation.validate_case_name` | code → pack: absent from section 2 |
| `_case_definition_validation.require_string_sequence` | code → pack: absent from section 2 |
| `artifact_index.ArtifactIndexError` | code → pack: absent from section 2 |
| `artifact_index.frame` | code → pack: absent from section 2 |
| `artifact_index.FileRecord` | code → pack: absent from section 2 |
| `artifact_index.ArtifactRecord` | code → pack: absent from section 2 |
| `artifact_index.ArtifactRecord.as_dict` | code → pack: absent from section 2 |
| `artifact_index.tree_digest` | code → pack: absent from section 2 |
| `artifact_index.artifact_id` | code → pack: absent from section 2 |
| `artifact_index.host_local_locator` | code → pack: absent from section 2 |
| `artifact_index.is_numeric_time_name` | code → pack: absent from section 2 |
| `artifact_index.snapshot_tree` | code → pack: absent from section 2 |
| `artifact_index.verify_unchanged` | code → pack: absent from section 2 |
| `artifact_index.select_roots` | code → pack: absent from section 2 |
| `artifact_index.build_index` | code → pack: absent from section 2 |
| `block_mesh.boundary_patch_types` | code → pack: absent from section 2 |
| `block_mesh.render_block_mesh_dict_body` | code → pack: absent from section 2 |
| `case_coupling.CouplingSpec` | code → pack: absent from section 2 |
| `case_coupling.CouplingSpec.plan_area` | code → pack: absent from section 2 |
| `case_coupling.CouplingSpec.tank_volume` | code → pack: absent from section 2 |
| `case_coupling.VerificationResult` | code → pack: absent from section 2 |
| `case_coupling.CaseManifest` | code → pack: absent from section 2 |
| `case_coupling.SyntheticCase` | code → pack: absent from section 2 |
| `case_coupling.transfer_volume` | code → pack: absent from section 2 |
| `case_coupling.peak_flow_rate` | code → pack: absent from section 2 |
| `case_coupling.mean_flow_rate` | code → pack: absent from section 2 |
| `case_coupling.map_sweep_case_to_openfoam_case` | code → pack: absent from section 2 |
| `case_coupling.verify_coupling` | code → pack: absent from section 2 |
| `case_coupling.build_case_manifest` | code → pack: absent from section 2 |
| `case_coupling.synthetic_coupling_spec` | code → pack: absent from section 2 |
| `case_coupling.synthetic_sweep_case` | code → pack: absent from section 2 |
| `case_coupling.emit_synthetic_gravity_exchange_case` | code → pack: absent from section 2 |
| `case_definition.CaseDefinitionError` | code → pack: absent from section 2 |
| `case_definition.FunctionObjectsConfig` | code → pack: absent from section 2 |
| `case_definition.SelectedExecutionPlan` | code → pack: absent from section 2 |
| `case_definition.ParsedAuthoredCaseV1` | code → pack: absent from section 2 |
| `case_definition.parse_case_request` | code → pack: absent from section 2 |
| `cli.cli` | code → pack: absent from section 2 |
| `cli.setup_cmd` | code → pack: absent from section 2 |
| `cli.parametric_cmd` | code → pack: absent from section 2 |
| `cli.run_cmd` | code → pack: absent from section 2 |
| `cli.doctor_cmd` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `convergence_audit.audit_one` | code → pack: absent from section 2 |
| `convergence_audit.md_table` | code → pack: absent from section 2 |
| `convergence_audit.main` | code → pack: absent from section 2 |
| `doctor.DoctorCheck` | code → pack: absent from section 2 |
| `doctor.run_doctor` | code → pack: absent from section 2 |
| `doctor.has_failure` | code → pack: absent from section 2 |
| `domain_builder.DomainBuilder` | code → pack: absent from section 2 |
| `domain_builder.DomainBuilder.from_hull_dims` | code → pack: absent from section 2 |
| `domain_builder.DomainBuilder.from_bounding_box` | code → pack: absent from section 2 |
| `force_cycle_average.load_force` | code → pack: absent from section 2 |
| `force_cycle_average.extrema` | code → pack: absent from section 2 |
| `force_cycle_average.transient_end` | code → pack: absent from section 2 |
| `force_cycle_average.central_value` | code → pack: absent from section 2 |
| `force_cycle_average.envelope_trend` | code → pack: absent from section 2 |
| `force_cycle_average.aitken` | code → pack: absent from section 2 |
| `force_cycle_average.damped_fit` | code → pack: absent from section 2 |
| `force_cycle_average.analyse` | code → pack: absent from section 2 |
| `force_cycle_average.main` | code → pack: absent from section 2 |
| `gmsh_bridge.GmshBridgeError` | code → pack: absent from section 2 |
| `gmsh_bridge.BridgeToolchain` | code → pack: absent from section 2 |
| `gmsh_bridge.BridgeToolchain.to_dict` | code → pack: absent from section 2 |
| `gmsh_bridge.FileDigest` | code → pack: absent from section 2 |
| `gmsh_bridge.TreeDigest` | code → pack: absent from section 2 |
| `gmsh_bridge.CommandEvidence` | code → pack: absent from section 2 |
| `gmsh_bridge.CommandEvidence.to_dict` | code → pack: absent from section 2 |
| `gmsh_bridge.BridgeResult` | code → pack: absent from section 2 |
| `gmsh_bridge.hash_tree` | code → pack: absent from section 2 |
| `gmsh_bridge.hash_case_inputs` | code → pack: absent from section 2 |
| `gmsh_bridge.prepare_gmsh_poly_mesh` | code → pack: absent from section 2 |
| `gravity_conduit.TankState` | code → pack: absent from section 2 |
| `gravity_conduit.TankState.capacity` | code → pack: absent from section 2 |
| `gravity_conduit.TankState.fill_fraction` | code → pack: absent from section 2 |
| `gravity_conduit.TankState.liquid_depth` | code → pack: absent from section 2 |
| `gravity_conduit.TankState.surface_elevation` | code → pack: absent from section 2 |
| `gravity_conduit.TankState.free_volume` | code → pack: absent from section 2 |
| `gravity_conduit.TankState.with_volume` | code → pack: absent from section 2 |
| `gravity_conduit.ConduitGeometry` | code → pack: absent from section 2 |
| `gravity_conduit.ConduitGeometry.total_loss_coefficient` | code → pack: absent from section 2 |
| `gravity_conduit.ConduitGeometry.effective_discharge_coefficient` | code → pack: absent from section 2 |
| `gravity_conduit.ConduitGeometry.has_inertia` | code → pack: absent from section 2 |
| `gravity_conduit.signed_hydrostatic_head` | code → pack: absent from section 2 |
| `gravity_conduit.conduit_flow_rate` | code → pack: absent from section 2 |
| `gravity_conduit.check_transfer_feasibility` | code → pack: absent from section 2 |
| `gravity_conduit.GravityExchangeResult` | code → pack: absent from section 2 |
| `gravity_conduit.simulate_gravity_exchange` | code → pack: absent from section 2 |
| `gravity_conduit.utube_natural_frequency` | code → pack: absent from section 2 |
| `gravity_conduit.simulate_inertial_exchange` | code → pack: absent from section 2 |
| `grid_convergence.GridConvergenceError` | code → pack: absent from section 2 |
| `grid_convergence.GridConvergence` | code → pack: absent from section 2 |
| `grid_convergence.GridConvergence.to_dict` | code → pack: absent from section 2 |
| `grid_convergence.settled_mean` | code → pack: absent from section 2 |
| `grid_convergence.gci_report` | code → pack: absent from section 2 |
| `grid_convergence.force_split` | code → pack: absent from section 2 |
| `grid_convergence.main` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.nu` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.froude` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.reynolds_number` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.surface_regions` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.stl_name` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.emesh_name` | code → pack: absent from section 2 |
| `hull_case.HullCaseConfig.replace` | code → pack: absent from section 2 |
| `hull_case.HullCaseDerivation` | code → pack: absent from section 2 |
| `hull_case.derive_hull_case` | code → pack: absent from section 2 |
| `hull_case.hull_case_templates_dir` | code → pack: absent from section 2 |
| `hull_case.build_hull_case` | code → pack: absent from section 2 |
| `hull_case.build_hull_case_from_files` | code → pack: absent from section 2 |
| `hull_case_dicts.render_case_tree` | code → pack: absent from section 2 |
| `hull_case_dicts.hull_case_tokens` | code → pack: absent from section 2 |
| `hull_case_dicts.case_provenance` | code → pack: absent from section 2 |
| `hull_case_dicts.surfaces_provenance` | code → pack: absent from section 2 |
| `hull_case_physics.CellBudgetError` | code → pack: absent from section 2 |
| `hull_case_physics.DecompositionError` | code → pack: absent from section 2 |
| `hull_case_physics.CellBudget` | code → pack: absent from section 2 |
| `hull_case_physics.CellBudget.to_provenance` | code → pack: absent from section 2 |
| `hull_case_physics.vof_vertical_blocks` | code → pack: absent from section 2 |
| `hull_case_physics.derive_cell_budget` | code → pack: absent from section 2 |
| `hull_case_physics.kcs_chain_config_path` | code → pack: absent from section 2 |
| `hull_case_physics.decomposition_vector` | code → pack: absent from section 2 |
| `hull_case_physics.ForceReference` | code → pack: absent from section 2 |
| `hull_case_physics.ForceReference.to_provenance` | code → pack: absent from section 2 |
| `hull_case_physics.derive_force_reference` | code → pack: absent from section 2 |
| `hull_case_regions.SurfaceRegion` | code → pack: absent from section 2 |
| `hull_case_regions.SurfaceRegion.stl_name` | code → pack: absent from section 2 |
| `hull_case_regions.SurfaceRegion.emesh_name` | code → pack: absent from section 2 |
| `hull_case_regions.hull_region` | code → pack: absent from section 2 |
| `hull_case_regions.region_tokens` | code → pack: absent from section 2 |
| `hull_case_regions.check_region_surfaces` | code → pack: absent from section 2 |
| `hull_case_regions.copy_region_surfaces` | code → pack: absent from section 2 |
| `hull_case_regions.region_provenance` | code → pack: absent from section 2 |
| `hull_case_regions.assert_regions_agree` | code → pack: absent from section 2 |
| `hull_domain.HullDomainError` | code → pack: absent from section 2 |
| `hull_domain.HullDomain` | code → pack: absent from section 2 |
| `hull_domain.HullDomain.length` | code → pack: absent from section 2 |
| `hull_domain.HullDomain.width` | code → pack: absent from section 2 |
| `hull_domain.HullDomain.height` | code → pack: absent from section 2 |
| `hull_domain.HullDomain.volume` | code → pack: absent from section 2 |
| `hull_domain.build_hull_domain` | code → pack: absent from section 2 |
| `hull_domain.refinement_boxes` | code → pack: absent from section 2 |
| `hull_domain.block_divisions` | code → pack: absent from section 2 |
| `hull_double_body.DoubleBodyCaseError` | code → pack: absent from section 2 |
| `hull_double_body.DoubleBodyCaseConfig` | code → pack: absent from section 2 |
| `hull_double_body.DoubleBodyDerivation` | code → pack: absent from section 2 |
| `hull_double_body.derive_double_body_case` | code → pack: absent from section 2 |
| `hull_double_body.double_body_templates_dir` | code → pack: absent from section 2 |
| `hull_double_body.build_double_body_case` | code → pack: absent from section 2 |
| `hull_double_body.build_double_body_case_from_files` | code → pack: absent from section 2 |
| `hull_double_body_dicts.double_body_tokens` | code → pack: absent from section 2 |
| `hull_double_body_dicts.double_body_case_provenance` | code → pack: absent from section 2 |
| `hull_double_body_domain.build_double_body_domain` | code → pack: absent from section 2 |
| `hull_double_body_domain.double_body_refinement_boxes` | code → pack: absent from section 2 |
| `hull_double_body_domain.clip_boxes_at_waterline` | code → pack: absent from section 2 |
| `hull_double_body_domain.double_body_divisions` | code → pack: absent from section 2 |
| `hull_double_body_domain.double_body_vertical_blocks` | code → pack: absent from section 2 |
| `hull_face_resolution.HullFaceResolutionError` | code → pack: absent from section 2 |
| `hull_face_resolution.PatchFaceAreas` | code → pack: absent from section 2 |
| `hull_face_resolution.PatchFaceAreas.mean_area_m2` | code → pack: absent from section 2 |
| `hull_face_resolution.assert_patch_face_resolution` | code → pack: absent from section 2 |
| `hull_face_resolution.finest_in_plane_cell_m` | code → pack: absent from section 2 |
| `hull_face_resolution.patch_face_areas` | code → pack: absent from section 2 |
| `hull_field_patches.add_appendage_patchfields` | code → pack: absent from section 2 |
| `hull_free_surface.FreeSurfaceResolutionError` | code → pack: absent from section 2 |
| `hull_free_surface.deep_water_wavelength` | code → pack: absent from section 2 |
| `hull_free_surface.FreeSurfaceResolution` | code → pack: absent from section 2 |
| `hull_free_surface.FreeSurfaceResolution.enforced` | code → pack: absent from section 2 |
| `hull_free_surface.FreeSurfaceResolution.meets_requirement` | code → pack: absent from section 2 |
| `hull_free_surface.FreeSurfaceResolution.to_provenance` | code → pack: absent from section 2 |
| `hull_free_surface.in_plane_cell_size` | code → pack: absent from section 2 |
| `hull_free_surface.free_surface_target_cell_size` | code → pack: absent from section 2 |
| `hull_free_surface.free_surface_resolution` | code → pack: absent from section 2 |
| `hull_free_surface.free_surface_divisions` | code → pack: absent from section 2 |
| `hull_free_surface.check_free_surface_resolution` | code → pack: absent from section 2 |
| `hull_manifest.HullManifestError` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.appendage_regions` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.reference_wetted_surface_m2` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.wetted_surface_upper_bound_m2` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.loa_m` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.midship_x_m` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.hull_height_m` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.half_beam_m` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.block_coefficient` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.from_dict` | code → pack: absent from section 2 |
| `hull_manifest.HullManifest.to_provenance` | code → pack: absent from section 2 |
| `hull_manifest.load_hull_manifest` | code → pack: absent from section 2 |
| `hull_placement.HullPlacementError` | code → pack: absent from section 2 |
| `hull_placement.hull_bbox` | code → pack: absent from section 2 |
| `hull_placement.hull_x_centre` | code → pack: absent from section 2 |
| `hull_placement.domain_bbox` | code → pack: absent from section 2 |
| `hull_placement.meshed_hull_bbox` | code → pack: absent from section 2 |
| `hull_placement.assert_boxes_contain_hull` | code → pack: absent from section 2 |
| `hull_placement.assert_inner_inside_outer` | code → pack: absent from section 2 |
| `hull_placement.offset_interp` | code → pack: absent from section 2 |
| `hull_placement.boxes_provenance` | code → pack: absent from section 2 |
| `hull_pressure_bins.patch_faces_geometry` | code → pack: absent from section 2 |
| `hull_pressure_bins.bin_pressure_force` | code → pack: absent from section 2 |
| `hull_pressure_bins.main` | code → pack: absent from section 2 |
| `hull_turbulence.TurbulenceMethod` | code → pack: absent from section 2 |
| `hull_turbulence.InletTurbulence` | code → pack: absent from section 2 |
| `hull_turbulence.InletTurbulence.to_provenance` | code → pack: absent from section 2 |
| `hull_turbulence.derive_inlet_turbulence` | code → pack: absent from section 2 |
| `hull_turbulence.wall_normal_first_cell_height` | code → pack: absent from section 2 |
| `initial_fields.write_velocity_field` | code → pack: absent from section 2 |
| `initial_fields.write_turbulence_fields` | code → pack: absent from section 2 |
| `initial_fields.write_alpha_water` | code → pack: absent from section 2 |
| `lane_config.load_lanes` | code → pack: absent from section 2 |
| `marine_solvers.WaveLoadingSetup` | code → pack: absent from section 2 |
| `marine_solvers.CurrentLoadingSetup` | code → pack: absent from section 2 |
| `marine_solvers.GreenWaterSetup` | code → pack: absent from section 2 |
| `marine_solvers.SloshingSetup` | code → pack: absent from section 2 |
| `marine_solvers.VIVSetup` | code → pack: absent from section 2 |
| `matched_window_relative.WindowRangeError` | code → pack: absent from section 2 |
| `matched_window_relative.analyse` | code → pack: absent from section 2 |
| `matched_window_relative.main` | code → pack: absent from section 2 |
| `mesh_store.MeshStoreError` | code → pack: absent from section 2 |
| `mesh_store.normalise_dict_text` | code → pack: absent from section 2 |
| `mesh_store.mesh_inputs` | code → pack: absent from section 2 |
| `mesh_store.input_shas` | code → pack: absent from section 2 |
| `mesh_store.mesh_identity` | code → pack: absent from section 2 |
| `mesh_store.mesh_cells` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry.identity` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry.tag` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry.polymesh` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry.provenance_path` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry.provenance` | code → pack: absent from section 2 |
| `mesh_store.StoreEntry.cells` | code → pack: absent from section 2 |
| `mesh_store.MeshStore` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.case_dir` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.entry` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.entries` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.identity` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.find` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.link` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.promote` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.dedupe` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.verify` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.status` | code → pack: absent from section 2 |
| `mesh_store.MeshStore.drop` | code → pack: absent from section 2 |
| `mesh_store.main` | code → pack: absent from section 2 |
| `models.CaseType` | code → pack: absent from section 2 |
| `models.BoundaryType` | code → pack: absent from section 2 |
| `models.TurbulenceType` | code → pack: absent from section 2 |
| `models.BoundaryCondition` | code → pack: absent from section 2 |
| `models.BoundaryCondition.to_foam_dict` | code → pack: absent from section 2 |
| `models.TurbulenceModel` | code → pack: absent from section 2 |
| `models.TurbulenceModel.to_dict` | code → pack: absent from section 2 |
| `models.SolverConfig` | code → pack: absent from section 2 |
| `models.SolverConfig.to_control_dict` | code → pack: absent from section 2 |
| `models.DomainConfig` | code → pack: absent from section 2 |
| `models.DomainConfig.from_hull_geometry` | code → pack: absent from section 2 |
| `models.DomainConfig.cell_counts` | code → pack: absent from section 2 |
| `models.DomainConfig.estimate_cell_count` | code → pack: absent from section 2 |
| `models.DomainConfig.block_mesh_vertices` | code → pack: absent from section 2 |
| `models.OpenFOAMCase` | code → pack: absent from section 2 |
| `models.OpenFOAMCase.for_case_type` | code → pack: absent from section 2 |
| `models.OpenFOAMCase.add_boundary_condition` | code → pack: absent from section 2 |
| `motion.MotionType` | code → pack: absent from section 2 |
| `motion.MotionType.is_rotational` | code → pack: absent from section 2 |
| `motion.MotionType.axis_index` | code → pack: absent from section 2 |
| `motion.PrescribedMotion` | code → pack: absent from section 2 |
| `motion.PrescribedMotion.omega` | code → pack: absent from section 2 |
| `motion.PrescribedMotion.frequency_hz` | code → pack: absent from section 2 |
| `motion.PrescribedMotion.amplitude_vector` | code → pack: absent from section 2 |
| `motion.PrescribedMotion.from_frequency_hz` | code → pack: absent from section 2 |
| `motion.PrescribedMotion.from_omega` | code → pack: absent from section 2 |
| `motion.render_dynamic_mesh_dict_body` | code → pack: absent from section 2 |
| `motion.render_multi_motion_body` | code → pack: absent from section 2 |
| `motion.render_dynamic_mesh_dict` | code → pack: absent from section 2 |
| `motion.write_dynamic_mesh_dict` | code → pack: absent from section 2 |
| `parametric.StudyParameter` | code → pack: absent from section 2 |
| `parametric.StudyParameter.n_values` | code → pack: absent from section 2 |
| `parametric.StudyParameter.from_range` | code → pack: absent from section 2 |
| `parametric.ParametricStudy` | code → pack: absent from section 2 |
| `parametric.ParametricStudy.add_parameter` | code → pack: absent from section 2 |
| `parametric.ParametricStudy.generate_cases` | code → pack: absent from section 2 |
| `parametric.ParametricStudy.generate_directories` | code → pack: absent from section 2 |
| `partial_fill.FillSnap` | code → pack: absent from section 2 |
| `partial_fill.FillSnap.adjustment` | code → pack: absent from section 2 |
| `partial_fill.snap_fill_to_cell_face` | code → pack: absent from section 2 |
| `partial_fill.partial_fill_box` | code → pack: absent from section 2 |
| `partial_fill.render_set_fields_dict_body` | code → pack: absent from section 2 |
| `poly_mesh_contract.PolyMeshContractError` | code → pack: absent from section 2 |
| `poly_mesh_contract.BoundaryContract` | code → pack: absent from section 2 |
| `poly_mesh_contract.BoundaryContract.patch_names` | code → pack: absent from section 2 |
| `poly_mesh_contract.BoundaryContract.expected_type` | code → pack: absent from section 2 |
| `poly_mesh_contract.BoundaryPatch` | code → pack: absent from section 2 |
| `poly_mesh_contract.CellZone` | code → pack: absent from section 2 |
| `poly_mesh_contract.CheckMeshSummary` | code → pack: absent from section 2 |
| `poly_mesh_contract.PolyMeshContract` | code → pack: absent from section 2 |
| `poly_mesh_contract.PolyMeshContract.boundary_face_count` | code → pack: absent from section 2 |
| `poly_mesh_contract.PolyMeshContract.patch_names` | code → pack: absent from section 2 |
| `poly_mesh_contract.PolyMeshContract.fluid_cell_labels` | code → pack: absent from section 2 |
| `poly_mesh_contract.parse_boundary` | code → pack: absent from section 2 |
| `poly_mesh_contract.parse_cell_zones` | code → pack: absent from section 2 |
| `poly_mesh_contract.parse_check_mesh_output` | code → pack: absent from section 2 |
| `poly_mesh_contract.validate_poly_mesh_contract` | code → pack: absent from section 2 |
| `post_processing.OpenFOAMPostProcessor` | code → pack: absent from section 2 |
| `post_processing.OpenFOAMPostProcessor.parse_force_file` | code → pack: absent from section 2 |
| `post_processing.OpenFOAMPostProcessor.parse_probe_file` | code → pack: absent from section 2 |
| `post_processing.OpenFOAMPostProcessor.extract_free_surface_from_probe` | code → pack: absent from section 2 |
| `prebuilt_mesh.PrebuiltMeshError` | code → pack: absent from section 2 |
| `prebuilt_mesh.PrebuiltExecution` | code → pack: absent from section 2 |
| `prebuilt_mesh.PrebuiltExecution.verify_unchanged` | code → pack: absent from section 2 |
| `prebuilt_mesh.PrebuiltExecution.release` | code → pack: absent from section 2 |
| `prebuilt_mesh.prepare_prebuilt_execution` | code → pack: absent from section 2 |
| `pressure_tap_analysis.PressureTapStatistics` | code → pack: absent from section 2 |
| `pressure_tap_analysis.PressureTapStatistics.envelope` | code → pack: absent from section 2 |
| `pressure_tap_analysis.PressureTapStatistics.to_dict` | code → pack: absent from section 2 |
| `pressure_tap_analysis.compute_tap_statistics` | code → pack: absent from section 2 |
| `pressure_tap_analysis.read_tap_statistics` | code → pack: absent from section 2 |
| `pressure_tap_models.PressureTap` | code → pack: absent from section 2 |
| `pressure_tap_models.PressureTap.kind` | code → pack: absent from section 2 |
| `pressure_taps.render_probes_entry` | code → pack: absent from section 2 |
| `pressure_taps.render_patch_probes_entry` | code → pack: absent from section 2 |
| `pressure_taps.render_surface_entry` | code → pack: absent from section 2 |
| `pressure_taps.render_pressure_tap_functions` | code → pack: absent from section 2 |
| `pressure_taps.point_tap_names` | code → pack: absent from section 2 |
| `pressure_taps.rectangular_tank_wall_taps` | code → pack: absent from section 2 |
| `region_refinement.near_hull_cell_size` | code → pack: absent from section 2 |
| `region_refinement.finest_in_plane_cell` | code → pack: absent from section 2 |
| `region_refinement.RegionRefinement` | code → pack: absent from section 2 |
| `region_refinement.RegionRefinement.clamped` | code → pack: absent from section 2 |
| `region_refinement.RegionRefinement.cells_across_after` | code → pack: absent from section 2 |
| `region_refinement.RegionRefinement.refinement_level` | code → pack: absent from section 2 |
| `region_refinement.RegionRefinement.to_dict` | code → pack: absent from section 2 |
| `region_refinement.refinement_for_extent` | code → pack: absent from section 2 |
| `region_refinement.refinement_for_bbox` | code → pack: absent from section 2 |
| `region_refinement.derive_region_levels` | code → pack: absent from section 2 |
| `report.render_case_report` | code → pack: absent from section 2 |
| `report.render_aggregate_report` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.total_fx` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.total_fy` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.total_fz` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.peak_fx` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.peak_fy` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.peak_fz` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.mean_fx` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.mean_fy` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.mean_fz` | code → pack: absent from section 2 |
| `results_models.ForceTimeSeries.to_dict` | code → pack: absent from section 2 |
| `results_models.ProbeTimeSeries` | code → pack: absent from section 2 |
| `results_models.ProbeTimeSeries.n_probes` | code → pack: absent from section 2 |
| `results_models.ProbeTimeSeries.n_timesteps` | code → pack: absent from section 2 |
| `results_models.ProbeTimeSeries.at_probe` | code → pack: absent from section 2 |
| `results_models.FreeSurfaceElevation` | code → pack: absent from section 2 |
| `results_models.FreeSurfaceElevation.n_positions` | code → pack: absent from section 2 |
| `results_models.FreeSurfaceElevation.n_timesteps` | code → pack: absent from section 2 |
| `results_models.FreeSurfaceElevation.max_elevation` | code → pack: absent from section 2 |
| `results_models.FreeSurfaceElevation.min_elevation` | code → pack: absent from section 2 |
| `runner.OpenFOAMRunStatus` | code → pack: absent from section 2 |
| `runner.OpenFOAMRunConfig` | code → pack: absent from section 2 |
| `runner.StageResult` | code → pack: absent from section 2 |
| `runner.StageResult.ok` | code → pack: absent from section 2 |
| `runner.OpenFOAMRunResult` | code → pack: absent from section 2 |
| `runner.OpenFOAMRunResult.vtk_dir` | code → pack: absent from section 2 |
| `runner.OpenFOAMRunner` | code → pack: absent from section 2 |
| `runner.OpenFOAMRunner.run` | code → pack: absent from section 2 |
| `runner.PollResult` | code → pack: absent from section 2 |
| `runner.PollResult.over_budget` | code → pack: absent from section 2 |
| `runner.poll_detached_run` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.from_cases` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.from_rows` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.from_sweep_manifest` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.fill_levels` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.n_cases` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.omega_range` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.moment_coefficients` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.added_roll_inertia` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.added_roll_damping` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.sloshing_moment` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.moment_callable` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.moment_time_series` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.moment_from_harmonic` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.best_antiroll_fill` | code → pack: absent from section 2 |
| `sloshing_coupling.SloshingCouplingModel.coupling_strength` | code → pack: absent from section 2 |
| `sloshing_coupling_analysis.moment_time_series` | code → pack: absent from section 2 |
| `sloshing_coupling_analysis.moment_from_harmonic` | code → pack: absent from section 2 |
| `sloshing_coupling_analysis.best_antiroll_fill` | code → pack: absent from section 2 |
| `sloshing_coupling_analysis.coupling_strength` | code → pack: absent from section 2 |
| `sloshing_coupling_models.SloshingCase` | code → pack: absent from section 2 |
| `sloshing_coupling_models.SloshingCase.omega` | code → pack: absent from section 2 |
| `sloshing_coupling_models.SloshingCase.roll_amplitude_rad` | code → pack: absent from section 2 |
| `sloshing_coupling_models.SloshingCase.from_row` | code → pack: absent from section 2 |
| `sloshing_coupling_models.MomentCoefficients` | code → pack: absent from section 2 |
| `sloshing_coupling_models.MomentCoefficients.added_roll_inertia` | code → pack: absent from section 2 |
| `sloshing_coupling_models.MomentCoefficients.added_roll_stiffness` | code → pack: absent from section 2 |
| `sloshing_coupling_models.MomentCoefficients.added_roll_damping` | code → pack: absent from section 2 |
| `sloshing_coupling_models.MomentCoefficients.moment` | code → pack: absent from section 2 |
| `sloshing_coupling_models.FillDampingResult` | code → pack: absent from section 2 |
| `sloshing_coupling_models.TuningReport` | code → pack: absent from section 2 |
| `sloshing_coupling_models.TuningReport.summary` | code → pack: absent from section 2 |
| `sloshing_coupling_models.CouplingStrengthReport` | code → pack: absent from section 2 |
| `sloshing_coupling_models.CouplingStrengthReport.summary` | code → pack: absent from section 2 |
| `sloshing_sweep.NaturalPeriodResult` | code → pack: absent from section 2 |
| `sloshing_sweep.first_sloshing_natural_period` | code → pack: absent from section 2 |
| `sloshing_sweep.period_ratio` | code → pack: absent from section 2 |
| `sloshing_sweep.SweepConfig` | code → pack: absent from section 2 |
| `sloshing_sweep.SweepConfig.effective_period_ratios` | code → pack: absent from section 2 |
| `sloshing_sweep.SweepConfig.schema_key` | code → pack: absent from section 2 |
| `sloshing_sweep.SweepCase` | code → pack: absent from section 2 |
| `sloshing_sweep.SweepManifest` | code → pack: absent from section 2 |
| `sloshing_sweep.generate_sweep` | code → pack: absent from section 2 |
| `smoke.SmokeError` | code → pack: absent from section 2 |
| `smoke.NotRunnable` | code → pack: absent from section 2 |
| `smoke.assert_log_advanced` | code → pack: absent from section 2 |
| `smoke.sha256_manifest` | code → pack: absent from section 2 |
| `smoke.SmokePlan` | code → pack: absent from section 2 |
| `smoke.SmokePlan.commands` | code → pack: absent from section 2 |
| `smoke.SmokePlan.flat_argv` | code → pack: absent from section 2 |
| `smoke.StageEvidence` | code → pack: absent from section 2 |
| `smoke.StageEvidence.to_dict` | code → pack: absent from section 2 |
| `smoke.MotionCheck` | code → pack: absent from section 2 |
| `smoke.SmokeResult` | code → pack: absent from section 2 |
| `smoke.SmokeResult.status` | code → pack: absent from section 2 |
| `smoke.SmokeResult.max_displacement` | code → pack: absent from section 2 |
| `smoke.SmokeResult.max_rotation_error` | code → pack: absent from section 2 |
| `smoke.SmokeResult.seconds_per_step` | code → pack: absent from section 2 |
| `smoke.plan_smoke` | code → pack: absent from section 2 |
| `smoke.parse_points` | code → pack: absent from section 2 |
| `smoke.verify_rigid_rotation` | code → pack: absent from section 2 |
| `smoke.execute_smoke` | code → pack: absent from section 2 |
| `smoke.write_reduced_evidence` | code → pack: absent from section 2 |
| `smoke_evidence.EvidenceValidationError` | code → pack: absent from section 2 |
| `smoke_evidence.validate_privacy` | code → pack: absent from section 2 |
| `smoke_evidence.validate_evidence` | code → pack: absent from section 2 |
| `smoke_evidence.cross_check_bridge_manifest` | code → pack: absent from section 2 |
| `smoke_evidence.build_evidence_payload` | code → pack: absent from section 2 |
| `smoke_evidence.write_evidence` | code → pack: absent from section 2 |
| `smoke_evidence.capture_pre_run_artifacts` | code → pack: absent from section 2 |
| `smoke_evidence.main` | code → pack: absent from section 2 |
| `solver_contracts.SolverDictContract` | code → pack: absent from section 2 |
| `solver_contracts.contract_for` | code → pack: absent from section 2 |
| `solver_contracts.render_div_schemes` | code → pack: absent from section 2 |
| `solver_contracts.render_fv_schemes_body` | code → pack: absent from section 2 |
| `spectral_analysis.SpectralPeak` | code → pack: absent from section 2 |
| `spectral_analysis.SpectrumResult` | code → pack: absent from section 2 |
| `spectral_analysis.SpectrumResult.dominant_frequency` | code → pack: absent from section 2 |
| `spectral_analysis.SpectrumResult.dominant_period` | code → pack: absent from section 2 |
| `spectral_analysis.SpectrumResult.top_frequencies` | code → pack: absent from section 2 |
| `spectral_analysis.compute_fft_spectrum` | code → pack: absent from section 2 |
| `spectral_analysis.compute_welch_spectrum` | code → pack: absent from section 2 |
| `spectral_analysis.extract_natural_frequency` | code → pack: absent from section 2 |
| `spectral_analysis.prismatic_tank_natural_frequency` | code → pack: absent from section 2 |
| `spectral_analysis.SloshingFrequencyResult` | code → pack: absent from section 2 |
| `spectral_analysis.SloshingFrequencyResult.abs_percent_error` | code → pack: absent from section 2 |
| `spectral_analysis.sloshing_natural_frequency` | code → pack: absent from section 2 |
| `time_history.validate_synchronized_time` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.n_samples` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.is_multichannel` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.peak` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.trough` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.mean` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.spatial_max` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.spatial_min` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.spatial_mean` | code → pack: absent from section 2 |
| `time_history.TimeSeriesChannel.to_provenance` | code → pack: absent from section 2 |
| `time_history.ValidationFlags` | code → pack: absent from section 2 |
| `time_history.ValidationFlags.to_dict` | code → pack: absent from section 2 |
| `time_history.SynchronizedTimeHistory` | code → pack: absent from section 2 |
| `time_history.SynchronizedTimeHistory.from_channels` | code → pack: absent from section 2 |
| `time_history.SynchronizedTimeHistory.pressure_vs_elevation` | code → pack: absent from section 2 |
| `time_history.SynchronizedTimeHistory.pressure_tap_statistics` | code → pack: absent from section 2 |
| `time_history.SynchronizedTimeHistory.phase_lag_deg` | code → pack: absent from section 2 |
| `time_history.phase_lag_deg` | code → pack: absent from section 2 |
| `time_history.RawCFDOutputs` | code → pack: absent from section 2 |
| `time_history.ExtractionConfig` | code → pack: absent from section 2 |
| `time_history.extract_time_history` | code → pack: absent from section 2 |
| `wave_cut.read_raw` | code → pack: absent from section 2 |
| `wave_cut.extract_eta` | code → pack: absent from section 2 |
| `wave_cut.summarize_cut` | code → pack: absent from section 2 |
| `wave_cut.summarize_iso` | code → pack: absent from section 2 |
| `wave_cut.extract_iso_eta` | code → pack: absent from section 2 |
| `wave_cut.reduce_files` | code → pack: absent from section 2 |
| `wave_cut.main` | code → pack: absent from section 2 |
| `wave_models.WaveTheory` | code → pack: absent from section 2 |
| `wave_models.CurrentProfileType` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC.amplitude` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC.angular_frequency` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC.wave_number` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC.wave_length` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC.phase_speed` | code → pack: absent from section 2 |
| `wave_models.RegularWaveBC.to_dict` | code → pack: absent from section 2 |
| `wave_models.IrregularWaveBC` | code → pack: absent from section 2 |
| `wave_models.IrregularWaveBC.peak_frequency` | code → pack: absent from section 2 |
| `wave_models.IrregularWaveBC.to_dict` | code → pack: absent from section 2 |
| `wave_models.CurrentProfile` | code → pack: absent from section 2 |
| `wave_models.CurrentProfile.speed_at_depth` | code → pack: absent from section 2 |
| `wave_models.CurrentProfile.to_dict` | code → pack: absent from section 2 |
| `workflow.OpenFOAMWorkflow` | code → pack: absent from section 2 |
| `workflow.OpenFOAMWorkflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 62. `src/digitalmodel/solvers/orcaflex/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 673.

| Function | Direction |
| --- | --- |
| `__init__.get_version` | code → pack: absent from section 2 |
| `__init__.check_availability` | code → pack: absent from section 2 |
| `__init__.list_cli_commands` | code → pack: absent from section 2 |
| `all_vars.AllVars` | code → pack: absent from section 2 |
| `all_vars.AllVars.router` | code → pack: absent from section 2 |
| `all_vars.AllVars.get_var_data_by_model` | code → pack: absent from section 2 |
| `all_vars.AllVars.get_var_data` | code → pack: absent from section 2 |
| `all_vars.AllVars.get_var_data_StaticResult` | code → pack: absent from section 2 |
| `all_vars.AllVars.initialize_var_data_output_dict` | code → pack: absent from section 2 |
| `all_vars.AllVars.save_var_data` | code → pack: absent from section 2 |
| `comprehensive_benchmark.BenchmarkResult` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark.create_test_datasets` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark.run_benchmark` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark.run_comparison_suite` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark.generate_summary` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark.save_results` | code → pack: absent from section 2 |
| `comprehensive_benchmark.ComprehensiveBenchmark.create_visualizations` | code → pack: absent from section 2 |
| `comprehensive_benchmark.run_comprehensive_benchmark` | code → pack: absent from section 2 |
| `convert_cli.convert_single` | code → pack: absent from section 2 |
| `convert_cli.convert_batch` | code → pack: absent from section 2 |
| `convert_cli.main` | code → pack: absent from section 2 |
| `deploy_production_optimization.ProductionDeployment` | code → pack: absent from section 2 |
| `deploy_production_optimization.ProductionDeployment.verify_production_files` | code → pack: absent from section 2 |
| `deploy_production_optimization.ProductionDeployment.test_thread_optimization` | code → pack: absent from section 2 |
| `deploy_production_optimization.ProductionDeployment.validate_optimizations` | code → pack: absent from section 2 |
| `deploy_production_optimization.ProductionDeployment.generate_deployment_report` | code → pack: absent from section 2 |
| `deploy_production_optimization.ProductionDeployment.create_production_config` | code → pack: absent from section 2 |
| `deploy_production_optimization.main` | code → pack: absent from section 2 |
| `dynamics_progress.dynamics_progress` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer.get_file_size_mb` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer.analyze_files` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer.get_optimal_threads` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer.get_optimal_threads_for_single_file` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer.group_files_by_optimal_threads` | code → pack: absent from section 2 |
| `file_size_optimizer.FileSizeOptimizer.recommend_batch_strategy` | code → pack: absent from section 2 |
| `file_size_optimizer.demonstrate_optimizer` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.scan_directory` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.scan_lngc_files` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.scan_standard_files` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.create_size_histogram` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.analyze_size_distribution` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.save_profile` | code → pack: absent from section 2 |
| `file_size_profiler.FileSizeProfiler.print_summary` | code → pack: absent from section 2 |
| `file_size_profiler.main` | code → pack: absent from section 2 |
| `hybrid_processor.TaskType` | code → pack: absent from section 2 |
| `hybrid_processor.WorkItem` | code → pack: absent from section 2 |
| `hybrid_processor.HybridExecutor` | code → pack: absent from section 2 |
| `hybrid_processor.HybridExecutor.start` | code → pack: absent from section 2 |
| `hybrid_processor.HybridExecutor.submit` | code → pack: absent from section 2 |
| `hybrid_processor.HybridExecutor.map_async` | code → pack: absent from section 2 |
| `hybrid_processor.HybridExecutor.shutdown` | code → pack: absent from section 2 |
| `hybrid_processor.HybridExecutor.get_stats` | code → pack: absent from section 2 |
| `hybrid_processor.WorkDistributor` | code → pack: absent from section 2 |
| `hybrid_processor.WorkDistributor.classify_task` | code → pack: absent from section 2 |
| `hybrid_processor.WorkDistributor.distribute_work` | code → pack: absent from section 2 |
| `hybrid_processor.WorkDistributor.optimize_distribution` | code → pack: absent from section 2 |
| `hybrid_processor.HybridOrcaFlexProcessor` | code → pack: absent from section 2 |
| `hybrid_processor.HybridOrcaFlexProcessor.process_files` | code → pack: absent from section 2 |
| `hybrid_processor.benchmark_hybrid_model` | code → pack: absent from section 2 |
| `inverse_resolver.Outcome` | code → pack: absent from section 2 |
| `inverse_resolver.MooringResolverInputs` | code → pack: absent from section 2 |
| `inverse_resolver.RiserResolverInputs` | code → pack: absent from section 2 |
| `inverse_resolver.PipelineResolverInputs` | code → pack: absent from section 2 |
| `inverse_resolver.resolve` | code → pack: absent from section 2 |
| `io_optimizer.IOOptimizer` | code → pack: absent from section 2 |
| `io_optimizer.IOOptimizer.async_read_file` | code → pack: absent from section 2 |
| `io_optimizer.IOOptimizer.async_write_file` | code → pack: absent from section 2 |
| `io_optimizer.IOOptimizer.batch_read_files` | code → pack: absent from section 2 |
| `io_optimizer.IOOptimizer.batch_write_files` | code → pack: absent from section 2 |
| `io_optimizer.IOOptimizer.get_io_stats` | code → pack: absent from section 2 |
| `io_optimizer.BufferedFileWriter` | code → pack: absent from section 2 |
| `io_optimizer.BufferedFileWriter.write` | code → pack: absent from section 2 |
| `io_optimizer.BufferedFileWriter.flush` | code → pack: absent from section 2 |
| `io_optimizer.BufferedFileWriter.close` | code → pack: absent from section 2 |
| `io_optimizer.ParallelIOProcessor` | code → pack: absent from section 2 |
| `io_optimizer.ParallelIOProcessor.process_files_parallel` | code → pack: absent from section 2 |
| `io_optimizer.CachedFileReader` | code → pack: absent from section 2 |
| `io_optimizer.CachedFileReader.read` | code → pack: absent from section 2 |
| `io_optimizer.CachedFileReader.get_cache_stats` | code → pack: absent from section 2 |
| `io_optimizer.io_optimization_benchmark` | code → pack: absent from section 2 |
| `library_generator.csv_row_to_line_type` | code → pack: absent from section 2 |
| `library_generator.csv_row_to_buoy_type` | code → pack: absent from section 2 |
| `library_generator.csv_row_to_vessel_type` | code → pack: absent from section 2 |
| `library_generator.LibraryGenerator` | code → pack: absent from section 2 |
| `library_generator.LibraryGenerator.generate_component` | code → pack: absent from section 2 |
| `library_generator.LibraryGenerator.generate_from_csv` | code → pack: absent from section 2 |
| `library_generator.LibraryGenerator.generate_index` | code → pack: absent from section 2 |
| `library_generator.LibraryGenerator.write_index` | code → pack: absent from section 2 |
| `library_generator.main` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryPool` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryPool.acquire` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryPool.release` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryPool.get_stats` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.OptimizedDataStructures` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.OptimizedDataStructures.create_compact_array` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.OptimizedDataStructures.create_numpy_array` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.OptimizedDataStructures.create_bounded_cache` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.OptimizedDataStructures.optimize_dict_memory` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryMappedBuffer` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryMappedBuffer.write` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryMappedBuffer.read` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryMappedBuffer.reset` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.MemoryMappedBuffer.close` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.WeakReferenceCache` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.WeakReferenceCache.set` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.WeakReferenceCache.get` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.WeakReferenceCache.clear_strong_refs` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer.create_pool` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer.get_from_pool` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer.return_to_pool` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer.optimize_large_data` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer.aggressive_cleanup` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.AdvancedMemoryOptimizer.get_optimization_report` | code → pack: absent from section 2 |
| `memory_optimizer_advanced.benchmark_memory_optimizations` | code → pack: absent from section 2 |
| `modal_benchmark.load_targets` | code → pack: absent from section 2 |
| `modal_benchmark.load_result` | code → pack: absent from section 2 |
| `modal_benchmark.compare_modes` | code → pack: absent from section 2 |
| `modal_benchmark.write_comparison` | code → pack: absent from section 2 |
| `modal_benchmark.main` | code → pack: absent from section 2 |
| `model_catalogue.load_renderer` | code → pack: absent from section 2 |
| `model_catalogue.classify_provenance` | code → pack: absent from section 2 |
| `model_catalogue.claim_levels` | code → pack: absent from section 2 |
| `model_catalogue.render_catalogue` | code → pack: absent from section 2 |
| `model_catalogue.model_name` | code → pack: absent from section 2 |
| `model_catalogue.model_path` | code → pack: absent from section 2 |
| `model_catalogue.read_text` | code → pack: absent from section 2 |
| `model_catalogue.source_groups` | code → pack: absent from section 2 |
| `model_catalogue.catalogue_entries` | code → pack: absent from section 2 |
| `model_catalogue.find_usage` | code → pack: absent from section 2 |
| `model_catalogue.extracted_models` | code → pack: absent from section 2 |
| `model_catalogue.build` | code → pack: absent from section 2 |
| `model_catalogue_render.number` | code → pack: absent from section 2 |
| `model_catalogue_render.fmt` | code → pack: absent from section 2 |
| `model_catalogue_render.object_sections` | code → pack: absent from section 2 |
| `model_catalogue_render.object_counts` | code → pack: absent from section 2 |
| `model_catalogue_render.environment_summary` | code → pack: absent from section 2 |
| `model_catalogue_render.position` | code → pack: absent from section 2 |
| `model_catalogue_render.line_ends` | code → pack: absent from section 2 |
| `model_catalogue_render.geometry` | code → pack: absent from section 2 |
| `model_catalogue_render.sketch` | code → pack: absent from section 2 |
| `model_catalogue_render.parse_model` | code → pack: absent from section 2 |
| `model_catalogue_render.card_metadata` | code → pack: absent from section 2 |
| `model_catalogue_render.card_geometry` | code → pack: absent from section 2 |
| `model_catalogue_render.render_card` | code → pack: absent from section 2 |
| `model_catalogue_render.page_template` | code → pack: absent from section 2 |
| `mooring.Mooring` | code → pack: absent from section 2 |
| `mooring.Mooring.convert_numpy_types` | code → pack: absent from section 2 |
| `mooring.Mooring.router` | code → pack: absent from section 2 |
| `mooring.Mooring.pretension_analysis` | code → pack: absent from section 2 |
| `mooring.Mooring.mooring_pre_tension_analysis` | code → pack: absent from section 2 |
| `mooring.Mooring.evaluate_line_length_next_iteration` | code → pack: absent from section 2 |
| `mooring.Mooring.evaluate_line_length_current` | code → pack: absent from section 2 |
| `mooring.Mooring.fender_force_analysis` | code → pack: absent from section 2 |
| `mooring.Mooring.evaluate_compression_current` | code → pack: absent from section 2 |
| `mooring.Mooring.evaluate_compression_next_iteration` | code → pack: absent from section 2 |
| `mooring.Mooring.get_tension_criteria` | code → pack: absent from section 2 |
| `mooring.Mooring.get_fender_criteria` | code → pack: absent from section 2 |
| `mooring.Mooring.get_iteration_flag` | code → pack: absent from section 2 |
| `mooring.Mooring.prepare_includefile_for_all_lines` | code → pack: absent from section 2 |
| `mooring.Mooring.prepare_includefile_for_fender_compression` | code → pack: absent from section 2 |
| `mooring.Mooring.get_tension` | code → pack: absent from section 2 |
| `mooring.Mooring.get_length` | code → pack: absent from section 2 |
| `mooring.Mooring.get_target_pretension` | code → pack: absent from section 2 |
| `mooring.Mooring.get_target_fender_force` | code → pack: absent from section 2 |
| `mooring.Mooring.calculate_mooring_stiffness_3d` | code → pack: absent from section 2 |
| `operability_analysis.OperabilityAnalyzer` | code → pack: absent from section 2 |
| `operability_analysis.OperabilityAnalyzer.load_simulation_results` | code → pack: absent from section 2 |
| `operability_analysis.OperabilityAnalyzer.generate_operability_envelope` | code → pack: absent from section 2 |
| `operability_analysis.OperabilityAnalyzer.calculate_weather_downtime` | code → pack: absent from section 2 |
| `operability_analysis.OperabilityAnalyzer.generate_critical_headings_report` | code → pack: absent from section 2 |
| `operability_analysis.OperabilityAnalyzer.generate_comprehensive_report` | code → pack: absent from section 2 |
| `opp.process_single_file` | code → pack: absent from section 2 |
| `opp.aggregate_results` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.post_process_router` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.get_cfg_with_master_data` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.get_cfg_with_expanded_postprocess` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.post_process` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.get_cfg_with_linked_statistics_master_data` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.get_cfg_with_time_series_master_data` | code → pack: absent from section 2 |
| `opp.OrcaFlexPostProcess.get_cfg_with_summary_master_data` | code → pack: absent from section 2 |
| `opp_config_expansion.build_summary_settings` | code → pack: absent from section 2 |
| `opp_config_expansion.build_range_graph` | code → pack: absent from section 2 |
| `opp_config_expansion.expand_high_level_postprocess` | code → pack: absent from section 2 |
| `opp_config_expansion.get_object_names_from_model` | code → pack: absent from section 2 |
| `opp_linkedstatistics.OPPLinkedStatistics` | code → pack: absent from section 2 |
| `opp_linkedstatistics.OPPLinkedStatistics.get_linked_statistics` | code → pack: absent from section 2 |
| `opp_linkedstatistics.OPPLinkedStatistics.get_linked_statistics_from_orcaflex_run` | code → pack: absent from section 2 |
| `opp_linkedstatistics.OPPLinkedStatistics.add_file_result_to_all_results` | code → pack: absent from section 2 |
| `opp_linkedstatistics.OPPLinkedStatistics.save_linked_statistics` | code → pack: absent from section 2 |
| `opp_range_graph.OPPRangeGraph` | code → pack: absent from section 2 |
| `opp_range_graph.OPPRangeGraph.get_RangeGraph` | code → pack: absent from section 2 |
| `opp_range_graph.OPPRangeGraph.postProcessRange` | code → pack: absent from section 2 |
| `opp_range_graph.OPPRangeGraph.add_result_to_cfg` | code → pack: absent from section 2 |
| `opp_range_graph.OPPRangeGraph.process_range_graphs` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_summary_for_file` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_summary_from_orcaflex_run` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.add_file_result_to_all_results` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.save_summary` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.process_summary_by_model_and_cfg_item` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_summary_df_columns` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_summary_value_columns` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_additional_data` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_StaticResult` | code → pack: absent from section 2 |
| `opp_summary.OPPSummary.get_input_data` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.process_summary_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.process_summary_groups_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.postProcessSummary_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.get_loading_condition_array_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.save_summary_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.save_summary_to_csv_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.injectSummaryToNewExcel_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.saveSummaryToNewExcel_superseded` | code → pack: absent from section 2 |
| `opp_summary_superseded.OPPSummary_superseded.transform_output_superseded` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.router` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_time_series_data` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_time_series_from_orcaflex_run` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_TimeHistory` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.post_process_RAOs` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_cfg_fft` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_RAOs_from_time_series_data` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.assign_file_info_to_Variable_df` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_RAO` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_time_series_output` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_cfg_time_series_reference` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_cfg_time_series_custom` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.prepare_histogram_bins_for_output` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.generate_cummulative_histograms` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.qa_histograms` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.save_histograms` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.post_process_histograms` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_histogram_object_from_orcaflex_run` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_detailed_histogram_df_columns` | code → pack: absent from section 2 |
| `opp_time_series.OPPTimeSeries.get_detailed_RAO_df_columns` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2.router` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2.process_time_series_data` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2.process_fatigue_analysis` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2.process_spectral_analysis` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2.process_histograms` | code → pack: absent from section 2 |
| `opp_time_series_v2.OPPTimeSeriesV2.process_RAOs` | code → pack: absent from section 2 |
| `opp_time_series_v2.migrate_configuration` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.get_visualizations` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.is_file_valid` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.save_views_for_files` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.set_general_visualization_settings` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.combine_models` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.save_all_views` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.assign_view_parameters` | code → pack: absent from section 2 |
| `opp_visualization.OPPVisualization.save_image` | code → pack: absent from section 2 |
| `orcaflex.OrcaFlex` | code → pack: absent from section 2 |
| `orcaflex.OrcaFlex.router` | code → pack: absent from section 2 |
| `orcaflex_analysis.OrcaflexAnalysis` | code → pack: absent from section 2 |
| `orcaflex_analysis.OrcaflexAnalysis.router` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_model_state_information` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_files` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.perform_analysis` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.assign_and_summarize_files` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.process_fea` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.post_process_files` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.save_data` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.post_process_RAOs` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_cfg_fft` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_RAOs_from_time_series_data` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.assign_file_info_to_Variable_df` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_RAO` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_time_series_output` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_cfg_time_series_reference` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_cfg_time_series_custom` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.process_range_graphs` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.process_summary` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.saveSummaryToExcel` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.postProcess` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_model_from_filename` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_SimulationFileName` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.loadSimulation` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.postProcessRange` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.post_process_histograms` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_histogram_object_from_orcaflex_run` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_time_series_from_orcaflex_run` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.add_result_to_cfg` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.postProcessSummary` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_RangeGraph` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_TimePeriodObject` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_ArcLengthObject` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.prepare_histogram_bins_for_output` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_loading_condition_array` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_seastate_probability` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_SimulationDuration` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_AddSummary_array` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_summary_df_columns` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_summary_value_columns` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_detailed_histogram_df_columns` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_detailed_RAO_df_columns` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.generate_cummulative_histograms` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.qa_histograms` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.save_histograms` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.LinkedStatistics` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.process_scripts` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.save_cfg_files_from_multiple_files` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_files_for_postprocess` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_files_for_analysis` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_simulation_filenames` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.get_filename_without_extension` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.transform_output` | code → pack: absent from section 2 |
| `orcaflex_analysis_components.OrcaFlexAnalysis.save_RAOs` | code → pack: absent from section 2 |
| `orcaflex_api.OrcaFlexApiError` | code → pack: absent from section 2 |
| `orcaflex_api.OrcFxAPIAlreadyImportedError` | code → pack: absent from section 2 |
| `orcaflex_api.OrcaFlexVersionUnavailableError` | code → pack: absent from section 2 |
| `orcaflex_api.installed_versions` | code → pack: absent from section 2 |
| `orcaflex_api.available` | code → pack: absent from section 2 |
| `orcaflex_api.is_configured` | code → pack: absent from section 2 |
| `orcaflex_api.configure` | code → pack: absent from section 2 |
| `orcaflex_api.api` | code → pack: absent from section 2 |
| `orcaflex_api.lazy_api` | code → pack: absent from section 2 |
| `orcaflex_converter_enhanced.OrcaFlexConverterEnhanced` | code → pack: absent from section 2 |
| `orcaflex_converter_enhanced.OrcaFlexConverterEnhanced.convert_file` | code → pack: absent from section 2 |
| `orcaflex_converter_enhanced.OrcaFlexConverterEnhanced.find_files` | code → pack: absent from section 2 |
| `orcaflex_converter_enhanced.OrcaFlexConverterEnhanced.convert_batch` | code → pack: absent from section 2 |
| `orcaflex_converter_enhanced.main` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.perform_simulations` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.get_files_for_analysis` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.process_fea` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.clean_model` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.run_static_analysis` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.analysis_with_calculated_positions` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.save_model_with_calculated_positions` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.iterate_to_target_value` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.get_variable_current_value_for_iteration` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.save_data` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.save_RAOs` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.process_summary_by_model_and_cfg` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.process_files_parallel` | code → pack: absent from section 2 |
| `orcaflex_custom_analysis.OrcaFlexCustomAnalysis.process_scripts` | code → pack: absent from section 2 |
| `orcaflex_fatigue_analysis.fatigue_analysis` | code → pack: absent from section 2 |
| `orcaflex_file_management.OrcaflexFileManagement` | code → pack: absent from section 2 |
| `orcaflex_file_management.OrcaflexFileManagement.file_management` | code → pack: absent from section 2 |
| `orcaflex_file_management.OrcaflexFileManagement.sim_file_analysis_and_update` | code → pack: absent from section 2 |
| `orcaflex_installation.OrcInstallation` | code → pack: absent from section 2 |
| `orcaflex_installation.OrcInstallation.create_model_for_water_depth` | code → pack: absent from section 2 |
| `orcaflex_installation.OrcInstallation.update_lines_for_installation` | code → pack: absent from section 2 |
| `orcaflex_installation.OrcInstallation.update_elevation_for_orc_object` | code → pack: absent from section 2 |
| `orcaflex_installation.OrcInstallation.create_installation_depth_model` | code → pack: absent from section 2 |
| `orcaflex_installation.OrcInstallation.create_installation_depth_model2` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.prepare_iterative_runs` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.make_copy_per_iterative_guideline` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.get_filename_copy` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.save_iterative_file` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.get_iterative_filename` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.find_related_files` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.get_iterative_yaml` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.iterate_add_keys` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.iterate_delete_keys` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.iterate_replace_values` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.get_filename_components` | code → pack: absent from section 2 |
| `orcaflex_iterative_runs.OrcaflexIterativeRuns.run_iterative_simulations` | code → pack: absent from section 2 |
| `orcaflex_linetypes.OrcaflexLineTypes` | code → pack: absent from section 2 |
| `orcaflex_linetypes.OrcaflexLineTypes.get_umbilical_lines` | code → pack: absent from section 2 |
| `orcaflex_linetypes.OrcaflexLineTypes.get_umbilical_properties` | code → pack: absent from section 2 |
| `orcaflex_linetypes.OrcaflexLineTypes.assign_key_umbilical_properties` | code → pack: absent from section 2 |
| `orcaflex_linetypes.OrcaflexLineTypes.save_model` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.run_modal_analysis` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.summarize_all_files` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.run_by_file` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.explore_mode_shapes` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.save_mode_shapes` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.save_summary_files` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.get_mode_summary` | code → pack: absent from section 2 |
| `orcaflex_modal_analysis.OrcModalAnalysis.get_mode_dof_responses` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.orcaflex_model` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.riser_joints_to_line_types` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.GetGeneral` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.GetEnvironment` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.seaAndSeabed` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.wave` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.current` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.wind` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.vesselTypes` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.supportTypes` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.codeChecks` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.RayleighDampingCoefficients` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.lineTypes` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.lines` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.customize_fj_types` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.customize_py_models` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.links` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.add_support_spring` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.add_tensioners` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.tensioner_vessel_locations` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.spring_properties` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.get_connection_definitions` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.get_connection_stiffnesses` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.get_fj_line_definitions` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.get_line_items_vertical` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.VariableData` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.BendingConnectionStiffness` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.LineTypeOuterDiameter` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.vessels` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.winches` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.groups` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.WriteOrcaflexModel` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.build_model` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.line_items_catenary` | code → pack: absent from section 2 |
| `orcaflex_model_components.OrcaflexModelComponents.variable_data` | code → pack: absent from section 2 |
| `orcaflex_model_linesetup_wizard.LineSetUpWizard` | code → pack: absent from section 2 |
| `orcaflex_model_linesetup_wizard.LineSetUpWizard.define_calculation_parameters` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities.get_6d_buoy_template` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities.get_umbilical_LineType_template` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities.get_wave_template` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities.get_current_template` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities.get_wave_template_with_n_waves` | code → pack: absent from section 2 |
| `orcaflex_model_utilities.OrcaflexModelUtilities.get_BaseFile_first` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_orcaflex_objects` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_arc_length_objects` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_ArcLengthObject` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_OrcFXAPIObject` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_SimulationPeriod` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_TimePeriodObject` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_objectExtra` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_objectExtra_vessel` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_objectExtra_rigid_pipe` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_input_data_variable_name` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_model_objects` | code → pack: absent from section 2 |
| `orcaflex_objects.OrcaFlexObjects.get_object_vars` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel.OrcaFlexOptimizedParallelAnalysis` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel.OrcaFlexOptimizedParallelAnalysis.process_single_file` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel.OrcaFlexOptimizedParallelAnalysis.process_files_parallel` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel.OrcaFlexOptimizedParallelAnalysis.process_directory` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel.run_optimized_parallel_analysis` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.process_single_file_worker` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.SimpleResourceManager` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.SimpleResourceManager.calculate_optimal_threads` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.OrcaFlexOptimizedParallelAnalysis` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.OrcaFlexOptimizedParallelAnalysis.process_files_parallel` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.OrcaFlexOptimizedParallelAnalysis.process_directory` | code → pack: absent from section 2 |
| `orcaflex_optimized_parallel_v2.run_optimized_parallel_analysis` | code → pack: absent from section 2 |
| `orcaflex_parallel_analysis.OrcaFlexParallelAnalysis` | code → pack: absent from section 2 |
| `orcaflex_parallel_analysis.OrcaFlexParallelAnalysis.process_single_file` | code → pack: absent from section 2 |
| `orcaflex_parallel_analysis.OrcaFlexParallelAnalysis.process_files_parallel` | code → pack: absent from section 2 |
| `orcaflex_parallel_analysis.OrcaFlexParallelAnalysis.process_directory` | code → pack: absent from section 2 |
| `orcaflex_parallel_analysis.OrcaFlexParallelAnalysis.get_optimal_thread_count` | code → pack: absent from section 2 |
| `orcaflex_parallel_analysis.run_parallel_analysis` | code → pack: absent from section 2 |
| `orcaflex_preprocess.OrcaflexPreProcess` | code → pack: absent from section 2 |
| `orcaflex_preprocess.OrcaflexPreProcess.router` | code → pack: absent from section 2 |
| `orcaflex_preprocess.OrcaflexPreProcess.check_yml_file` | code → pack: absent from section 2 |
| `orcaflex_preprocess.OrcaflexPreProcess.check_1_yml_file` | code → pack: absent from section 2 |
| `orcaflex_utilities.mock_mode_requested` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.require_orcaflex_or_raise` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.is_orcaflex_available` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.save_sim_file` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.file_run_and_save` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.update_model` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_sim_file_finish_status` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.update_input_file` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.file_management` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_files` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.sim_file_analysis_and_update` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.assign_and_summarize_files` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_simulation_filenames` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.prepare_operating_window_definition` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.prepare_operating_window_for_input_set` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_full_yaml_file_and_save` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_general_yml` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_random_wave_seeds` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_wave_peak_enhancement_factor_by_region` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_theoretical_wave_peak_enhancement_factor` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_theoretical_tz` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_theoretical_hmax` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_theoretical_tassociated` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_load_matrix_with_filenames` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_model_and_metadata` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_SimulationFileName` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_filename_without_extension` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.loadSimulation` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_seastate_probability` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_SimulationDuration` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_AddSummary_array` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.save_cfg_files_from_multiple_files` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.get_files_for_postprocess` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.save_RAOs` | code → pack: absent from section 2 |
| `orcaflex_utilities.OrcaflexUtilities.add_basic_statistics_to_df` | code → pack: absent from section 2 |
| `orcaflex_yml_converter.convert_to_yml` | code → pack: absent from section 2 |
| `orcaflex_yml_converter.main` | code → pack: absent from section 2 |
| `output_control.OutputController` | code → pack: absent from section 2 |
| `output_control.OutputController.from_config` | code → pack: absent from section 2 |
| `output_control.OutputController.start_processing` | code → pack: absent from section 2 |
| `output_control.OutputController.file_started` | code → pack: absent from section 2 |
| `output_control.OutputController.file_completed` | code → pack: absent from section 2 |
| `output_control.OutputController.finish_processing` | code → pack: absent from section 2 |
| `output_control.OutputController.progress_context` | code → pack: absent from section 2 |
| `output_control.add_output_arguments` | code → pack: absent from section 2 |
| `output_control.get_output_level_from_argv` | code → pack: absent from section 2 |
| `parallel_runner.CaseFailed` | code → pack: absent from section 2 |
| `parallel_runner.fault_class` | code → pack: absent from section 2 |
| `parallel_runner.BatchGuard` | code → pack: absent from section 2 |
| `parallel_runner.BatchGuard.record` | code → pack: absent from section 2 |
| `parallel_runner.sha256_file` | code → pack: absent from section 2 |
| `parallel_runner.input_digest` | code → pack: absent from section 2 |
| `parallel_runner.reextract` | code → pack: absent from section 2 |
| `parallel_runner.campaign_cases` | code → pack: absent from section 2 |
| `parallel_runner.write_stop_record` | code → pack: absent from section 2 |
| `performance_monitor.PerformanceMonitor` | code → pack: absent from section 2 |
| `performance_monitor.PerformanceMonitor.start_monitoring` | code → pack: absent from section 2 |
| `performance_monitor.PerformanceMonitor.record_metric` | code → pack: absent from section 2 |
| `performance_monitor.PerformanceMonitor.get_summary` | code → pack: absent from section 2 |
| `performance_monitor.PerformanceMonitor.save_report` | code → pack: absent from section 2 |
| `performance_monitor.performance_timer` | code → pack: absent from section 2 |
| `performance_monitor.ResourceManager` | code → pack: absent from section 2 |
| `performance_monitor.ResourceManager.analyze_file_sizes` | code → pack: absent from section 2 |
| `performance_monitor.ResourceManager.calculate_optimal_threads` | code → pack: absent from section 2 |
| `performance_monitor.ResourceManager.monitor_resources` | code → pack: absent from section 2 |
| `performance_monitor.ResourceManager.should_adjust_threads` | code → pack: absent from section 2 |
| `performance_monitor.MemoryOptimizer` | code → pack: absent from section 2 |
| `performance_monitor.MemoryOptimizer.optimize_memory` | code → pack: absent from section 2 |
| `performance_monitor.MemoryOptimizer.cleanup_after_file` | code → pack: absent from section 2 |
| `performance_monitor.MemoryOptimizer.get_memory_stats` | code → pack: absent from section 2 |
| `performance_monitor.BatchOptimizer` | code → pack: absent from section 2 |
| `performance_monitor.BatchOptimizer.group_files_by_size` | code → pack: absent from section 2 |
| `performance_monitor.BatchOptimizer.get_optimal_batch_config` | code → pack: absent from section 2 |
| `performance_monitor.create_performance_dashboard` | code → pack: absent from section 2 |
| `pipeline_schematic.BoundaryConditionType` | code → pack: absent from section 2 |
| `pipeline_schematic.BoundaryCondition` | code → pack: absent from section 2 |
| `pipeline_schematic.BoundaryCondition.x` | code → pack: absent from section 2 |
| `pipeline_schematic.BoundaryCondition.y` | code → pack: absent from section 2 |
| `pipeline_schematic.BoundaryCondition.z` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineData` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.parse_model` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.create_plan_view` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.create_elevation_view` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.create_boundary_condition_legend` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.generate_html_report` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.save_individual_views` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.capture_orcaflex_screenshots` | code → pack: absent from section 2 |
| `pipeline_schematic.PipelineSchematicGenerator.generate_combined_report` | code → pack: absent from section 2 |
| `pipeline_schematic.OrcaFlexViewCapture` | code → pack: absent from section 2 |
| `pipeline_schematic.OrcaFlexViewCapture.load_model` | code → pack: absent from section 2 |
| `pipeline_schematic.OrcaFlexViewCapture.capture_view` | code → pack: absent from section 2 |
| `pipeline_schematic.OrcaFlexViewCapture.capture_all_views` | code → pack: absent from section 2 |
| `pipeline_schematic.OrcaFlexViewCapture.set_visualization_settings` | code → pack: absent from section 2 |
| `run_state.SimulationStateError` | code → pack: absent from section 2 |
| `run_state.state_name` | code → pack: absent from section 2 |
| `run_state.check_state` | code → pack: absent from section 2 |
| `run_state.check_statics` | code → pack: absent from section 2 |
| `run_state.check_simulation` | code → pack: absent from section 2 |
| `run_to_sim.OrcaFlexModelRunner` | code → pack: absent from section 2 |
| `run_to_sim.OrcaFlexModelRunner.describe_mode` | code → pack: absent from section 2 |
| `run_to_sim.OrcaFlexModelRunner.run_single_model` | code → pack: absent from section 2 |
| `run_to_sim.OrcaFlexModelRunner.run_batch` | code → pack: absent from section 2 |
| `run_to_sim.run_models` | code → pack: absent from section 2 |
| `run_to_sim_cli.setup_logging` | code → pack: absent from section 2 |
| `run_to_sim_cli.main` | code → pack: absent from section 2 |
| `schematic_capture.save_orcaflex_views` | code → pack: absent from section 2 |
| `schematic_capture.gdf_to_mesh3d` | code → pack: absent from section 2 |
| `schematic_capture.capture_model_schematic` | code → pack: absent from section 2 |
| `schematic_capture.img_to_base64` | code → pack: absent from section 2 |
| `sim_retention.latest_ok` | code → pack: absent from section 2 |
| `sim_retention.mark` | code → pack: absent from section 2 |
| `sim_retention.run_dirs` | code → pack: absent from section 2 |
| `sim_retention.pending_bytes` | code → pack: absent from section 2 |
| `sim_retention.marked_pending_bytes` | code → pack: absent from section 2 |
| `sim_retention.cycle` | code → pack: absent from section 2 |
| `sim_retention.main` | code → pack: absent from section 2 |
| `spec_author.OrcaFlexIntent` | code → pack: absent from section 2 |
| `spec_author.build_system_prompt` | code → pack: absent from section 2 |
| `spec_author.IntentAuthor` | code → pack: absent from section 2 |
| `spec_author.IntentAuthor.author` | code → pack: absent from section 2 |
| `spec_author.ClaudeCliIntentAuthor` | code → pack: absent from section 2 |
| `spec_author.ClaudeCliIntentAuthor.author` | code → pack: absent from section 2 |
| `spec_author.AnthropicIntentAuthor` | code → pack: absent from section 2 |
| `spec_author.AnthropicIntentAuthor.author` | code → pack: absent from section 2 |
| `spec_author.intent_to_resolver_inputs` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.to_spec_yaml` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.assumptions_markdown` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.write` | code → pack: absent from section 2 |
| `spec_author.AuthoredSpec.generate_model` | code → pack: absent from section 2 |
| `spec_author.main` | code → pack: absent from section 2 |
| `spec_upgrader.DetectedModelType` | code → pack: absent from section 2 |
| `spec_upgrader.DetectionResult` | code → pack: absent from section 2 |
| `spec_upgrader.detect_model_type` | code → pack: absent from section 2 |
| `spec_upgrader.upgrade_spec` | code → pack: absent from section 2 |
| `template_generator.TemplateInfo` | code → pack: absent from section 2 |
| `template_generator.TemplateInfo.to_dict` | code → pack: absent from section 2 |
| `template_generator.TemplateManager` | code → pack: absent from section 2 |
| `template_generator.TemplateManager.discover_templates` | code → pack: absent from section 2 |
| `template_generator.TemplateManager.get_template` | code → pack: absent from section 2 |
| `template_generator.TemplateManager.list_templates` | code → pack: absent from section 2 |
| `template_generator.TemplateGenerator` | code → pack: absent from section 2 |
| `template_generator.TemplateGenerator.merge_yaml_files` | code → pack: absent from section 2 |
| `template_generator.TemplateGenerator.generate` | code → pack: absent from section 2 |
| `template_generator.ModelValidator` | code → pack: absent from section 2 |
| `template_generator.ModelValidator.validate_syntax` | code → pack: absent from section 2 |
| `template_generator.ModelValidator.validate_structure` | code → pack: absent from section 2 |
| `template_generator.ModelValidator.validate_with_orcaflex` | code → pack: absent from section 2 |
| `template_generator.ModelValidator.validate` | code → pack: absent from section 2 |
| `template_generator.format_template_list` | code → pack: absent from section 2 |
| `template_generator.cmd_list_templates` | code → pack: absent from section 2 |
| `template_generator.cmd_generate` | code → pack: absent from section 2 |
| `template_generator.cmd_validate` | code → pack: absent from section 2 |
| `template_generator.create_parser` | code → pack: absent from section 2 |
| `template_generator.main` | code → pack: absent from section 2 |
| `time_trace_processor.TimeTraceConfig` | code → pack: absent from section 2 |
| `time_trace_processor.OrcaFlexTimeTraceProcessor` | code → pack: absent from section 2 |
| `time_trace_processor.OrcaFlexTimeTraceProcessor.process` | code → pack: absent from section 2 |
| `time_trace_processor.create_default_config` | code → pack: absent from section 2 |
| `umbilical_analysis_components.UmbilicalAnalysis` | code → pack: absent from section 2 |
| `umbilical_analysis_components.UmbilicalAnalysis.perform_analysis` | code → pack: absent from section 2 |
| `umbilical_analysis_components.UmbilicalAnalysis.first_end_analysis` | code → pack: absent from section 2 |
| `umbilical_analysis_components.UmbilicalAnalysis.second_end_analysis` | code → pack: absent from section 2 |
| `umbilical_analysis_components.UmbilicalAnalysis.installation_phases` | code → pack: absent from section 2 |
| `umbilical_installation_IV_to_host.InstallationVtoHost` | code → pack: absent from section 2 |
| `umbilical_installation_IV_to_host.InstallationVtoHost.installation_phase` | code → pack: absent from section 2 |
| `umbilical_installation_IV_to_host.InstallationVtoHost.installation_step` | code → pack: absent from section 2 |
| `umbilical_installation_IV_to_host.InstallationVtoHost.get_target_result` | code → pack: absent from section 2 |
| `umbilical_installation_IV_to_host.InstallationVtoHost.get_target_line_length` | code → pack: absent from section 2 |
| `umbilical_installation_IV_to_host.InstallationVtoHost.get_target_reference_distance` | code → pack: absent from section 2 |
| `umbilical_installation_lower_2nd_end.Lower2ndEnd` | code → pack: absent from section 2 |
| `umbilical_installation_lower_2nd_end.Lower2ndEnd.installation_phase` | code → pack: absent from section 2 |
| `umbilical_installation_lower_2nd_end.Lower2ndEnd.installation_step` | code → pack: absent from section 2 |
| `umbilical_installation_lower_2nd_end.Lower2ndEnd.get_target_result` | code → pack: absent from section 2 |
| `umbilical_installation_lower_2nd_end.Lower2ndEnd.get_target_line_length` | code → pack: absent from section 2 |
| `umbilical_installation_lower_2nd_end.Lower2ndEnd.get_target_reference_distance` | code → pack: absent from section 2 |
| `universal_cli.main` | code → pack: absent from section 2 |
| `universal_cli.create_config` | code → pack: absent from section 2 |
| `universal_cli.cli` | code → pack: absent from section 2 |
| `verify_optimization_deployment.verify_deployment` | code → pack: absent from section 2 |
| `visualization.MooringVisualization` | code → pack: absent from section 2 |
| `visualization.MooringVisualization.plot_mooring_forces` | code → pack: absent from section 2 |
| `visualization.MooringVisualization.plot_stiffness_characteristics` | code → pack: absent from section 2 |
| `yaml_timestep_validation.inspect_source` | code → pack: absent from section 2 |
| `yaml_utils.OrcaFlexLoader` | code → pack: absent from section 2 |
| `yaml_utils.OrcaFlexDumper` | code → pack: absent from section 2 |
| `yaml_utils.orcaflex_dump` | code → pack: absent from section 2 |
| `yaml_utils.orcaflex_load` | code → pack: absent from section 2 |
| `yaml_validator.Severity` | code → pack: absent from section 2 |
| `yaml_validator.ValidationIssue` | code → pack: absent from section 2 |
| `yaml_validator.ValidationResult` | code → pack: absent from section 2 |
| `yaml_validator.ValidationResult.errors` | code → pack: absent from section 2 |
| `yaml_validator.ValidationResult.warnings` | code → pack: absent from section 2 |
| `yaml_validator.ValidationResult.add` | code → pack: absent from section 2 |
| `yaml_validator.OrcaFlexYAMLValidator` | code → pack: absent from section 2 |
| `yaml_validator.OrcaFlexYAMLValidator.validate_file` | code → pack: absent from section 2 |
| `yaml_validator.OrcaFlexYAMLValidator.validate_directory` | code → pack: absent from section 2 |
| `yaml_validator.validate_orcaflex_yaml` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L434-L508` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L81` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L279-L342` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L396-L428` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L270-L300` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 63. `src/digitalmodel/solvers/orcaflex/format_converter/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 36.

| Function | Direction |
| --- | --- |
| `cli.cmd_single2modular` | code → pack: absent from section 2 |
| `cli.cmd_modular2single` | code → pack: absent from section 2 |
| `cli.cmd_spec2modular` | code → pack: absent from section 2 |
| `cli.cmd_spec2single` | code → pack: absent from section 2 |
| `cli.cmd_modular2spec` | code → pack: absent from section 2 |
| `cli.cmd_single2spec` | code → pack: absent from section 2 |
| `cli.cmd_detect` | code → pack: absent from section 2 |
| `cli.cmd_auto` | code → pack: absent from section 2 |
| `cli.create_parser` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `format_detector.FormatType` | code → pack: absent from section 2 |
| `format_detector.detect_format` | code → pack: absent from section 2 |
| `modular_to_single.ModularToSingleConverter` | code → pack: absent from section 2 |
| `modular_to_single.ModularToSingleConverter.convert` | code → pack: absent from section 2 |
| `modular_to_single.ModularToSingleConverter.supported_formats` | code → pack: absent from section 2 |
| `modular_to_single.ModularToSingleConverter.merge_to_dict` | code → pack: absent from section 2 |
| `modular_to_spec.ModularToSpecConverter` | code → pack: absent from section 2 |
| `modular_to_spec.ModularToSpecConverter.convert` | code → pack: absent from section 2 |
| `modular_to_spec.ModularToSpecConverter.supported_formats` | code → pack: absent from section 2 |
| `protocols.ConversionReport.summary` | code → pack: absent from section 2 |
| `protocols.FormatConverter` | code → pack: absent from section 2 |
| `protocols.FormatConverter.convert` | code → pack: absent from section 2 |
| `protocols.FormatConverter.supported_formats` | code → pack: absent from section 2 |
| `single_to_modular.SingleToModularConverter.supported_formats` | code → pack: absent from section 2 |
| `single_to_spec.SingleToSpecConverter` | code → pack: absent from section 2 |
| `single_to_spec.SingleToSpecConverter.convert` | code → pack: absent from section 2 |
| `single_to_spec.SingleToSpecConverter.supported_formats` | code → pack: absent from section 2 |
| `spec_to_modular.SpecToModularConverter` | code → pack: absent from section 2 |
| `spec_to_modular.SpecToModularConverter.convert` | code → pack: absent from section 2 |
| `spec_to_modular.SpecToModularConverter.supported_formats` | code → pack: absent from section 2 |
| `spec_to_single.SpecToSingleConverter` | code → pack: absent from section 2 |
| `spec_to_single.SpecToSingleConverter.convert` | code → pack: absent from section 2 |
| `spec_to_single.SpecToSingleConverter.supported_formats` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 64. `src/digitalmodel/solvers/orcaflex/model_generator/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 9.

| Function | Direction |
| --- | --- |
| `__init__.ComponentNotFoundError` | code → pack: absent from section 2 |
| `__init__.ValidationError` | code → pack: absent from section 2 |
| `__init__.OrcaFlexModelGenerator` | code → pack: absent from section 2 |
| `__init__.OrcaFlexModelGenerator.list_components` | code → pack: absent from section 2 |
| `__init__.OrcaFlexModelGenerator.get_component` | code → pack: absent from section 2 |
| `__init__.OrcaFlexModelGenerator.generate_from_template` | code → pack: absent from section 2 |
| `__init__.OrcaFlexModelGenerator.validate` | code → pack: absent from section 2 |
| `__init__.OrcaFlexModelGenerator.add_component` | code → pack: absent from section 2 |
| `__init__.generate_model` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 65. `src/digitalmodel/solvers/orcaflex/modular_generator/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 47.

| Function | Direction |
| --- | --- |
| `__init__.ModelValidationError` | code → pack: absent from section 2 |
| `__init__.GenerationResult` | code → pack: absent from section 2 |
| `__init__.ModularModelGenerator` | code → pack: absent from section 2 |
| `__init__.ModularModelGenerator.from_spec` | code → pack: absent from section 2 |
| `__init__.ModularModelGenerator.generate` | code → pack: absent from section 2 |
| `__init__.ModularModelGenerator.generate_with_overrides` | code → pack: absent from section 2 |
| `cli.print_error` | code → pack: absent from section 2 |
| `cli.print_success` | code → pack: absent from section 2 |
| `cli.print_progress` | code → pack: absent from section 2 |
| `cli.validate_spec` | code → pack: absent from section 2 |
| `cli.cmd_validate` | code → pack: absent from section 2 |
| `cli.cmd_generate` | code → pack: absent from section 2 |
| `cli.cmd_campaign` | code → pack: absent from section 2 |
| `cli.create_parser` | code → pack: absent from section 2 |
| `extractor.MonolithicExtractor` | code → pack: absent from section 2 |
| `extractor.MonolithicExtractor.extract` | code → pack: absent from section 2 |
| `extractor.MonolithicExtractor.raw` | code → pack: absent from section 2 |
| `extractor.MonolithicExtractor.get_section_keys` | code → pack: absent from section 2 |
| `extractor.MonolithicExtractor.get_object_names` | code → pack: absent from section 2 |
| `merge.merge_named_lists` | code → pack: absent from section 2 |
| `post_validator.ValidationWarning` | code → pack: absent from section 2 |
| `post_validator.CollectionCollision` | code → pack: absent from section 2 |
| `post_validator.CollectionCollision.describe` | code → pack: absent from section 2 |
| `post_validator.CollectionCollisionError` | code → pack: absent from section 2 |
| `post_validator.CollectionScanResult` | code → pack: absent from section 2 |
| `post_validator.PostGenerationValidator` | code → pack: absent from section 2 |
| `post_validator.PostGenerationValidator.validate_directory` | code → pack: absent from section 2 |
| `post_validator.PostGenerationValidator.validate_data` | code → pack: absent from section 2 |
| `post_validator.PostGenerationValidator.check_collection_collisions` | code → pack: absent from section 2 |
| `post_validator.check_collection_collisions` | code → pack: absent from section 2 |
| `sections.VariableResolver` | code → pack: absent from section 2 |
| `sections.VariableResolver.resolve` | code → pack: absent from section 2 |
| `slay_execution.read_contract` | code → pack: absent from section 2 |
| `slay_execution.write_queue` | code → pack: absent from section 2 |
| `slay_probe.stinger_layout_for_depth` | code → pack: absent from section 2 |
| `slay_probe.end_a_support_reaction` | code → pack: absent from section 2 |
| `slay_probe.screen_case` | code → pack: absent from section 2 |
| `slay_probe.evaluate_static_acceptance` | code → pack: absent from section 2 |
| `slay_probe.assert_static_acceptance` | code → pack: absent from section 2 |
| `slay_readback.support_readback` | code → pack: absent from section 2 |
| `slay_readback.material_node_indices` | code → pack: absent from section 2 |
| `slay_readback.aligned_static_graphs` | code → pack: absent from section 2 |
| `slay_readback.sag_bend_indices` | code → pack: absent from section 2 |
| `slay_readback.static_readback` | code → pack: absent from section 2 |
| `slay_readback.last_contact` | code → pack: absent from section 2 |
| `slay_readback.calculate_statics_recorded` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/marine_ops/marine_analysis/aqwa_enhanced_parser.py:L757-L803` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L396-L428` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L49-L63` | pack → code: file missing or range invalid |
| `tests/hydrodynamics/aqwa/test_viscous_damping_determination.py:L99-L129` | pack → code: file missing or range invalid |

### 66. `src/digitalmodel/solvers/orcaflex/modular_input_validation/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 65.

| Function | Direction |
| --- | --- |
| `cli.create_parser` | code → pack: absent from section 2 |
| `cli.resolve_paths` | code → pack: absent from section 2 |
| `cli.create_config_from_args` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `config.ValidationConfig.get_reference_data_paths` | code → pack: absent from section 2 |
| `config.ValidationConfig.ensure_output_directories` | code → pack: absent from section 2 |
| `config.ValidationConfig.from_dict` | code → pack: absent from section 2 |
| `data_loader.ParameterRange` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyReferenceData` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.load_all` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.load_hull_geometry_ranges` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.load_metocean_ranges` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.load_mooring_capacity_ranges` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.load_environmental_conditions` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.load_mooring_line_properties` | code → pack: absent from section 2 |
| `data_loader.CALMBuoyDataLoader.get_parameter_range` | code → pack: absent from section 2 |
| `level_1_yaml.Level1YAMLValidator.validate_module` | code → pack: absent from section 2 |
| `level_1_yaml.Level1YAMLValidator.build_dependency_graph` | code → pack: absent from section 2 |
| `level_1_yaml.Level1YAMLValidator.detect_circular_dependencies` | code → pack: absent from section 2 |
| `level_2_orcaflex.Level2OrcaFlexValidator` | code → pack: absent from section 2 |
| `level_2_orcaflex.Level2OrcaFlexValidator.validate` | code → pack: absent from section 2 |
| `level_2_orcaflex.Level2OrcaFlexValidator.get_software_info` | code → pack: absent from section 2 |
| `level_3_physical.PhysicalConsistencyCheck` | code → pack: absent from section 2 |
| `level_3_physical.Level3PhysicalValidator` | code → pack: absent from section 2 |
| `level_3_physical.Level3PhysicalValidator.validate` | code → pack: absent from section 2 |
| `models.ValidationLevel` | code → pack: absent from section 2 |
| `models.ValidationStatus` | code → pack: absent from section 2 |
| `models.Severity` | code → pack: absent from section 2 |
| `models.OrcaFlexAvailability` | code → pack: absent from section 2 |
| `models.ValidationIssue` | code → pack: absent from section 2 |
| `models.ValidationIssue.to_dict` | code → pack: absent from section 2 |
| `models.Level1Result` | code → pack: absent from section 2 |
| `models.Level2Result` | code → pack: absent from section 2 |
| `models.PhysicalConsistencyCheck` | code → pack: absent from section 2 |
| `models.Level3Result` | code → pack: absent from section 2 |
| `models.ValidationResult.add_issue` | code → pack: absent from section 2 |
| `models.ValidationResult.get_issues_by_severity` | code → pack: absent from section 2 |
| `models.ValidationResult.get_issues_by_level` | code → pack: absent from section 2 |
| `models.ValidationResult.has_critical_issues` | code → pack: absent from section 2 |
| `models.ValidationResult.has_warnings` | code → pack: absent from section 2 |
| `utils.calculate_percentage_difference` | code → pack: absent from section 2 |
| `utils.is_within_tolerance` | code → pack: absent from section 2 |
| `utils.is_within_range` | code → pack: absent from section 2 |
| `utils.extract_parameter_from_yaml` | code → pack: absent from section 2 |
| `utils.format_file_size` | code → pack: absent from section 2 |
| `utils.sanitize_filename` | code → pack: absent from section 2 |
| `utils.merge_dicts_deep` | code → pack: absent from section 2 |
| `validator.ModularInputValidator` | code → pack: absent from section 2 |
| `validator.ModularInputValidator.validate_file` | code → pack: absent from section 2 |
| `validator.ModularInputValidator.validate_files` | code → pack: absent from section 2 |
| `validator.ModularInputValidator.validate_directory` | code → pack: absent from section 2 |
| `validator.ModularInputValidator.generate_reports` | code → pack: absent from section 2 |
| `validator.ModularInputValidator.run_validation` | code → pack: absent from section 2 |
| `validator.ModularInputValidator.get_validation_summary` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 67. `src/digitalmodel/solvers/orcaflex/mooring_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 10.

| Function | Direction |
| --- | --- |
| `natural_periods.NaturalPeriods.to_dict` | code → pack: absent from section 2 |
| `natural_periods.NaturalPeriods.validate_ranges` | code → pack: absent from section 2 |
| `natural_periods.NaturalPeriodAnalyzer` | code → pack: absent from section 2 |
| `natural_periods.NaturalPeriodAnalyzer.set_vessel_properties` | code → pack: absent from section 2 |
| `natural_periods.NaturalPeriodAnalyzer.set_stiffness_matrix` | code → pack: absent from section 2 |
| `natural_periods.NaturalPeriodAnalyzer.calculate_natural_periods` | code → pack: absent from section 2 |
| `natural_periods.NaturalPeriodAnalyzer.calculate_frequency_response` | code → pack: absent from section 2 |
| `natural_periods.calculate_natural_periods_from_stiffness` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 68. `src/digitalmodel/solvers/orcaflex/results_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 101.

| Function | Direction |
| --- | --- |
| `anomaly_detection.AnomalyType` | code → pack: absent from section 2 |
| `anomaly_detection.DetectionMethod` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyResult` | code → pack: absent from section 2 |
| `anomaly_detection.EngineeringLimits` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyDetectionService` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyDetectionService.detect_statistical_outliers` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyDetectionService.detect_engineering_limit_violations` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyDetectionService.detect_multivariate_anomalies` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyDetectionService.detect_time_series_anomalies` | code → pack: absent from section 2 |
| `anomaly_detection.AnomalyDetectionService.comprehensive_anomaly_scan` | code → pack: absent from section 2 |
| `comparative_analysis.ComparisonType` | code → pack: absent from section 2 |
| `comparative_analysis.TestType` | code → pack: absent from section 2 |
| `comparative_analysis.ComparisonResult` | code → pack: absent from section 2 |
| `comparative_analysis.GroupComparisonResult` | code → pack: absent from section 2 |
| `comparative_analysis.ComparativeAnalysisService` | code → pack: absent from section 2 |
| `comparative_analysis.ComparativeAnalysisService.compare_loading_conditions` | code → pack: absent from section 2 |
| `comparative_analysis.ComparativeAnalysisService.compare_environmental_conditions` | code → pack: absent from section 2 |
| `comparative_analysis.ComparativeAnalysisService.benchmark_performance` | code → pack: absent from section 2 |
| `component_classifier.ComponentType` | code → pack: absent from section 2 |
| `component_classifier.ParameterType` | code → pack: absent from section 2 |
| `component_classifier.ClassificationResult` | code → pack: absent from section 2 |
| `component_classifier.ComponentClassifier` | code → pack: absent from section 2 |
| `component_classifier.ComponentClassifier.classify_column` | code → pack: absent from section 2 |
| `component_classifier.ComponentClassifier.classify_file` | code → pack: absent from section 2 |
| `component_classifier.ComponentClassifier.get_component_summary` | code → pack: absent from section 2 |
| `component_classifier.ComponentClassifier.validate_classification` | code → pack: absent from section 2 |
| `component_classifier.quick_classify` | code → pack: absent from section 2 |
| `component_classifier.get_component_hierarchy` | code → pack: absent from section 2 |
| `csv_parser.FileType` | code → pack: absent from section 2 |
| `csv_parser.CSVParser.identify_file_type` | code → pack: absent from section 2 |
| `csv_parser.CSVParser.parse_column_metadata` | code → pack: absent from section 2 |
| `csv_parser.CSVParser.extract_polar_data` | code → pack: absent from section 2 |
| `csv_parser.CSVParser.validate_polar_data` | code → pack: absent from section 2 |
| `csv_parser.quick_parse` | code → pack: absent from section 2 |
| `csv_parser.extract_time_series_stats` | code → pack: absent from section 2 |
| `data_validator.ValidationLevel` | code → pack: absent from section 2 |
| `data_validator.ValidationCategory` | code → pack: absent from section 2 |
| `data_validator.Severity` | code → pack: absent from section 2 |
| `data_validator.ValidationIssue` | code → pack: absent from section 2 |
| `data_validator.DataValidator.get_validation_summary` | code → pack: absent from section 2 |
| `data_validator.DataValidator.get_validator_stats` | code → pack: absent from section 2 |
| `data_validator.quick_validate` | code → pack: absent from section 2 |
| `data_validator.validate_file_data` | code → pack: absent from section 2 |
| `file_monitor.EventType` | code → pack: absent from section 2 |
| `file_monitor.FileStatus` | code → pack: absent from section 2 |
| `file_monitor.FileEvent` | code → pack: absent from section 2 |
| `file_monitor.MonitoredFile` | code → pack: absent from section 2 |
| `file_monitor.FileMonitorConfig` | code → pack: absent from section 2 |
| `file_monitor.OrcaFlexFileHandler` | code → pack: absent from section 2 |
| `file_monitor.OrcaFlexFileHandler.on_created` | code → pack: absent from section 2 |
| `file_monitor.OrcaFlexFileHandler.on_modified` | code → pack: absent from section 2 |
| `file_monitor.OrcaFlexFileHandler.on_deleted` | code → pack: absent from section 2 |
| `file_monitor.FileMonitor` | code → pack: absent from section 2 |
| `file_monitor.FileMonitor.start` | code → pack: absent from section 2 |
| `file_monitor.FileMonitor.stop` | code → pack: absent from section 2 |
| `file_monitor.FileMonitor.get_monitoring_stats` | code → pack: absent from section 2 |
| `file_monitor.FileMonitor.get_monitored_files` | code → pack: absent from section 2 |
| `file_monitor.FileMonitor.force_rescan` | code → pack: absent from section 2 |
| `file_monitor.create_orcaflex_monitor` | code → pack: absent from section 2 |
| `loading_decoder.WaterLevel` | code → pack: absent from section 2 |
| `loading_decoder.VolumeCondition` | code → pack: absent from section 2 |
| `loading_decoder.SideConfiguration` | code → pack: absent from section 2 |
| `loading_decoder.LoadingPhase` | code → pack: absent from section 2 |
| `loading_decoder.EnvironmentalConditions` | code → pack: absent from section 2 |
| `loading_decoder.LoadingConditions` | code → pack: absent from section 2 |
| `loading_decoder.LoadingDecoder.decode_from_filename` | code → pack: absent from section 2 |
| `loading_decoder.LoadingDecoder.decode_from_data` | code → pack: absent from section 2 |
| `loading_decoder.LoadingDecoder.decode_loading_conditions` | code → pack: absent from section 2 |
| `loading_decoder.LoadingDecoder.validate_conditions` | code → pack: absent from section 2 |
| `loading_decoder.LoadingDecoder.get_condition_summary` | code → pack: absent from section 2 |
| `loading_decoder.quick_decode` | code → pack: absent from section 2 |
| `loading_decoder.batch_decode` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityMethod` | code → pack: absent from section 2 |
| `sensitivity_analysis.ParameterRange` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityResult` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityAnalysisService` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityAnalysisService.perform_parameter_sensitivity_analysis` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityAnalysisService.perform_environmental_load_case_analysis` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityAnalysisService.generate_response_surface` | code → pack: absent from section 2 |
| `sensitivity_analysis.SensitivityAnalysisService.perform_monte_carlo_sensitivity` | code → pack: absent from section 2 |
| `statistical_analysis.AnalysisType` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalResult` | code → pack: absent from section 2 |
| `statistical_analysis.RegressionResult` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService.calculate_correlation_matrix` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService.perform_linear_regression` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService.calculate_confidence_interval` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService.perform_trend_analysis` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService.analyze_distribution` | code → pack: absent from section 2 |
| `statistical_analysis.StatisticalAnalysisService.generate_statistical_report` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407:L1-L4` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407:L28464-L28471` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@b5acbe5f61ed4c755c1f7ea4fafa4611bd14e407:L50-L23673` | pack → code: file missing or range invalid |
| `tests/solvers/orcaflex/results_analysis/test_csv_parser.py:L1-L914` | pack → code: file missing or range invalid |

### 69. `src/digitalmodel/solvers/orcaflex/universal/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 34.

| Function | Direction |
| --- | --- |
| `__main__.parse_kwargs` | code → pack: absent from section 2 |
| `__main__.main` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor.process_batch` | code → pack: absent from section 2 |
| `batch_processor.BatchProcessor.get_performance_stats` | code → pack: absent from section 2 |
| `model_discovery.ModelDiscovery` | code → pack: absent from section 2 |
| `model_discovery.ModelDiscovery.find_models` | code → pack: absent from section 2 |
| `model_discovery.ModelDiscovery.validate_model` | code → pack: absent from section 2 |
| `model_discovery.ModelDiscovery.find_related_models` | code → pack: absent from section 2 |
| `model_discovery.ModelDiscovery.group_models_by_pattern` | code → pack: absent from section 2 |
| `model_discovery.ModelDiscovery.get_discovery_stats` | code → pack: absent from section 2 |
| `path_resolver.PathResolver` | code → pack: absent from section 2 |
| `path_resolver.PathResolver.resolve_path` | code → pack: absent from section 2 |
| `path_resolver.PathResolver.resolve_model_path` | code → pack: absent from section 2 |
| `path_resolver.PathResolver.validate_path` | code → pack: absent from section 2 |
| `path_resolver.PathResolver.ensure_directory` | code → pack: absent from section 2 |
| `path_resolver.PathResolver.get_relative_path` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.update_terminal_title` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.display_progress` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.log_result` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.display_summary` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.save_report` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.reset` | code → pack: absent from section 2 |
| `status_reporter.StatusReporter.set_final_status` | code → pack: absent from section 2 |
| `universal_runner.RunResults.success_rate` | code → pack: absent from section 2 |
| `universal_runner.UniversalOrcaFlexRunner` | code → pack: absent from section 2 |
| `universal_runner.UniversalOrcaFlexRunner.path_resolver` | code → pack: absent from section 2 |
| `universal_runner.UniversalOrcaFlexRunner.model_discovery` | code → pack: absent from section 2 |
| `universal_runner.UniversalOrcaFlexRunner.batch_processor` | code → pack: absent from section 2 |
| `universal_runner.UniversalOrcaFlexRunner.status_reporter` | code → pack: absent from section 2 |
| `universal_runner.UniversalOrcaFlexRunner.run` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 70. `src/digitalmodel/solvers/orcawave/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 0.

| Function | Direction |
| --- | --- |
| None detected by lexical comparison | No code → pack finding |

| Locator | Direction |
| --- | --- |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L1-L4` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10005-L10017` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10018-L10032` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10033-L10046` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10047-L10060` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10061-L10072` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10073-L10087` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10088-L10099` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10100-L10111` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10112-L10122` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10123-L10134` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10135-L10146` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10147-L10158` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10159-L10169` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10170-L10181` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10182-L10193` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10194-L10207` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10208-L10222` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10223-L10235` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10236-L10246` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10247-L10260` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10261-L10272` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10273-L10284` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10285-L10296` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10297-L10307` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10308-L10319` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10320-L10333` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10334-L10345` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10346-L10356` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10357-L10368` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10369-L10380` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10381-L10391` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10392-L10403` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10404-L10415` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10416-L10427` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10428-L10439` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10440-L10451` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10452-L10465` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10466-L10477` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10478-L10489` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10490-L10501` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10502-L10513` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10514-L10527` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10528-L10539` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10540-L10551` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10552-L10563` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10564-L10575` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10576-L10587` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10588-L10599` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10600-L10611` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10612-L10623` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10624-L10635` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10636-L10647` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10648-L10659` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10660-L10671` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10672-L10683` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10684-L10699` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10700-L10711` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10712-L10725` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10726-L10737` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10738-L10749` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10750-L10761` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10762-L10775` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10776-L10789` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10790-L10801` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10802-L10815` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10816-L10828` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10829-L10841` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10842-L10853` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10854-L10865` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10866-L10877` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10878-L10891` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10892-L10905` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10906-L10917` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10918-L10933` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10934-L10948` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10949-L10962` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10963-L10979` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10980-L10991` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L10992-L11003` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11004-L11015` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11016-L11027` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11028-L11039` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11040-L11051` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11052-L11063` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11064-L11075` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11076-L11089` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11090-L11101` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11102-L11113` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11114-L11125` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11126-L11137` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11138-L11149` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11150-L11163` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11164-L11175` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11176-L11187` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11188-L11202` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11203-L11213` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11214-L11225` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11226-L11237` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11238-L11248` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11249-L11260` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11261-L11274` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11275-L11286` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11287-L11298` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11299-L11310` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11311-L11322` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11323-L11334` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11335-L11346` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11347-L11360` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11361-L11374` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11375-L11388` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11389-L11400` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11401-L11414` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11415-L11426` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11427-L11438` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11439-L11450` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11451-L11462` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11463-L11474` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11475-L11486` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11487-L11498` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11499-L11510` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11511-L11524` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11525-L11536` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11537-L11548` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11549-L11560` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11561-L11574` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11575-L11590` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11591-L11603` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11604-L11619` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11620-L11633` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11634-L11648` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11649-L11662` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11663-L11674` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11675-L11686` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11687-L11698` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11699-L11710` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11711-L11724` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11725-L11736` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11737-L11748` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11749-L11762` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11763-L11774` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11775-L11786` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11787-L11798` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11799-L11812` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11813-L11824` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11825-L11836` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11837-L11850` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11851-L11862` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11863-L11874` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11875-L11888` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11889-L11900` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11901-L11912` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11913-L11926` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11927-L11938` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11939-L11950` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11951-L11964` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11965-L11976` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11977-L11990` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L11991-L12002` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12003-L12014` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12015-L12026` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12027-L12038` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12039-L12052` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12053-L12064` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12065-L12078` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12079-L12095` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12096-L12107` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12108-L12119` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12120-L12131` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12132-L12143` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12144-L12159` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12160-L12173` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12174-L12186` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12187-L12199` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12200-L12211` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12212-L12226` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12227-L12238` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12239-L12254` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12255-L12266` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12267-L12278` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12279-L12290` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12291-L12302` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12303-L12314` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12315-L12326` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12327-L12338` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12339-L12350` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12351-L12362` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12363-L12376` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12377-L12388` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12389-L12400` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12401-L12412` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12413-L12427` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12428-L12441` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12442-L12453` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12454-L12465` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12466-L12477` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12478-L12489` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12490-L12503` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12504-L12515` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12516-L12529` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12530-L12541` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12542-L12555` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12556-L12567` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12568-L12582` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12583-L12594` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12595-L12606` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12607-L12620` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12621-L12632` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12633-L12646` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12647-L12658` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12659-L12670` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12671-L12682` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12683-L12696` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12697-L12710` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12711-L12721` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12722-L12733` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12734-L12745` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12746-L12759` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12760-L12773` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12774-L12787` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12788-L12799` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12800-L12813` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12814-L12827` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12828-L12839` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12840-L12850` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12851-L12862` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12863-L12874` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12875-L12886` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12887-L12898` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12899-L12910` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12911-L12922` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12923-L12934` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12935-L12946` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12947-L12958` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12959-L12973` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12974-L12986` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12987-L12998` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L12999-L13010` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13011-L13022` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13023-L13034` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13035-L13048` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13049-L13062` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13063-L13074` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13075-L13086` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13087-L13101` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13102-L13113` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13114-L13125` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13126-L13139` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13140-L13153` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13154-L13167` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13168-L13181` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13182-L13193` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13194-L13204` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13205-L13216` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13217-L13228` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L13229-L13240` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24511-L24520` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24521-L24530` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24531-L24540` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24541-L24550` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24551-L24560` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24561-L24570` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24571-L24580` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24581-L24590` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24591-L24600` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24601-L24610` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24611-L24620` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24621-L24630` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24631-L24640` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24641-L24650` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24651-L24660` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24661-L24670` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24671-L24680` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24681-L24690` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24691-L24700` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24701-L24712` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24713-L24722` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24723-L24732` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24733-L24742` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24743-L24751` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24752-L24760` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24761-L24769` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24770-L24778` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24779-L24787` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24788-L24796` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24797-L24805` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24806-L24814` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24815-L24823` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24824-L24832` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24833-L24841` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24842-L24850` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24851-L24859` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24860-L24868` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24869-L24877` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24878-L24886` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24887-L24895` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24896-L24904` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24905-L24913` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24914-L24922` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24923-L24932` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24933-L24942` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24943-L24952` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24953-L24962` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24963-L24974` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24975-L24984` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24985-L24997` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L24998-L25007` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25008-L25017` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25018-L25027` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25028-L25037` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25038-L25047` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25048-L25057` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25058-L25067` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25068-L25079` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25080-L25089` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25090-L25099` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25100-L25109` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25110-L25119` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25120-L25129` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25130-L25139` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25140-L25149` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25150-L25159` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25160-L25169` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25170-L25179` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25180-L25189` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25190-L25199` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25200-L25209` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25210-L25219` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25220-L25229` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25230-L25239` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25240-L25249` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25250-L25259` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25260-L25269` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25270-L25279` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25280-L25289` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25290-L25299` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25300-L25309` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25310-L25319` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25320-L25329` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25330-L25339` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25340-L25349` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25350-L25359` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25360-L25369` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25370-L25379` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25380-L25389` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25390-L25399` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25400-L25409` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25410-L25419` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25420-L25429` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25430-L25439` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25440-L25455` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25456-L25465` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25466-L25475` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25476-L25485` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25486-L25495` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25496-L25505` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25506-L25518` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25519-L25528` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25529-L25538` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25539-L25548` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25549-L25558` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25559-L25568` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25569-L25578` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25579-L25588` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25589-L25598` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25599-L25608` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25609-L25618` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25619-L25628` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25629-L25638` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25639-L25648` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25649-L25658` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25659-L25668` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25669-L25678` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25679-L25688` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25689-L25698` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25699-L25708` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25709-L25718` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25719-L25728` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25729-L25738` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25739-L25748` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25749-L25758` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25759-L25770` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25771-L25780` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25781-L25790` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25791-L25800` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25801-L25810` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25811-L25820` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25821-L25830` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25831-L25840` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25841-L25850` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25851-L25860` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25861-L25870` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L25871-L25880` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4935-L4942` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4940-L4949` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4950-L4958` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4959-L4968` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4969-L4978` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4979-L4988` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4989-L4998` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L4999-L5008` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5009-L5020` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5021-L5030` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5031-L5040` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5041-L5050` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5051-L5060` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5061-L5070` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5071-L5080` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5081-L5090` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5091-L5100` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5101-L5110` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5111-L5120` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5121-L5132` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5133-L5142` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5143-L5151` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5152-L5161` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5162-L5171` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5172-L5181` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5182-L5191` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5192-L5201` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5202-L5211` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5212-L5223` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5224-L5233` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5234-L5243` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5244-L5253` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5254-L5263` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5264-L5273` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5274-L5283` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5284-L5293` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5294-L5303` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5304-L5313` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5314-L5323` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5324-L5333` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5334-L5343` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5344-L5353` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5354-L5363` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5364-L5372` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5373-L5382` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5383-L5392` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5393-L5402` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5403-L5415` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5416-L5425` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5426-L5437` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5438-L5447` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5448-L5457` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5458-L5467` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5468-L5477` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5478-L5487` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5488-L5497` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5498-L5507` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5508-L5519` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5520-L5531` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5532-L5540` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5541-L5550` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5551-L5560` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5561-L5570` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5571-L5580` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5581-L5593` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5594-L5603` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5604-L5615` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5616-L5628` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5629-L5638` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5639-L5648` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5649-L5658` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5659-L5667` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5668-L5677` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5678-L5687` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5688-L5697` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5698-L5707` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5708-L5717` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5718-L5727` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5728-L5737` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5738-L5749` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5750-L5759` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5760-L5769` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5770-L5781` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5782-L5791` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5792-L5801` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5802-L5811` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5812-L5823` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5824-L5833` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5834-L5843` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5844-L5853` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5854-L5863` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5864-L5872` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5873-L5882` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5883-L5891` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5892-L5901` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5902-L5911` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5912-L5921` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5922-L5933` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5934-L5943` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5944-L5953` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5954-L5965` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5966-L5975` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5976-L5985` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5986-L5994` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L5995-L6004` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6005-L6014` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6015-L6024` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6025-L6034` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6035-L6044` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6045-L6054` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6055-L6064` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6065-L6074` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6075-L6084` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6085-L6096` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6097-L6109` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6110-L6119` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6120-L6129` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6130-L6141` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6142-L6153` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6154-L6163` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6164-L6173` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6174-L6183` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6184-L6193` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6194-L6203` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6204-L6213` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6214-L6223` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6224-L6233` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6234-L6243` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6244-L6253` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6254-L6263` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6264-L6273` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6274-L6283` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6284-L6293` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6294-L6303` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6304-L6313` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6314-L6323` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6324-L6333` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6334-L6343` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6344-L6353` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6354-L6363` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6364-L6373` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6374-L6383` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6384-L6393` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6394-L6403` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6404-L6413` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6414-L6423` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6424-L6436` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6437-L6448` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6449-L6458` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6459-L6468` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6469-L6478` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6479-L6490` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6491-L6500` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6501-L6510` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6511-L6520` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6521-L6530` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6531-L6540` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6541-L6550` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6551-L6560` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6561-L6570` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6571-L6580` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6581-L6590` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6591-L6600` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6601-L6610` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6611-L6620` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6621-L6630` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6631-L6640` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6641-L6650` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6651-L6659` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6660-L6672` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6673-L6682` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6683-L6692` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6693-L6704` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6705-L6714` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6715-L6724` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6725-L6734` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6735-L6744` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6745-L6754` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6755-L6764` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6765-L6774` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6775-L6784` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6785-L6794` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6795-L6804` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6805-L6814` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6815-L6824` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6825-L6834` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6835-L6844` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6845-L6854` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6855-L6864` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6865-L6874` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6875-L6884` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6885-L6894` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6895-L6904` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6905-L6914` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6915-L6927` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6928-L6937` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6938-L6947` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6948-L6957` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6958-L6967` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6968-L6977` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6978-L6987` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6988-L6997` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L6998-L7007` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7008-L7017` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7018-L7031` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7032-L7041` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7042-L7051` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7052-L7061` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7062-L7071` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7072-L7081` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7082-L7091` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7092-L7101` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7102-L7111` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7112-L7121` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7122-L7131` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7132-L7141` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7142-L7153` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7154-L7163` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7164-L7176` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7177-L7186` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7187-L7196` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7197-L7207` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7208-L7217` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7218-L7227` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7228-L7237` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7238-L7247` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7248-L7257` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7258-L7269` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7270-L7279` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7280-L7289` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7290-L7299` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7300-L7309` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7310-L7319` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7320-L7329` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7330-L7339` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7340-L7349` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7350-L7359` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7360-L7369` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7370-L7379` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7380-L7389` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7390-L7399` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7400-L7409` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7410-L7419` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7420-L7429` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7430-L7439` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7440-L7449` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7450-L7459` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7460-L7469` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7470-L7479` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7480-L7489` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7490-L7499` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7500-L7509` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7510-L7519` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7520-L7529` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7530-L7539` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7540-L7551` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7552-L7561` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7562-L7571` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7572-L7581` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7582-L7591` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7592-L7603` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7604-L7613` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7614-L7626` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7627-L7636` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7637-L7646` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7647-L7656` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7657-L7666` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7667-L7676` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7677-L7686` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7687-L7696` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7697-L7706` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7707-L7716` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7717-L7729` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7730-L7739` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7740-L7749` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7750-L7762` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7763-L7774` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7775-L7784` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7785-L7794` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7795-L7804` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7805-L7816` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7817-L7826` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7827-L7836` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7837-L7846` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7847-L7856` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7857-L7866` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7867-L7878` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7879-L7888` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7889-L7898` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7899-L7908` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7909-L7921` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7922-L7931` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7932-L7941` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7942-L7951` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7952-L7961` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7962-L7971` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7972-L7981` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7982-L7991` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L7992-L8001` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8002-L8011` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8012-L8021` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8022-L8031` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8032-L8043` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8044-L8053` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8054-L8063` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8064-L8073` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8074-L8083` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8084-L8093` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8094-L8103` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8104-L8113` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8114-L8123` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8124-L8133` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8134-L8143` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8144-L8155` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8156-L8165` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8166-L8175` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8176-L8185` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8186-L8195` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8205` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8196-L8237` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8206-L8215` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8216-L8227` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8228-L8237` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8238-L8247` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8248-L8257` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8258-L8267` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8268-L8279` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8280-L8290` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8291-L8300` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8301-L8310` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8311-L8323` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8324-L8333` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8334-L8343` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8344-L8353` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8354-L8363` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8364-L8373` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8374-L8383` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8384-L8393` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8394-L8403` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8404-L8413` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8414-L8423` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8424-L8435` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8436-L8445` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8446-L8457` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8458-L8470` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8471-L8480` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8481-L8492` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8493-L8505` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8506-L8517` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8518-L8527` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8528-L8537` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8538-L8547` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8548-L8560` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8561-L8572` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8573-L8582` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8583-L8596` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8597-L8608` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8609-L8619` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8620-L8632` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8633-L8647` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8648-L8657` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8658-L8667` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8668-L8677` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8678-L8687` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8688-L8701` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8702-L8714` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8715-L8724` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8725-L8739` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8740-L8749` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8750-L8759` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8760-L8773` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8774-L8783` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8784-L8793` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8794-L8803` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8804-L8813` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8814-L8823` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8824-L8833` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8834-L8843` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8844-L8856` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8857-L8866` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8867-L8876` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8877-L8886` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8887-L8896` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8897-L8908` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8909-L8918` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8919-L8928` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8929-L8938` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8939-L8948` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8949-L8958` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8959-L8970` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8971-L8980` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8981-L8990` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L8991-L9000` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9001-L9010` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9011-L9020` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9021-L9030` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9031-L9040` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9041-L9050` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9051-L9060` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9061-L9070` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9071-L9080` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9081-L9090` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9091-L9100` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9101-L9110` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9111-L9120` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9121-L9130` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9131-L9140` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9141-L9152` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9153-L9162` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9163-L9172` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9173-L9182` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9183-L9192` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9193-L9202` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9203-L9212` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9213-L9222` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9223-L9232` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9233-L9242` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9243-L9252` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9253-L9262` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9263-L9272` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9273-L9282` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9283-L9292` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9293-L9302` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9303-L9312` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9313-L9322` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9323-L9332` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9333-L9342` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9343-L9352` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9353-L9362` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9363-L9372` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9373-L9382` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9383-L9392` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9393-L9402` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9403-L9412` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9413-L9422` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9423-L9432` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9433-L9442` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9443-L9452` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9453-L9462` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9463-L9472` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9473-L9482` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9483-L9493` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9516-L9527` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9528-L9539` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9540-L9551` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9552-L9563` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9564-L9578` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9579-L9589` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9590-L9601` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9602-L9613` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9614-L9625` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9626-L9637` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9638-L9652` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9653-L9664` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9665-L9678` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9679-L9690` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9691-L9704` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9705-L9715` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9716-L9727` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9728-L9739` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9740-L9753` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9754-L9765` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9766-L9777` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9778-L9788` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9789-L9800` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9801-L9812` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9813-L9826` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9827-L9838` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9839-L9849` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9850-L9861` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9862-L9873` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9874-L9885` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9886-L9897` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9898-L9909` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9910-L9921` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9922-L9932` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9933-L9943` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9944-L9954` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9955-L9965` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9966-L9977` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9978-L9989` | pack → code: file missing or range invalid |
| `docs/registry/implementation-map.yaml@3256bab95f84da3d9fadce8d5dcf8458d66d1eb4:L9990-L10004` | pack → code: file missing or range invalid |

### 71. `src/digitalmodel/structural/analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 76.

| Function | Direction |
| --- | --- |
| `cli.cli` | code → pack: absent from section 2 |
| `cli.sweep` | code → pack: absent from section 2 |
| `cli.phases` | code → pack: absent from section 2 |
| `cli.list_grades` | code → pack: absent from section 2 |
| `cli.lookup` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `wall_thickness.DesignCode` | code → pack: absent from section 2 |
| `wall_thickness.CodeEdition` | code → pack: absent from section 2 |
| `wall_thickness.CodeEdition.display_label` | code → pack: absent from section 2 |
| `wall_thickness.SafetyClass` | code → pack: absent from section 2 |
| `wall_thickness.FabricationType` | code → pack: absent from section 2 |
| `wall_thickness.PipeGeometry.inner_diameter` | code → pack: absent from section 2 |
| `wall_thickness.PipeGeometry.d_over_t` | code → pack: absent from section 2 |
| `wall_thickness.PipeMaterial.alpha_fab` | code → pack: absent from section 2 |
| `wall_thickness.DesignFactors.gamma_sc` | code → pack: absent from section 2 |
| `wall_thickness.DesignFactors.gamma_m` | code → pack: absent from section 2 |
| `wall_thickness.DesignFactors.gamma_inc` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_pressure_containment` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_collapse` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_propagation_buckling` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_combined_loading` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_api_burst` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_api_collapse` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.check_api_propagation` | code → pack: absent from section 2 |
| `wall_thickness.WallThicknessAnalyzer.perform_analysis` | code → pack: absent from section 2 |
| `wall_thickness_compare_workflow.run_wall_thickness_compare` | code → pack: absent from section 2 |
| `wall_thickness_compare_workflow.build_compare_note` | code → pack: absent from section 2 |
| `wall_thickness_comparison.CodeComparisonResult` | code → pack: absent from section 2 |
| `wall_thickness_comparison.compare_codes` | code → pack: absent from section 2 |
| `wall_thickness_comparison.generate_comparison_report` | code → pack: absent from section 2 |
| `wall_thickness_interactive_report.InteractiveReportBuilder` | code → pack: absent from section 2 |
| `wall_thickness_interactive_report.InteractiveReportBuilder.add_utilisation_vs_wt_chart` | code → pack: absent from section 2 |
| `wall_thickness_interactive_report.InteractiveReportBuilder.add_phase_comparison_chart` | code → pack: absent from section 2 |
| `wall_thickness_interactive_report.InteractiveReportBuilder.build` | code → pack: absent from section 2 |
| `wall_thickness_lookup.LookupConfig` | code → pack: absent from section 2 |
| `wall_thickness_lookup.MTLookupGenerator.generate` | code → pack: absent from section 2 |
| `wall_thickness_mt_report.generate_mt_report` | code → pack: absent from section 2 |
| `wall_thickness_parametric.ParametricSweep` | code → pack: absent from section 2 |
| `wall_thickness_parametric.ParametricSweep.run` | code → pack: absent from section 2 |
| `wall_thickness_parametric.plot_utilisation_vs_wall_thickness` | code → pack: absent from section 2 |
| `wall_thickness_parametric.plot_utilisation_heatmap` | code → pack: absent from section 2 |
| `wall_thickness_parametric.plot_governing_check` | code → pack: absent from section 2 |
| `wall_thickness_parametric.plot_tension_moment_interaction` | code → pack: absent from section 2 |
| `wall_thickness_parametric.generate_report` | code → pack: absent from section 2 |
| `wall_thickness_parametric.export_to_excel` | code → pack: absent from section 2 |
| `wall_thickness_phases.compute_hydrostatic_pressure` | code → pack: absent from section 2 |
| `wall_thickness_phases.PipeDefinition` | code → pack: absent from section 2 |
| `wall_thickness_phases.PipeDefinition.to_geometry` | code → pack: absent from section 2 |
| `wall_thickness_phases.PipeDefinition.to_material` | code → pack: absent from section 2 |
| `wall_thickness_phases.PipelinePhase` | code → pack: absent from section 2 |
| `wall_thickness_phases.PipelinePhase.to_design_loads` | code → pack: absent from section 2 |
| `wall_thickness_phases.PhaseResult` | code → pack: absent from section 2 |
| `wall_thickness_phases.PhaseComparisonResult` | code → pack: absent from section 2 |
| `wall_thickness_phases.PhaseComparisonResult.to_dataframe` | code → pack: absent from section 2 |
| `wall_thickness_phases.PhaseComparisonResult.summary_dataframe` | code → pack: absent from section 2 |
| `wall_thickness_phases.PhaseAnalysisRunner` | code → pack: absent from section 2 |
| `wall_thickness_phases.PhaseAnalysisRunner.run` | code → pack: absent from section 2 |
| `wall_thickness_phases.plot_phase_utilisation_comparison` | code → pack: absent from section 2 |
| `wall_thickness_phases.plot_phase_tm_interaction` | code → pack: absent from section 2 |
| `wall_thickness_phases.generate_phase_summary_table` | code → pack: absent from section 2 |
| `wall_thickness_phases.generate_phase_report` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 72. `src/digitalmodel/structural/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 35.

| Function | Direction |
| --- | --- |
| `buckling_parametric.BucklingSweepConfig.n_cases` | code → pack: absent from section 2 |
| `buckling_parametric.slenderness_beta` | code → pack: absent from section 2 |
| `buckling_parametric.run_sweep` | code → pack: absent from section 2 |
| `buckling_parametric.utility_curves` | code → pack: absent from section 2 |
| `buckling_parametric.write_outputs` | code → pack: absent from section 2 |
| `buckling_workflow.BucklingParametricWorkflow` | code → pack: absent from section 2 |
| `buckling_workflow.BucklingParametricWorkflow.router` | code → pack: absent from section 2 |
| `panel_buckling_parametric.PanelSweepConfig.n_cases` | code → pack: absent from section 2 |
| `panel_buckling_parametric.run_sweep` | code → pack: absent from section 2 |
| `panel_buckling_parametric.utility_curves` | code → pack: absent from section 2 |
| `panel_buckling_parametric.write_outputs` | code → pack: absent from section 2 |
| `panel_design_explorer.combo_key` | code → pack: absent from section 2 |
| `panel_design_explorer.build_design_grids` | code → pack: absent from section 2 |
| `parametric_coordinator.OrcaFlexUnavailableError` | code → pack: absent from section 2 |
| `parametric_coordinator.StudyType` | code → pack: absent from section 2 |
| `parametric_coordinator.StudySpec` | code → pack: absent from section 2 |
| `parametric_coordinator.ParametricCoordinator` | code → pack: absent from section 2 |
| `parametric_coordinator.ParametricCoordinator.run` | code → pack: absent from section 2 |
| `parametric_report.build_wt_html` | code → pack: absent from section 2 |
| `parametric_report.build_combined_html` | code → pack: absent from section 2 |
| `parametric_report.build_simple_html` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 73. `src/digitalmodel/structural/fatigue/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 121.

| Function | Direction |
| --- | --- |
| `__init__.quick_fatigue_analysis` | code → pack: absent from section 2 |
| `__init__.create_fatigue_report` | code → pack: absent from section 2 |
| `__init__.get_module_info` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisConfig` | code → pack: absent from section 2 |
| `analysis.FatigueValidationResults` | code → pack: absent from section 2 |
| `analysis.FatigueValidationResults.add_warning` | code → pack: absent from section 2 |
| `analysis.FatigueValidationResults.add_error` | code → pack: absent from section 2 |
| `analysis.FatigueValidationResults.add_recommendation` | code → pack: absent from section 2 |
| `analysis.MultislopeSNCurve` | code → pack: absent from section 2 |
| `analysis.MultislopeSNCurve.get_allowable_cycles` | code → pack: absent from section 2 |
| `analysis.MultislopeSNCurve.get_stress_range` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisEngine` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisEngine.analyze_time_series` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisEngine.analyze_psd` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisEngine.calculate_high_stress_range_life` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisEngine.analyze_shear_data` | code → pack: absent from section 2 |
| `analysis.FatigueAnalysisEngine.generate_report` | code → pack: absent from section 2 |
| `analysis.quick_time_domain_analysis` | code → pack: absent from section 2 |
| `analysis.quick_frequency_domain_analysis` | code → pack: absent from section 2 |
| `damage_accumulation.LoadCycle` | code → pack: absent from section 2 |
| `damage_accumulation.DamageState` | code → pack: absent from section 2 |
| `damage_accumulation.DamageAccumulationBase` | code → pack: absent from section 2 |
| `damage_accumulation.DamageAccumulationBase.calculate_damage` | code → pack: absent from section 2 |
| `damage_accumulation.DamageAccumulationBase.reset_history` | code → pack: absent from section 2 |
| `damage_accumulation.ModifiedMinersRule` | code → pack: absent from section 2 |
| `damage_accumulation.ModifiedMinersRule.calculate_damage` | code → pack: absent from section 2 |
| `damage_accumulation.NonlinearDamageAccumulation` | code → pack: absent from section 2 |
| `damage_accumulation.NonlinearDamageAccumulation.calculate_damage` | code → pack: absent from section 2 |
| `damage_accumulation.CriticalPlaneAnalysis` | code → pack: absent from section 2 |
| `damage_accumulation.CriticalPlaneAnalysis.calculate_equivalent_stress` | code → pack: absent from section 2 |
| `damage_accumulation.compare_damage_methods` | code → pack: absent from section 2 |
| `design_code_report.DesignCodeReport` | code → pack: absent from section 2 |
| `design_code_report.DesignCodeReport.generate` | code → pack: absent from section 2 |
| `fatigue_reliability.reliability_index` | code → pack: absent from section 2 |
| `fatigue_reliability.sensitivity_analysis` | code → pack: absent from section 2 |
| `frequency_domain.SpectralMoments` | code → pack: absent from section 2 |
| `frequency_domain.FrequencyDomainResult` | code → pack: absent from section 2 |
| `frequency_domain.FrequencyDomainBase` | code → pack: absent from section 2 |
| `frequency_domain.FrequencyDomainBase.calculate_damage_rate` | code → pack: absent from section 2 |
| `frequency_domain.NarrowBandMethod` | code → pack: absent from section 2 |
| `frequency_domain.NarrowBandMethod.calculate_damage_rate` | code → pack: absent from section 2 |
| `frequency_domain.DirlikMethod` | code → pack: absent from section 2 |
| `frequency_domain.DirlikMethod.calculate_damage_rate` | code → pack: absent from section 2 |
| `frequency_domain.TovoBenasciuttiMethod` | code → pack: absent from section 2 |
| `frequency_domain.TovoBenasciuttiMethod.calculate_damage_rate` | code → pack: absent from section 2 |
| `frequency_domain.SingleMomentMethod` | code → pack: absent from section 2 |
| `frequency_domain.SingleMomentMethod.calculate_damage_rate` | code → pack: absent from section 2 |
| `frequency_domain.ResponsePSDCalculator` | code → pack: absent from section 2 |
| `frequency_domain.ResponsePSDCalculator.calculate_response_psd` | code → pack: absent from section 2 |
| `frequency_domain.ResponsePSDCalculator.sdof_transfer_function` | code → pack: absent from section 2 |
| `frequency_domain.ResponsePSDCalculator.mdof_transfer_function` | code → pack: absent from section 2 |
| `frequency_domain.compare_frequency_methods` | code → pack: absent from section 2 |
| `parametric_sweep.run_sweep` | code → pack: absent from section 2 |
| `parametric_sweep.compute_sensitivity` | code → pack: absent from section 2 |
| `parametric_sweep.generate_sweep_report` | code → pack: absent from section 2 |
| `rainflow.RainflowParameters` | code → pack: absent from section 2 |
| `rainflow.CycleStatistics` | code → pack: absent from section 2 |
| `rainflow.RainflowCounter` | code → pack: absent from section 2 |
| `rainflow.RainflowCounter.count_cycles` | code → pack: absent from section 2 |
| `rainflow.RainflowCounter.create_histogram` | code → pack: absent from section 2 |
| `rainflow.RainflowCounter.extract_cycles_with_means` | code → pack: absent from section 2 |
| `rainflow.RainflowBatch` | code → pack: absent from section 2 |
| `rainflow.RainflowBatch.process_files` | code → pack: absent from section 2 |
| `rainflow.RainflowBatch.get_combined_cycles` | code → pack: absent from section 2 |
| `rainflow.rainflow_count` | code → pack: absent from section 2 |
| `rainflow.rainflow_with_means` | code → pack: absent from section 2 |
| `scatter_fatigue.ScatterFatigueResult` | code → pack: absent from section 2 |
| `scatter_fatigue.scatter_fatigue_damage` | code → pack: absent from section 2 |
| `skill.FatigueAssessmentResult` | code → pack: absent from section 2 |
| `skill.fatigue_assessment` | code → pack: absent from section 2 |
| `sn_comparison_report.generate_sn_comparison_report` | code → pack: absent from section 2 |
| `sn_curve_plotter.SNCurvePlotter` | code → pack: absent from section 2 |
| `sn_curve_plotter.SNCurvePlotter.get_curve_data` | code → pack: absent from section 2 |
| `sn_curve_plotter.SNCurvePlotter.calculate_sn_points` | code → pack: absent from section 2 |
| `sn_curve_plotter.SNCurvePlotter.plot_curves` | code → pack: absent from section 2 |
| `sn_curve_plotter.SNCurvePlotter.list_curves` | code → pack: absent from section 2 |
| `sn_curve_plotter.SNCurvePlotter.create_comparison_plot` | code → pack: absent from section 2 |
| `sn_curve_plotter.main` | code → pack: absent from section 2 |
| `sn_curves.MaterialProperties` | code → pack: absent from section 2 |
| `sn_curves.SNPoint` | code → pack: absent from section 2 |
| `sn_curves.SNCurveBase` | code → pack: absent from section 2 |
| `sn_curves.SNCurveBase.get_allowable_cycles` | code → pack: absent from section 2 |
| `sn_curves.SNCurveBase.get_stress_range` | code → pack: absent from section 2 |
| `sn_curves.PowerLawSNCurve.knee_stress` | code → pack: absent from section 2 |
| `sn_curves.PowerLawSNCurve.get_stress_range` | code → pack: absent from section 2 |
| `sn_curves.BilinearSNCurve` | code → pack: absent from section 2 |
| `sn_curves.BilinearSNCurve.get_allowable_cycles` | code → pack: absent from section 2 |
| `sn_curves.BilinearSNCurve.get_stress_range` | code → pack: absent from section 2 |
| `sn_curves.StandardSNCurves` | code → pack: absent from section 2 |
| `sn_curves.StandardSNCurves.register_custom_curve` | code → pack: absent from section 2 |
| `sn_curves.StandardSNCurves.get_dnv_c203_curve` | code → pack: absent from section 2 |
| `sn_curves.StandardSNCurves.get_curve` | code → pack: absent from section 2 |
| `sn_curves.StandardSNCurves.get_multislope_curve` | code → pack: absent from section 2 |
| `sn_curves.StandardSNCurves.list_curves` | code → pack: absent from section 2 |
| `sn_curves.MeanStressCorrection` | code → pack: absent from section 2 |
| `sn_curves.MeanStressCorrection.goodman` | code → pack: absent from section 2 |
| `sn_curves.MeanStressCorrection.gerber` | code → pack: absent from section 2 |
| `sn_curves.MeanStressCorrection.soderberg` | code → pack: absent from section 2 |
| `sn_curves.MeanStressCorrection.walker` | code → pack: absent from section 2 |
| `sn_curves.ThicknessCorrection` | code → pack: absent from section 2 |
| `sn_curves.ThicknessCorrection.apply_thickness_effect` | code → pack: absent from section 2 |
| `sn_curves.SNDataFitting` | code → pack: absent from section 2 |
| `sn_curves.SNDataFitting.fit_power_law` | code → pack: absent from section 2 |
| `sn_curves.plot_sn_curve` | code → pack: absent from section 2 |
| `sn_curves.get_dnv_curve` | code → pack: absent from section 2 |
| `sn_curves.get_api_curve` | code → pack: absent from section 2 |
| `sn_curves.get_bs_curve` | code → pack: absent from section 2 |
| `worked_examples.ExampleResult` | code → pack: absent from section 2 |
| `worked_examples.pipeline_girth_weld` | code → pack: absent from section 2 |
| `worked_examples.scr_touchdown` | code → pack: absent from section 2 |
| `worked_examples.mooring_chain` | code → pack: absent from section 2 |
| `worked_examples.generate_example_report` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L270-L300` | pack → code: file missing or range invalid |

### 74. `src/digitalmodel/structural/fatigue_apps/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 191.

| Function | Direction |
| --- | --- |
| `__main__.setup_logging` | code → pack: absent from section 2 |
| `__main__.parse_arguments` | code → pack: absent from section 2 |
| `__main__.validate_inputs` | code → pack: absent from section 2 |
| `__main__.run_analysis` | code → pack: absent from section 2 |
| `__main__.main` | code → pack: absent from section 2 |
| `cli_load_scaling.setup_logging` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.setup_paths` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.process_single_configuration` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.process_batch` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.process_parallel` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.validate_inputs` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.generate_report` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.run` | code → pack: absent from section 2 |
| `cli_load_scaling.LoadScalingCLI.run_test_mode` | code → pack: absent from section 2 |
| `cli_load_scaling.main` | code → pack: absent from section 2 |
| `fatigue_damage.SNCurve` | code → pack: absent from section 2 |
| `fatigue_damage.SNCurve.calculate_cycles_to_failure` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.calculate_damage` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.calculate_annual_damage` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.calculate_fatigue_life` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.process_stress_file` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.process_configuration` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.aggregate_damage` | code → pack: absent from section 2 |
| `fatigue_damage.FatigueDamageCalculator.generate_fatigue_report` | code → pack: absent from section 2 |
| `fatigue_damage.main` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.SNCurveParameters` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.SNCurveParameters.a1` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.SNCurveParameters.a2` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator.cycles_to_failure` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator.calculate_damage` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator.calculate_fatigue_life` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator.mean_stress_correction` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator.get_damage_histogram` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.FatigueDamageCalculator.export_results` | code → pack: absent from section 2 |
| `fatigue_damage_calculator.calculate_combined_damage` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer.get_scaled_ref_filename` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer.get_combined_filename` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer.get_rainflow_filename` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer.get_damage_filename` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer.get_summary_filename` | code → pack: absent from section 2 |
| `file_namer.FatigueFileNamer.get_processing_map_filename` | code → pack: absent from section 2 |
| `file_namer.OverallFileNamer` | code → pack: absent from section 2 |
| `file_namer.OverallFileNamer.get_overall_summary_filename` | code → pack: absent from section 2 |
| `file_namer.OverallFileNamer.get_configuration_comparison_filename` | code → pack: absent from section 2 |
| `file_namer.OverallFileNamer.get_weighted_life_filename` | code → pack: absent from section 2 |
| `file_namer.create_output_structure` | code → pack: absent from section 2 |
| `integrated_processor.Configuration` | code → pack: absent from section 2 |
| `integrated_processor.FatigueCondition` | code → pack: absent from section 2 |
| `integrated_processor.TensionToStressConverter` | code → pack: absent from section 2 |
| `integrated_processor.TensionToStressConverter.convert_to_stress` | code → pack: absent from section 2 |
| `integrated_processor.ProductionDataHandler` | code → pack: absent from section 2 |
| `integrated_processor.ProductionDataHandler.get_reference_dirs` | code → pack: absent from section 2 |
| `integrated_processor.ProductionDataHandler.parse_reference_dir` | code → pack: absent from section 2 |
| `integrated_processor.ProductionDataHandler.load_strut_data` | code → pack: absent from section 2 |
| `integrated_processor.IntegratedFatigueProcessor` | code → pack: absent from section 2 |
| `integrated_processor.IntegratedFatigueProcessor.load_fatigue_conditions` | code → pack: absent from section 2 |
| `integrated_processor.IntegratedFatigueProcessor.create_sample_fatigue_conditions` | code → pack: absent from section 2 |
| `integrated_processor.IntegratedFatigueProcessor.select_closest_reference` | code → pack: absent from section 2 |
| `integrated_processor.IntegratedFatigueProcessor.process_fatigue_condition` | code → pack: absent from section 2 |
| `integrated_processor.main` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.IntegratedFatigueProcessorWithNaming` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.IntegratedFatigueProcessorWithNaming.process_single_condition` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.IntegratedFatigueProcessorWithNaming.process_single_sea_state` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.IntegratedFatigueProcessorWithNaming.process_configuration` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.IntegratedFatigueProcessorWithNaming.process_configuration_with_sea_states` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.IntegratedFatigueProcessorWithNaming.run_complete_analysis` | code → pack: absent from section 2 |
| `integrated_processor_with_naming.main` | code → pack: absent from section 2 |
| `load_scaling.ScalingFactors` | code → pack: absent from section 2 |
| `load_scaling.LoadCase` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.load_input_data` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.load_reference_data` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.select_reference_conditions` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.calculate_scaling_factors` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.apply_scaling` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.process_all_cases` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.save_outputs` | code → pack: absent from section 2 |
| `load_scaling.LoadScalingProcessor.run` | code → pack: absent from section 2 |
| `load_scaling.main` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientOutputWriterV2` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientOutputWriterV2.write_scaled_tension` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientOutputWriterV2.finalize` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientDataReaderV2` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientDataReaderV2.read_scaled_tension` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientDataReaderV2.get_file_listing` | code → pack: absent from section 2 |
| `load_scaling_efficient_v2.EfficientDataReaderV2.get_metadata_usage_stats` | code → pack: absent from section 2 |
| `parallel_processor.ProcessingTask` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.create_tasks` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.process_single_task` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.process_batch_parallel` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.process_by_configuration` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.process_by_strut` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.get_performance_metrics` | code → pack: absent from section 2 |
| `parallel_processor.ParallelFatigueProcessor.save_performance_report` | code → pack: absent from section 2 |
| `parallel_processor.AdaptiveParallelProcessor` | code → pack: absent from section 2 |
| `parallel_processor.AdaptiveParallelProcessor.get_system_load` | code → pack: absent from section 2 |
| `parallel_processor.AdaptiveParallelProcessor.adjust_workers` | code → pack: absent from section 2 |
| `parallel_processor.AdaptiveParallelProcessor.process_batch_parallel` | code → pack: absent from section 2 |
| `parallel_processor.parallel_fatigue_analysis` | code → pack: absent from section 2 |
| `rainflow_counter.RainflowCycle` | code → pack: absent from section 2 |
| `rainflow_counter.RainflowCounter` | code → pack: absent from section 2 |
| `rainflow_counter.RainflowCounter.count_cycles` | code → pack: absent from section 2 |
| `rainflow_counter.RainflowCounter.get_cycle_statistics` | code → pack: absent from section 2 |
| `rainflow_counter.RainflowCounter.export_results` | code → pack: absent from section 2 |
| `rainflow_counter.rainflow_count` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.find_peaks_and_valleys` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.apply_gate_filter` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.rainflow_counting_astm` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.process_time_series` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.create_cycle_histogram` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.process_scaled_load_file` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.process_batch` | code → pack: absent from section 2 |
| `rainflow_counting.RainflowCounter.save_cycle_data` | code → pack: absent from section 2 |
| `rainflow_counting.main` | code → pack: absent from section 2 |
| `reference_seastate_processor.Configuration` | code → pack: absent from section 2 |
| `reference_seastate_processor.FatigueCondition` | code → pack: absent from section 2 |
| `reference_seastate_processor.TensionToStressConverter` | code → pack: absent from section 2 |
| `reference_seastate_processor.TensionToStressConverter.convert_to_stress` | code → pack: absent from section 2 |
| `reference_seastate_processor.TensionToStressConverter.convert_array` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.load_fatigue_conditions` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.load_strut_data` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.select_reference_seastate` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.process_fatigue_condition` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.process_configuration` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.calculate_overall_fatigue_life` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.run_analysis` | code → pack: absent from section 2 |
| `reference_seastate_processor.ReferenceSeaStateProcessor.save_results` | code → pack: absent from section 2 |
| `strut_foundation_processor.Configuration` | code → pack: absent from section 2 |
| `strut_foundation_processor.FatigueCondition` | code → pack: absent from section 2 |
| `strut_foundation_processor.SeaState` | code → pack: absent from section 2 |
| `strut_foundation_processor.ProductionDataHandler` | code → pack: absent from section 2 |
| `strut_foundation_processor.ProductionDataHandler.get_reference_files` | code → pack: absent from section 2 |
| `strut_foundation_processor.ProductionDataHandler.parse_reference_name` | code → pack: absent from section 2 |
| `strut_foundation_processor.ProductionDataHandler.load_strut_data` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.load_fatigue_conditions` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.load_sea_states` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.create_sample_sea_states` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.create_sample_fatigue_conditions` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.select_closest_reference` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.process_fatigue_condition` | code → pack: absent from section 2 |
| `strut_foundation_processor.LoadScaler.process_sea_state` | code → pack: absent from section 2 |
| `strut_foundation_processor.main` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.ProductionDataHandlerWithProgress` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.ProductionDataHandlerWithProgress.process_all_configurations` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.ProductionDataHandlerWithProgress.process_single_analysis` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.BatchProcessor` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.BatchProcessor.run_batch_analysis` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.BatchProcessor.save_summary` | code → pack: absent from section 2 |
| `strut_foundation_processor_with_progress.main` | code → pack: absent from section 2 |
| `tension_stress_conversion.TensionStressConverter` | code → pack: absent from section 2 |
| `tension_stress_conversion.TensionStressConverter.convert_tension_to_stress` | code → pack: absent from section 2 |
| `tension_stress_conversion.TensionStressConverter.process_rainflow_results` | code → pack: absent from section 2 |
| `tension_stress_conversion.TensionStressConverter.save_stress_data` | code → pack: absent from section 2 |
| `tension_stress_conversion.TensionStressConverter.process_batch` | code → pack: absent from section 2 |
| `tension_stress_conversion.TensionStressConverter.visualize_lookup_table` | code → pack: absent from section 2 |
| `tension_stress_conversion.main` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.Colors` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.print_header` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.print_step` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.print_success` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.print_warning` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.print_error` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.print_formula` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.wait_for_confirmation` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.run_validation` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.validate_wind_scaling` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.validate_wave_scaling` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.validate_load_combination` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.validate_with_sample_data` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.validate_scaling_factors` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.validate_processing_chain` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.LoadScalingValidator.generate_validation_report` | code → pack: absent from section 2 |
| `validate_load_scaling_interactive.main` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer.plot_sn_curve` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer.plot_damage_histogram` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer.plot_configuration_comparison` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer.plot_rainflow_results` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer.plot_time_series_with_cycles` | code → pack: absent from section 2 |
| `visualizer.FatigueVisualizer.create_dashboard` | code → pack: absent from section 2 |
| `visualizer.generate_all_visualizations` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 75. `src/digitalmodel/structural/jacket_topside/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 32.

| Function | Direction |
| --- | --- |
| `dented_axial_screen.SourceReference` | code → pack: absent from section 2 |
| `dented_axial_screen.DentedAxialInput` | code → pack: absent from section 2 |
| `dented_axial_screen.DentedAxialResult` | code → pack: absent from section 2 |
| `dented_axial_screen.screen_dented_axial` | code → pack: absent from section 2 |
| `joint_checks.JointType` | code → pack: absent from section 2 |
| `joint_checks.JointGeometry.gamma` | code → pack: absent from section 2 |
| `joint_checks.JointGeometry.beta` | code → pack: absent from section 2 |
| `joint_checks.JointGeometry.tau` | code → pack: absent from section 2 |
| `joint_checks.JointGeometry.num_braces` | code → pack: absent from section 2 |
| `joint_checks.JointCheckResult` | code → pack: absent from section 2 |
| `joint_checks.classify_joint` | code → pack: absent from section 2 |
| `member_checks.TubularSection.inner_diameter` | code → pack: absent from section 2 |
| `member_checks.TubularSection.cross_sectional_area` | code → pack: absent from section 2 |
| `member_checks.TubularSection.moment_of_inertia` | code → pack: absent from section 2 |
| `member_checks.TubularSection.section_modulus` | code → pack: absent from section 2 |
| `member_checks.TubularSection.radius_of_gyration` | code → pack: absent from section 2 |
| `member_checks.TubularSection.slenderness_ratio` | code → pack: absent from section 2 |
| `member_checks.TubularSection.dt_ratio` | code → pack: absent from section 2 |
| `member_checks.TubularSection.gamma` | code → pack: absent from section 2 |
| `member_checks.MemberCheckResult` | code → pack: absent from section 2 |
| `member_checks.axial_tension_uc` | code → pack: absent from section 2 |
| `member_checks.axial_compression_uc` | code → pack: absent from section 2 |
| `member_checks.bending_uc` | code → pack: absent from section 2 |
| `member_checks.combined_axial_bending_uc` | code → pack: absent from section 2 |
| `workflow.JacketChecksWorkflow` | code → pack: absent from section 2 |
| `workflow.JacketChecksWorkflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 76. `src/digitalmodel/structural/offshore_resilience/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 43.

| Function | Direction |
| --- | --- |
| `_sensor_templates.fixed_platform_template` | code → pack: absent from section 2 |
| `_sensor_templates.floating_structure_template` | code → pack: absent from section 2 |
| `_sensor_templates.offshore_wind_template` | code → pack: absent from section 2 |
| `installation_checklist.ChecklistStatus` | code → pack: absent from section 2 |
| `installation_checklist.HammerType` | code → pack: absent from section 2 |
| `installation_checklist.FoundationType` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist.pass_count` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist.fail_count` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist.pending_count` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist.completion_ratio` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist.is_complete` | code → pack: absent from section 2 |
| `installation_checklist.InstallationChecklist.summary` | code → pack: absent from section 2 |
| `installation_checklist.build_structure_checklist` | code → pack: absent from section 2 |
| `installation_checklist.build_wind_foundation_checklist` | code → pack: absent from section 2 |
| `lifecycle_planning.DecomFlagSeverity` | code → pack: absent from section 2 |
| `minimum_facility.InstallationMethod` | code → pack: absent from section 2 |
| `minimum_facility.classify_bsee_platform` | code → pack: absent from section 2 |
| `structural_health.AlertSeverity` | code → pack: absent from section 2 |
| `structural_health.MeasurementType` | code → pack: absent from section 2 |
| `structural_health.build_sensor_template` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 77. `src/digitalmodel/structural/pipe_capacity/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 16.

| Function | Direction |
| --- | --- |
| `PipeSizing.PipeSizing.evaluate_pipe_system_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.pipe_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.pipe_section_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.get_fea_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.system_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.get_pipe_system_properties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.pipeProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.steelSectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.insulationSectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.buoyancySectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.sectionProperties` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.equivalentPipe` | code → pack: absent from section 2 |
| `PipeSizing.PipeSizing.buoyancyFactorAndDiameter` | code → pack: absent from section 2 |
| `pipe_capacity.PipeCapacity.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 78. `src/digitalmodel/subsea/catenary_riser/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 59.

| Function | Direction |
| --- | --- |
| `cli.cli` | code → pack: absent from section 2 |
| `cli.effective_weight` | code → pack: absent from section 2 |
| `cli.lazy_wave_analysis` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `effective_weight.EffectiveWeightCalculator` | code → pack: absent from section 2 |
| `effective_weight.EffectiveWeightCalculator.calculate` | code → pack: absent from section 2 |
| `effective_weight.EffectiveWeightCalculator.calculate_with_buoyancy_module` | code → pack: absent from section 2 |
| `lazy_wave.LazyWaveAnalyzer` | code → pack: absent from section 2 |
| `lazy_wave.LazyWaveAnalyzer.analyze` | code → pack: absent from section 2 |
| `lazy_wave.LazyWaveAnalyzer.estimate_required_buoyancy` | code → pack: absent from section 2 |
| `models.RiserType` | code → pack: absent from section 2 |
| `models.FluidType` | code → pack: absent from section 2 |
| `models.MaterialProperties` | code → pack: absent from section 2 |
| `models.FluidProperties` | code → pack: absent from section 2 |
| `models.RiserConfiguration` | code → pack: absent from section 2 |
| `models.RiserConfiguration.inner_diameter` | code → pack: absent from section 2 |
| `models.RiserConfiguration.pipe_cross_section` | code → pack: absent from section 2 |
| `models.RiserConfiguration.internal_area` | code → pack: absent from section 2 |
| `models.RiserConfiguration.external_area` | code → pack: absent from section 2 |
| `models.RiserConfiguration.displaced_volume_per_length` | code → pack: absent from section 2 |
| `models.RiserConfiguration.steel_weight_per_length` | code → pack: absent from section 2 |
| `models.RiserConfiguration.coating_weight_per_length` | code → pack: absent from section 2 |
| `models.RiserConfiguration.contents_weight_per_length` | code → pack: absent from section 2 |
| `models.RiserConfiguration.buoyancy_per_length` | code → pack: absent from section 2 |
| `models.RiserConfiguration.effective_weight_per_length` | code → pack: absent from section 2 |
| `models.BuoyancyModule` | code → pack: absent from section 2 |
| `models.BuoyancyModule.buoyancy_force_per_length` | code → pack: absent from section 2 |
| `models.LazyWaveConfiguration` | code → pack: absent from section 2 |
| `models.LazyWaveConfiguration.total_buoyancy_length` | code → pack: absent from section 2 |
| `models.LazyWaveConfiguration.get_effective_weight_profile` | code → pack: absent from section 2 |
| `models.LazyWaveResult` | code → pack: absent from section 2 |
| `models.EffectiveWeightResult` | code → pack: absent from section 2 |
| `models.EffectiveWeightResult.to_dict` | code → pack: absent from section 2 |
| `models.get_material` | code → pack: absent from section 2 |
| `models.get_fluid` | code → pack: absent from section 2 |
| `report.CodeCheck` | code → pack: absent from section 2 |
| `report.CodeCheck.status` | code → pack: absent from section 2 |
| `report.StrengthResult` | code → pack: absent from section 2 |
| `report.StrengthResult.status` | code → pack: absent from section 2 |
| `report.FatigueResult` | code → pack: absent from section 2 |
| `report.FatigueResult.fatigue_life_years` | code → pack: absent from section 2 |
| `report.FatigueResult.status` | code → pack: absent from section 2 |
| `report.Provenance` | code → pack: absent from section 2 |
| `report.Provenance.mode_label` | code → pack: absent from section 2 |
| `report.RiserReportData` | code → pack: absent from section 2 |
| `report.RiserReportData.max_strength_utilisation` | code → pack: absent from section 2 |
| `report.RiserReportData.min_fatigue_life_years` | code → pack: absent from section 2 |
| `report.RiserReportData.overall_status` | code → pack: absent from section 2 |
| `report.sample_riser_report_data` | code → pack: absent from section 2 |
| `report.render_html` | code → pack: absent from section 2 |
| `report.render_markdown` | code → pack: absent from section 2 |
| `report.generate_riser_report` | code → pack: absent from section 2 |
| `simple_catenary.SimpleCatenaryAnalyzer.calculate_tension_along_riser` | code → pack: absent from section 2 |
| `simple_catenary.SimpleCatenaryAnalyzer.calculate_angle_along_riser` | code → pack: absent from section 2 |
| `simple_catenary.SimpleCatenaryAnalyzer.check_design_limits` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 79. `src/digitalmodel/subsea/flexible_pipes/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 31.

| Function | Direction |
| --- | --- |
| `bend_stiffener.developed_min_radius` | code → pack: absent from section 2 |
| `bend_stiffener.required_effective_ei` | code → pack: absent from section 2 |
| `bend_stiffener.screen_bend_stiffener` | code → pack: absent from section 2 |
| `cross_section.Layer.mean_diameter` | code → pack: absent from section 2 |
| `cross_section.Layer.outer_diameter` | code → pack: absent from section 2 |
| `cross_section.Layer.annular_area` | code → pack: absent from section 2 |
| `cross_section.Layer.structural_area` | code → pack: absent from section 2 |
| `cross_section.Layer.mass_per_length` | code → pack: absent from section 2 |
| `cross_section.Layer.axial_stiffness` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.bore_diameter` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.outer_diameter` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.dry_mass_per_length` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.displaced_mass_per_length` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.content_mass_per_length` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.submerged_weight_per_length` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.tensile_armour_area` | code → pack: absent from section 2 |
| `cross_section.FlexiblePipeSection.axial_stiffness` | code → pack: absent from section 2 |
| `design_checks_17J.LoadCase` | code → pack: absent from section 2 |
| `design_checks_17J.FlexCheckResult.max_utilisation` | code → pack: absent from section 2 |
| `design_checks_17J.allowable_wire_stress` | code → pack: absent from section 2 |
| `design_checks_17J.armour_tension_stress` | code → pack: absent from section 2 |
| `design_checks_17J.armour_bending_stress` | code → pack: absent from section 2 |
| `design_checks_17J.verify_flexible_pipe` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 80. `src/digitalmodel/subsea/mooring_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 78.

| Function | Direction |
| --- | --- |
| `catenary.CatenaryAnalyzer` | code → pack: absent from section 2 |
| `catenary.CatenaryAnalyzer.solve_catenary` | code → pack: absent from section 2 |
| `catenary.CatenaryAnalyzer.solve_for_horizontal_tension` | code → pack: absent from section 2 |
| `catenary.CatenaryAnalyzer.calculate_stiffness` | code → pack: absent from section 2 |
| `catenary.CatenaryAnalyzer.check_touchdown` | code → pack: absent from section 2 |
| `cli.catenary_analysis` | code → pack: absent from section 2 |
| `cli.design_verification` | code → pack: absent from section 2 |
| `cli.generate_orcaflex_model` | code → pack: absent from section 2 |
| `cli.list_materials` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `designer.MooringDesigner` | code → pack: absent from section 2 |
| `designer.MooringDesigner.calculate_environmental_loads` | code → pack: absent from section 2 |
| `designer.MooringDesigner.analyze_line_tension` | code → pack: absent from section 2 |
| `designer.MooringDesigner.analyze_intact_condition` | code → pack: absent from section 2 |
| `designer.MooringDesigner.analyze_damaged_condition` | code → pack: absent from section 2 |
| `designer.MooringDesigner.analyze_all_conditions` | code → pack: absent from section 2 |
| `designer.MooringDesigner.generate_design_summary` | code → pack: absent from section 2 |
| `dynmoor.DynmoorSegment.weight_water_lb_ft` | code → pack: absent from section 2 |
| `dynmoor.DynmoorSegment.ea_lb` | code → pack: absent from section 2 |
| `dynmoor.NonlinearSpring` | code → pack: absent from section 2 |
| `dynmoor.DynmoorLineType` | code → pack: absent from section 2 |
| `dynmoor.DynmoorLineType.brkm_kips` | code → pack: absent from section 2 |
| `dynmoor.DynmoorLineType.friction` | code → pack: absent from section 2 |
| `dynmoor.DynmoorLine` | code → pack: absent from section 2 |
| `dynmoor.LineTable` | code → pack: absent from section 2 |
| `dynmoor.build_line_table` | code → pack: absent from section 2 |
| `dynmoor.interp_table` | code → pack: absent from section 2 |
| `dynmoor.anchor_pull` | code → pack: absent from section 2 |
| `dynmoor.qinter_table` | code → pack: absent from section 2 |
| `dynmoor.PretensionSolution` | code → pack: absent from section 2 |
| `dynmoor.anchor_position` | code → pack: absent from section 2 |
| `dynmoor.RestoringForceResult` | code → pack: absent from section 2 |
| `dynmoor.Fender` | code → pack: absent from section 2 |
| `dynmoor.Fender.stiffnesses` | code → pack: absent from section 2 |
| `dynmoor.fender_forces` | code → pack: absent from section 2 |
| `dynmoor.wind_height_coefficient` | code → pack: absent from section 2 |
| `dynmoor.WindArea` | code → pack: absent from section 2 |
| `dynmoor.WindLoadModel` | code → pack: absent from section 2 |
| `dynmoor.build_wind_model` | code → pack: absent from section 2 |
| `dynmoor.static_wind_force` | code → pack: absent from section 2 |
| `dynmoor.anchor_holding_power` | code → pack: absent from section 2 |
| `dynmoor.parship_forces` | code → pack: absent from section 2 |
| `dynmoor.pership_forces` | code → pack: absent from section 2 |
| `dynmoor.PassingForce` | code → pack: absent from section 2 |
| `dynmoor.PassingSweepSample` | code → pack: absent from section 2 |
| `dynmoor.passing_ship_sweep` | code → pack: absent from section 2 |
| `fpso_workflow.FPSOMooringWorkflow` | code → pack: absent from section 2 |
| `fpso_workflow.FPSOMooringWorkflow.router` | code → pack: absent from section 2 |
| `models.MooringType` | code → pack: absent from section 2 |
| `models.LineType` | code → pack: absent from section 2 |
| `models.ConditionType` | code → pack: absent from section 2 |
| `models.MooringLineProperties` | code → pack: absent from section 2 |
| `models.MooringLine.total_length` | code → pack: absent from section 2 |
| `models.MooringLine.min_mbl` | code → pack: absent from section 2 |
| `models.MooringSystem` | code → pack: absent from section 2 |
| `models.MooringSystem.n_lines` | code → pack: absent from section 2 |
| `models.EnvironmentalLoads` | code → pack: absent from section 2 |
| `models.DesignLoadCase` | code → pack: absent from section 2 |
| `models.DesignLoadCase.safety_factor_required` | code → pack: absent from section 2 |
| `models.get_material` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator.generate_line_type` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator.generate_line_data` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator.generate_vessel_data` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator.generate_environment_data` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator.generate_model_yml` | code → pack: absent from section 2 |
| `orcaflex_generator.OrcaFlexModelGenerator.generate_model_dict` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenaryMethods.py:L434-L508` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/legacy/catenary_equation.py:L63-L81` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L279-L342` | pack → code: file missing or range invalid |

### 81. `src/digitalmodel/subsea/on_bottom_stability/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 40.

| Function | Direction |
| --- | --- |
| `dnv_rp_f109.absolute_stability_check` | code → pack: absent from section 2 |
| `dnv_rp_f109.generalized_stability_check` | code → pack: absent from section 2 |
| `e305_fixture.validate_fixture` | code → pack: absent from section 2 |
| `e305_fixture.load_fixture` | code → pack: absent from section 2 |
| `e305_fixture.convert_workbook` | code → pack: absent from section 2 |
| `e305_fixture.compare_fixture` | code → pack: absent from section 2 |
| `e305_simplified.WeightResult` | code → pack: absent from section 2 |
| `e305_simplified.weight_build_up` | code → pack: absent from section 2 |
| `e305_simplified.CaseResult` | code → pack: absent from section 2 |
| `e305_simplified.dimensionless_ratios` | code → pack: absent from section 2 |
| `e305_simplified.evaluate_case` | code → pack: absent from section 2 |
| `e305_simplified.diameter_averaged_current` | code → pack: absent from section 2 |
| `e305_simplified.wave_from_sea_state` | code → pack: absent from section 2 |
| `e305_simplified.InverseResult` | code → pack: absent from section 2 |
| `e305_simplified.inverse_concrete_thickness` | code → pack: absent from section 2 |
| `e305_tables.TableError` | code → pack: absent from section 2 |
| `e305_tables.TableSet` | code → pack: absent from section 2 |
| `e305_tables.TableSet.lookup` | code → pack: absent from section 2 |
| `e305_tables.load_tables` | code → pack: absent from section 2 |
| `e305_workflow.router` | code → pack: absent from section 2 |
| `simplified.SubmergedWeightResult` | code → pack: absent from section 2 |
| `simplified.HydrodynamicLoadResult` | code → pack: absent from section 2 |
| `simplified.VerticalStabilityResult` | code → pack: absent from section 2 |
| `simplified.VerticalStabilityResult.utilisation` | code → pack: absent from section 2 |
| `simplified.submerged_weight` | code → pack: absent from section 2 |
| `simplified.hydrodynamic_loads` | code → pack: absent from section 2 |
| `simplified.drag_force_per_meter` | code → pack: absent from section 2 |
| `simplified.inertia_force_per_meter` | code → pack: absent from section 2 |
| `simplified.lateral_stability_check` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 82. `src/digitalmodel/subsea/pipeline/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 110.

| Function | Direction |
| --- | --- |
| `api_rp_1111_installation.slay_sagbend_bending_strain` | code → pack: absent from section 2 |
| `api_rp_1111_installation.slay_overbend_bending_strain` | code → pack: absent from section 2 |
| `api_rp_1111_installation.slay_allowable_bending_strain` | code → pack: absent from section 2 |
| `api_rp_1111_installation.slay_bending_check` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.run` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_friction_force` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_mesh` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_internal_pressure` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_external_pressure` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.assign_mesh` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_differential_temp` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_circumferential_stress` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_longitudinal_stress_fully_restrained` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_longitudinal_force_fully_restrained` | code → pack: absent from section 2 |
| `buckling_common.CommonBucklingCaculations.get_water_depth` | code → pack: absent from section 2 |
| `integrity_workflows.run_free_span` | code → pack: absent from section 2 |
| `integrity_workflows.run_lateral_buckling` | code → pack: absent from section 2 |
| `integrity_workflows.run_upheaval_buckling` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.run` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_results` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_plots` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_friction_force` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_lateral_buckling` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.calculateLateralBuckling` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_anchor_length` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_longitudinal_strain_and_thermal_stress` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_longitudinal_force_and_stress` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_mesh` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.get_differential_temp` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_temperature_plot` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_buckling_force_plot_imp` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_buckling_force_plot_met` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_buckling_stress_plot_imp` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_buckling_stress_plot_met` | code → pack: absent from section 2 |
| `lateral_buckling.LateralBuckling.save_buckling_strain_plot` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.evaluate_pipe_system_properties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.pipe_properties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.pipe_section_properties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.get_fea_properties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.system_properties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.get_pipe_system_properties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.pipeProperties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.steelSectionProperties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.insulationSectionProperties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.buoyancySectionProperties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.sectionProperties` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.equivalentPipe` | code → pack: absent from section 2 |
| `pipe_sizing.PipeSizing.buoyancyFactorAndDiameter` | code → pack: absent from section 2 |
| `pipeline.Pipeline` | code → pack: absent from section 2 |
| `pipeline.Pipeline.router` | code → pack: absent from section 2 |
| `pipeline.Pipeline.get_pipe_properties` | code → pack: absent from section 2 |
| `pipeline_pressure.PipeGeometry.D_over_t` | code → pack: absent from section 2 |
| `pipeline_pressure.dnv_burst_resistance` | code → pack: absent from section 2 |
| `pipeline_pressure.dnv_pressure_containment_check` | code → pack: absent from section 2 |
| `pipeline_pressure.dnv_system_pressure_test` | code → pack: absent from section 2 |
| `pipeline_pressure.dnv_maop` | code → pack: absent from section 2 |
| `pipeline_pressure.api_water_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure.api_elastic_collapse_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure.api_collapse_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure.api_external_collapse_check` | code → pack: absent from section 2 |
| `pipeline_pressure.api_ovality_reduction_factor` | code → pack: absent from section 2 |
| `pipeline_pressure.api_combined_loading_external_check` | code → pack: absent from section 2 |
| `pipeline_pressure.api_combined_loading_internal_check` | code → pack: absent from section 2 |
| `pipeline_pressure.api_propagating_buckle_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure.api_transition_water_depth` | code → pack: absent from section 2 |
| `pipeline_pressure.api_propagating_buckle_check` | code → pack: absent from section 2 |
| `pipeline_pressure_dnv.dnv_collapse_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure_dnv.dnv_external_collapse_check` | code → pack: absent from section 2 |
| `pipeline_pressure_dnv.dnv_propagating_buckle_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure_dnv.dnv_propagating_buckle_check` | code → pack: absent from section 2 |
| `pipeline_pressure_dnv.dnv_combined_loading_check` | code → pack: absent from section 2 |
| `pipeline_pressure_workflow.api_burst_pressure` | code → pack: absent from section 2 |
| `pipeline_pressure_workflow.api_internal_pressure_check` | code → pack: absent from section 2 |
| `pipeline_pressure_workflow.api_combined_loading_check` | code → pack: absent from section 2 |
| `pipeline_pressure_workflow.WallThicknessSizingWorkflow.run` | code → pack: absent from section 2 |
| `pipeline_pressure_workflow.run_sizing_from_yaml` | code → pack: absent from section 2 |
| `pressure_loss.Pressureloss.run` | code → pack: absent from section 2 |
| `pressure_loss.Pressureloss.get_pipe_data` | code → pack: absent from section 2 |
| `pressure_loss.Pressureloss.get_fluid_properties` | code → pack: absent from section 2 |
| `pressure_loss.Pressureloss.flow_parameters` | code → pack: absent from section 2 |
| `thermal_buckling.ThermalBuckling` | code → pack: absent from section 2 |
| `thermal_buckling.ThermalBuckling.run` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.run` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_friction_force` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_upheaval_buckling` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_level_1_analysis` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_buckle_result` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_soil_pipe_friction_model` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_uplift_resistance_model` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_trench_and_rock_burial_model` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.get_force_in_pipe_section` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_results` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_temperature_plot` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_buckling_force_plot_imp` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_buckling_force_plot_met` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_buckling_stress_plot_imp` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_buckling_stress_plot_met` | code → pack: absent from section 2 |
| `upheaval_buckling.UpheavalBuckling.save_buckling_strain_plot` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `scripts/capabilities/build_onepagers.py:L1-L915` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/catenary_riser/simple_catenary.py:L396-L428` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L270-L300` | pack → code: file missing or range invalid |

### 83. `src/digitalmodel/subsea/riser/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 6.

| Function | Direction |
| --- | --- |
| `riser_case_screening.RiserLoadCase.to_loads` | code → pack: absent from section 2 |
| `riser_case_screening.screen_riser_cases` | code → pack: absent from section 2 |
| `riser_case_screening.run_riser_screening` | code → pack: absent from section 2 |
| `riser_case_screening.build_calc_note` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 84. `src/digitalmodel/subsea/viv_analysis/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 87.

| Function | Direction |
| --- | --- |
| `cli.natural_frequency` | code → pack: absent from section 2 |
| `cli.screening_cmd` | code → pack: absent from section 2 |
| `cli.main` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator.calculate_allowable_cycles` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator.calculate_stress_range` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator.calculate_damage` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator.calculate_fatigue_life` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator.analyze_viv_fatigue` | code → pack: absent from section 2 |
| `fatigue.VIVFatigueCalculator.cumulative_damage` | code → pack: absent from section 2 |
| `frequency_calculator.FrequencyCalculator` | code → pack: absent from section 2 |
| `frequency_calculator.FrequencyCalculator.calculate_natural_frequency` | code → pack: absent from section 2 |
| `frequency_calculator.FrequencyCalculator.calculate_multiple_modes` | code → pack: absent from section 2 |
| `frequency_calculator.FrequencyCalculator.calculate_effective_tension` | code → pack: absent from section 2 |
| `frequency_calculator.FrequencyCalculator.check_stability` | code → pack: absent from section 2 |
| `models.BoundaryCondition` | code → pack: absent from section 2 |
| `models.CurrentProfileType` | code → pack: absent from section 2 |
| `models.DesignCode` | code → pack: absent from section 2 |
| `models.MaterialProperties` | code → pack: absent from section 2 |
| `models.MaterialProperties.youngs_modulus_pa` | code → pack: absent from section 2 |
| `models.TubularMember` | code → pack: absent from section 2 |
| `models.TubularMember.inner_diameter` | code → pack: absent from section 2 |
| `models.TubularMember.cross_sectional_area` | code → pack: absent from section 2 |
| `models.TubularMember.second_moment_of_area` | code → pack: absent from section 2 |
| `models.TubularMember.mass_per_length` | code → pack: absent from section 2 |
| `models.FluidProperties` | code → pack: absent from section 2 |
| `models.CurrentProfile` | code → pack: absent from section 2 |
| `models.CurrentProfile.velocity_at_depth` | code → pack: absent from section 2 |
| `models.NaturalFrequencyResult` | code → pack: absent from section 2 |
| `models.VortexSheddingResult` | code → pack: absent from section 2 |
| `models.ReducedVelocityResult` | code → pack: absent from section 2 |
| `models.VIVScreeningResult` | code → pack: absent from section 2 |
| `models.VIVFatigueResult` | code → pack: absent from section 2 |
| `models.get_material` | code → pack: absent from section 2 |
| `screening.VIVScreening` | code → pack: absent from section 2 |
| `screening.VIVScreening.reduced_velocity` | code → pack: absent from section 2 |
| `screening.VIVScreening.check_lock_in` | code → pack: absent from section 2 |
| `screening.VIVScreening.calculate_safety_factor` | code → pack: absent from section 2 |
| `screening.VIVScreening.screen_member` | code → pack: absent from section 2 |
| `screening.VIVScreening.screen_multiple_modes` | code → pack: absent from section 2 |
| `screening.VIVScreening.screen_with_current_profile` | code → pack: absent from section 2 |
| `screening.VIVScreening.critical_current_velocity` | code → pack: absent from section 2 |
| `sn_parameters.SNParameters` | code → pack: absent from section 2 |
| `validation.require_positive` | code → pack: absent from section 2 |
| `validation.require_nonnegative` | code → pack: absent from section 2 |
| `validation.validate_lock_in_inputs` | code → pack: absent from section 2 |
| `viv_analysis.VIVAnalysis` | code → pack: absent from section 2 |
| `viv_analysis.VIVAnalysis.router` | code → pack: absent from section 2 |
| `viv_analysis.VIVAnalysis.get_pipe_properties` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.modal_analysis` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.readFirstLine` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.save_modal_visualizations` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.save_mode_shapes` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.get_current_data` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.plot_current_non_exceedance` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.plot_current_exceedance` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.plot_current_profiles` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.get_wave_data` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.plot_wave_data` | code → pack: absent from section 2 |
| `viv_analysis_components.VIVAnalysisComponents.plot_wave_exceedance` | code → pack: absent from section 2 |
| `viv_analysis_legacy.set_up_application` | code → pack: absent from section 2 |
| `viv_analysis_legacy.run_cfg_variations` | code → pack: absent from section 2 |
| `viv_analysis_legacy.run_cfg_variations_by_type` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.analyze` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.save_results` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.save_plots` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_natural_frequencies` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_free_free_modes` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_clamped_modes` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_simply_supported_modes` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_vs_frequencies` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_safety_factors` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_natural_frequencies_plot` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_vs_frequencies_plot` | code → pack: absent from section 2 |
| `viv_tubular_members.VIVTubularMembers.get_safety_factor_plot` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.shedding_frequency` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.reynolds_number` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.strouhal_from_reynolds` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.analyze_member` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.analyze_with_profile` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.dominant_shedding_frequency` | code → pack: absent from section 2 |
| `vortex_shedding.VortexSheddingAnalyzer.rms_shedding_frequency` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L1-L300` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L209-L300` | pack → code: file missing or range invalid |
| `src/digitalmodel/subsea/viv_analysis/fatigue.py:L289-L300` | pack → code: file missing or range invalid |
| `tests/subsea/viv_analysis/test_viv_analysis.py:L1-L49` | pack → code: file missing or range invalid |
| `tests/subsea/viv_analysis/test_viv_analysis.py:L13-L47` | pack → code: file missing or range invalid |

### 85. `src/digitalmodel/synthetic_rope_mooring_fatigue/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 1.

| Function | Direction |
| --- | --- |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 86. `src/digitalmodel/vessel_stability_screening/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 1.

| Function | Direction |
| --- | --- |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 87. `src/digitalmodel/viv_screening/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 3.

| Function | Direction |
| --- | --- |
| `inputs.parse_inputs` | code → pack: absent from section 2 |
| `workflow.router` | code → pack: absent from section 2 |
| `workflow.save_echo` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 88. `src/digitalmodel/weather_window/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 3.

| Function | Direction |
| --- | --- |
| `workflow.window_lengths_hours` | code → pack: absent from section 2 |
| `workflow.assess_weather_window` | code → pack: absent from section 2 |
| `workflow.router` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 89. `src/digitalmodel/well/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 6.

| Function | Direction |
| --- | --- |
| `workflow.run_well_bore_design` | code → pack: absent from section 2 |
| `workflow.run_well_hydraulics` | code → pack: absent from section 2 |
| `workflow.run_swab_surge` | code → pack: absent from section 2 |
| `workflow.run_rop_analysis` | code → pack: absent from section 2 |
| `workflow.run_tubular_design` | code → pack: absent from section 2 |
| `workflow.run_casing_design` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 90. `src/digitalmodel/well/drilling/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 84.

| Function | Direction |
| --- | --- |
| `dual_gradient.PressurePoint` | code → pack: absent from section 2 |
| `dual_gradient.PressurePoint.as_dict` | code → pack: absent from section 2 |
| `dual_gradient.DrillingWindow` | code → pack: absent from section 2 |
| `dual_gradient.DrillingWindow.from_gradients` | code → pack: absent from section 2 |
| `dual_gradient.DrillingWindow.td_ft` | code → pack: absent from section 2 |
| `dual_gradient.DrillingWindow.pore_at` | code → pack: absent from section 2 |
| `dual_gradient.DrillingWindow.fracture_at` | code → pack: absent from section 2 |
| `dual_gradient.CasingDesign` | code → pack: absent from section 2 |
| `dual_gradient.CasingDesign.as_dict` | code → pack: absent from section 2 |
| `dual_gradient.RiserMargin` | code → pack: absent from section 2 |
| `dual_gradient.RiserMargin.as_dict` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.bhp_conventional` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.emw_conventional` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.bhp_dual_gradient` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.emw_dual_gradient` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.pressure_point` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.mud_weight_bounds` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.available_window_ppg` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.min_available_window_ppg` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.casing_design` | code → pack: absent from section 2 |
| `dual_gradient.DualGradientScreen.riser_margin` | code → pack: absent from section 2 |
| `dysfunction_detector.DysfunctionType` | code → pack: absent from section 2 |
| `dysfunction_detector.DysfunctionEvent` | code → pack: absent from section 2 |
| `dysfunction_detector.DysfunctionDetector` | code → pack: absent from section 2 |
| `dysfunction_detector.DysfunctionDetector.detect` | code → pack: absent from section 2 |
| `hydraulics.WellboreHydraulics` | code → pack: absent from section 2 |
| `hydraulics.WellboreHydraulics.annular_velocity` | code → pack: absent from section 2 |
| `hydraulics.WellboreHydraulics.pressure_drop_annulus` | code → pack: absent from section 2 |
| `hydraulics.WellboreHydraulics.ecd` | code → pack: absent from section 2 |
| `hydraulics.WellboreHydraulics.cuttings_transport_ratio` | code → pack: absent from section 2 |
| `pore_pressure.PorePressureDistribution` | code → pack: absent from section 2 |
| `pore_pressure.PorePressureDistribution.mean` | code → pack: absent from section 2 |
| `pore_pressure.PorePressureDistribution.shifted` | code → pack: absent from section 2 |
| `pore_pressure.PorePressureDistribution.scaled` | code → pack: absent from section 2 |
| `pore_pressure.PorePressureDistribution.as_dict` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow.static_width_ppg` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow.dynamic_width_ppg` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow.static_feasible` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow.dynamic_feasible` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow.recommended_mud_weight_ppg` | code → pack: absent from section 2 |
| `pore_pressure.SafeDrillingWindow.as_dict` | code → pack: absent from section 2 |
| `pore_pressure.eaton_pore_pressure` | code → pack: absent from section 2 |
| `pore_pressure.bowers_pore_pressure` | code → pack: absent from section 2 |
| `pore_pressure.combine_methods` | code → pack: absent from section 2 |
| `pore_pressure.calibrate` | code → pack: absent from section 2 |
| `pore_pressure.safe_drilling_window` | code → pack: absent from section 2 |
| `rop_models.RopPrediction` | code → pack: absent from section 2 |
| `rop_models.BourgoineYoungROP` | code → pack: absent from section 2 |
| `rop_models.BourgoineYoungROP.predict` | code → pack: absent from section 2 |
| `rop_models.BourgoineYoungROP.sensitivity_wob` | code → pack: absent from section 2 |
| `rop_models.BourgoineYoungROP.sensitivity_rpm` | code → pack: absent from section 2 |
| `rop_models.WarrenROP` | code → pack: absent from section 2 |
| `rop_models.WarrenROP.predict` | code → pack: absent from section 2 |
| `rop_models.WarrenROP.fit_from_data` | code → pack: absent from section 2 |
| `swab_surge.SwabSurgeResult` | code → pack: absent from section 2 |
| `swab_surge.SwabSurgeResult.as_dict` | code → pack: absent from section 2 |
| `swab_surge.TripSpeedLimit` | code → pack: absent from section 2 |
| `swab_surge.TripSpeedLimit.governing_ft_min` | code → pack: absent from section 2 |
| `swab_surge.TripSpeedLimit.as_dict` | code → pack: absent from section 2 |
| `swab_surge.SwabSurge` | code → pack: absent from section 2 |
| `swab_surge.SwabSurge.displacement_factor` | code → pack: absent from section 2 |
| `swab_surge.SwabSurge.effective_velocity` | code → pack: absent from section 2 |
| `swab_surge.SwabSurge.pressure` | code → pack: absent from section 2 |
| `swab_surge.SwabSurge.evaluate` | code → pack: absent from section 2 |
| `swab_surge.SwabSurge.max_safe_trip_speed` | code → pack: absent from section 2 |
| `well_bore_design.CasingString` | code → pack: absent from section 2 |
| `well_bore_design.CostBreakdown` | code → pack: absent from section 2 |
| `well_bore_design.WellDesign` | code → pack: absent from section 2 |
| `well_bore_design.WellDesign.casing_program` | code → pack: absent from section 2 |
| `well_bore_design.WellDesign.estimate_cost` | code → pack: absent from section 2 |
| `well_bore_design.WellDesign.production_potential` | code → pack: absent from section 2 |
| `well_bore_design.WellDesign.risk_assessment` | code → pack: absent from section 2 |
| `well_bore_design.WellDesign.recommend_hole_type` | code → pack: absent from section 2 |
| `wellbore_stability.StabilityInput` | code → pack: absent from section 2 |
| `wellbore_stability.stress_in_well_axes` | code → pack: absent from section 2 |
| `wellbore_stability.kirsch_wall` | code → pack: absent from section 2 |
| `wellbore_stability.failure_margin` | code → pack: absent from section 2 |
| `wellbore_stability.pressure_bounds` | code → pack: absent from section 2 |
| `wellbore_stability.run_workflow` | code → pack: absent from section 2 |
| `wellbore_stability.main` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 91. `src/digitalmodel/well/tubulars/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 83.

| Function | Direction |
| --- | --- |
| `casing.CasingGrade` | code → pack: absent from section 2 |
| `casing.casing_grade` | code → pack: absent from section 2 |
| `casing.internal_yield_pressure` | code → pack: absent from section 2 |
| `casing.pipe_body_yield_strength` | code → pack: absent from section 2 |
| `casing.collapse_pressure` | code → pack: absent from section 2 |
| `casing.Casing.id_in` | code → pack: absent from section 2 |
| `casing.Casing.d_over_t` | code → pack: absent from section 2 |
| `casing.Casing.metal_area_in2` | code → pack: absent from section 2 |
| `casing.Casing.burst_psi` | code → pack: absent from section 2 |
| `casing.Casing.body_yield_lbf` | code → pack: absent from section 2 |
| `casing.Casing.collapse_psi` | code → pack: absent from section 2 |
| `casing.Casing.collapse_regime` | code → pack: absent from section 2 |
| `casing.Casing.reference` | code → pack: absent from section 2 |
| `casing.Casing.to_tubular_geometry` | code → pack: absent from section 2 |
| `casing.list_sizes` | code → pack: absent from section 2 |
| `casing.wall_for_size` | code → pack: absent from section 2 |
| `casing.list_tubing_sizes` | code → pack: absent from section 2 |
| `casing.tubing_wall_for_size` | code → pack: absent from section 2 |
| `casing_design.api_round_pressure_psi` | code → pack: absent from section 2 |
| `casing_design.api_round_force_lbf` | code → pack: absent from section 2 |
| `casing_design.fluid_column_profile` | code → pack: absent from section 2 |
| `casing_design.shut_in_tubing_pressure` | code → pack: absent from section 2 |
| `casing_design.tubing_leak_internal_profile` | code → pack: absent from section 2 |
| `casing_design.injection_internal_profile` | code → pack: absent from section 2 |
| `casing_design.burst_external_profile` | code → pack: absent from section 2 |
| `casing_design.full_evacuation_internal_profile` | code → pack: absent from section 2 |
| `casing_design.DesignFactors` | code → pack: absent from section 2 |
| `casing_design.DesignFactors.burst_for_sicp` | code → pack: absent from section 2 |
| `casing_design.check_burst` | code → pack: absent from section 2 |
| `casing_design.reduced_yield_under_tension` | code → pack: absent from section 2 |
| `casing_design.collapse_pressure_under_tension` | code → pack: absent from section 2 |
| `casing_design.check_collapse` | code → pack: absent from section 2 |
| `casing_design.buoyancy_factor` | code → pack: absent from section 2 |
| `casing_design.axial_load_profile` | code → pack: absent from section 2 |
| `casing_design.ballooning_tension_delta_lbf` | code → pack: absent from section 2 |
| `casing_design.cooling_tension_delta_lbf` | code → pack: absent from section 2 |
| `casing_design.check_tension` | code → pack: absent from section 2 |
| `casing_design.check_triaxial` | code → pack: absent from section 2 |
| `casing_design.max_frac_surface_pressure` | code → pack: absent from section 2 |
| `casing_design.h2s_partial_pressure_psia` | code → pack: absent from section 2 |
| `casing_design.SourServiceAssessment` | code → pack: absent from section 2 |
| `casing_design.sour_service_screen` | code → pack: absent from section 2 |
| `casing_design.grade_sour_ok` | code → pack: absent from section 2 |
| `casing_design.ConnectionClass` | code → pack: absent from section 2 |
| `casing_design.ConnectionClass.joint_strength_lbf` | code → pack: absent from section 2 |
| `casing_design.connection_class` | code → pack: absent from section 2 |
| `casing_design.check_production_casing` | code → pack: absent from section 2 |
| `design_envelope.compute_hoop_stress` | code → pack: absent from section 2 |
| `design_envelope.compute_vme_stress` | code → pack: absent from section 2 |
| `design_envelope.IsotropicVmeEnvelope` | code → pack: absent from section 2 |
| `design_envelope.IsotropicVmeEnvelope.compute` | code → pack: absent from section 2 |
| `design_envelope.ApiEllipseEnvelope` | code → pack: absent from section 2 |
| `design_envelope.ApiEllipseEnvelope.compute` | code → pack: absent from section 2 |
| `design_envelope.AnisotropicVmeEnvelope` | code → pack: absent from section 2 |
| `design_envelope.AnisotropicVmeEnvelope.compute` | code → pack: absent from section 2 |
| `sucker_rod.RodGrade` | code → pack: absent from section 2 |
| `sucker_rod.rod_grade` | code → pack: absent from section 2 |
| `sucker_rod.rod_area` | code → pack: absent from section 2 |
| `sucker_rod.modified_goodman_allowable` | code → pack: absent from section 2 |
| `sucker_rod.SuckerRod` | code → pack: absent from section 2 |
| `sucker_rod.SuckerRod.area_in2` | code → pack: absent from section 2 |
| `sucker_rod.SuckerRod.stress_psi` | code → pack: absent from section 2 |
| `sucker_rod.SuckerRod.allowable_stress_psi` | code → pack: absent from section 2 |
| `sucker_rod.SuckerRod.goodman_check` | code → pack: absent from section 2 |
| `sucker_rod.SuckerRod.to_rod_section` | code → pack: absent from section 2 |
| `sucker_rod.RodTaperSection` | code → pack: absent from section 2 |
| `sucker_rod.rod_string_goodman` | code → pack: absent from section 2 |
| `tubing_design.shut_in_tubing_profile` | code → pack: absent from section 2 |
| `tubing_design.packer_fluid_annulus_profile` | code → pack: absent from section 2 |
| `tubing_design.shut_in_piston_force_lbf` | code → pack: absent from section 2 |
| `tubing_design.check_production_tubing` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

### 92. `src/digitalmodel/well_access/docs/agent-pack.md`

Strict checker exit: 1; package definitions: 31.

| Function | Direction |
| --- | --- |
| `intervention_rates.expected_demand_per_well` | code → pack: absent from section 2 |
| `intervention_rates.expected_events` | code → pack: absent from section 2 |
| `intervention_rates.resolve_access_multiplier` | code → pack: absent from section 2 |
| `intervention_rates.build_intervention_types` | code → pack: absent from section 2 |
| `lifecycle_summary.run_lifecycle` | code → pack: absent from section 2 |
| `lifecycle_summary.compare_architectures` | code → pack: absent from section 2 |
| `models.AccessClass` | code → pack: absent from section 2 |
| `models.AgeBandRate` | code → pack: absent from section 2 |
| `models.InterventionType` | code → pack: absent from section 2 |
| `models.ResourcePool` | code → pack: absent from section 2 |
| `models.WellProgram` | code → pack: absent from section 2 |
| `resource_queue.compute_resource_demand` | code → pack: absent from section 2 |
| `uptime_rollup.rollup_uptime` | code → pack: absent from section 2 |
| `wed_calibration.BandDemand` | code → pack: absent from section 2 |
| `wed_calibration.FleetSupply` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.load` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.from_dict` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.band` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.as_of` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.provenance` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.empirical_demand_summary` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.heavy_pool_available_days_per_year` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.calibrated_resource_pools` | code → pack: absent from section 2 |
| `wed_calibration.WedCalibration.access_multiplier_from_supply_ratio` | code → pack: absent from section 2 |
| `wed_calibration.load_wed_calibration` | code → pack: absent from section 2 |

| Locator | Direction |
| --- | --- |
| None | No invalid locator detected |

## Existing node-ID collection limitations

The repository-wide collector returned 43 passes and 58 failures. The three new packs passed; every failed pack below was already present at the pin. Its pack bytes and every cited test-file byte were read with `git show origin/main:<path>` and matched the working tree, or both sides lacked the cited file. Collection failures include absent dependencies and existing stale citations. No second checkout was executed. Main engine import evidence is `src/digitalmodel/engine.py:10`; the unchanged foam routing test imports it at `tests/fire_safety/test_foam_system_sizing.py:371`. The isolated environment lacks `loguru` required by the external utility dependency.

| Pack | Collection failure / follow-up |
| --- | --- |
| `src/digitalmodel/alt_fuel_ship_sizing/docs/agent-pack.md` | Missing dependencies: `jinja2` |
| `src/digitalmodel/asset_integrity/assessment/docs/agent-pack.md` | Missing dependencies: `loguru`, `pint` |
| `src/digitalmodel/asset_integrity/common/docs/agent-pack.md` | Missing dependencies: `pint` |
| `src/digitalmodel/asset_integrity/docs/agent-pack.md` | Missing dependencies: `pint` |
| `src/digitalmodel/cathodic_protection/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/code_checks/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/data_systems/data_procurement/docs/agent-pack.md` | Missing dependencies: `requests` |
| `src/digitalmodel/drilling_riser/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/drilling_riser/stackup_drawing/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/fatigue/docs/agent-pack.md` | Missing dependencies: `jsonschema`, `pylife` |
| `src/digitalmodel/field_development/schematics/docs/agent-pack.md` | Missing dependencies: `matplotlib` |
| `src/digitalmodel/floating_wind/docs/agent-pack.md` | Missing dependencies: `jinja2`, `loguru` |
| `src/digitalmodel/geotechnical/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/hull_girder_screening/docs/agent-pack.md` | Missing dependencies: `jinja2` |
| `src/digitalmodel/hydrodynamics/aqwa/docs/agent-pack.md` | Missing dependencies: `webcolors` |
| `src/digitalmodel/hydrodynamics/bemrosetta/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/hydrodynamics/diffraction/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/hydrodynamics/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/hydrodynamics/hull_library/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/hydrodynamics/parametric_hull_analysis/docs/agent-pack.md` | Missing dependencies: `matplotlib` |
| `src/digitalmodel/infrastructure/base_solvers/docs/agent-pack.md` | Missing dependencies: `deepdiff`, `loguru` |
| `src/digitalmodel/marine_ops/artificial_lift/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/marine_ops/marine_analysis/docs/agent-pack.md` | Missing dependencies: `matplotlib` |
| `src/digitalmodel/marine_ops/marine_engineering/docs/agent-pack.md` | Missing dependencies: `loguru`, `matplotlib` |
| `src/digitalmodel/marine_ops/reservoir/docs/agent-pack.md` | Missing dependencies: `matplotlib` |
| `src/digitalmodel/materials/docs/agent-pack.md` | Missing dependencies: `pint` |
| `src/digitalmodel/orcaflex/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/pipelay/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/rao_spectral_fatigue/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/riser_fatigue/docs/agent-pack.md` | Missing dependencies: `loguru`, `pylife` |
| `src/digitalmodel/signal_processing/signal_analysis/docs/agent-pack.md` | Missing dependencies: `pylife` |
| `src/digitalmodel/solvers/blender_automation/docs/agent-pack.md` | Missing dependencies: `digitalmodel.blender_automation` |
| `src/digitalmodel/solvers/calculix/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/ghs/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/gmsh_meshing/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/mystran/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/openfoam/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/format_converter/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/model_generator/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/modular_input_validation/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/mooring_analysis/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/results_analysis/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcaflex/universal/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/solvers/orcawave/docs/agent-pack.md` | Missing dependencies: `h5py` |
| `src/digitalmodel/structural/docs/agent-pack.md` | Missing dependencies: `pylife` |
| `src/digitalmodel/structural/fatigue/docs/agent-pack.md` | Missing dependencies: `pylife` |
| `src/digitalmodel/structural/fatigue_apps/docs/agent-pack.md` | Missing dependencies: `matplotlib` |
| `src/digitalmodel/structural/jacket_topside/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/structural/pipe_capacity/docs/agent-pack.md` | Missing dependencies: `loguru`, `pint` |
| `src/digitalmodel/subsea/pipeline/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/subsea/viv_analysis/docs/agent-pack.md` | Missing dependencies: `deepdiff`, `loguru` |
| `src/digitalmodel/synthetic_rope_mooring_fatigue/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/viv_screening/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/weather_window/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/well/docs/agent-pack.md` | Missing dependencies: `loguru` |
| `src/digitalmodel/well/drilling/docs/agent-pack.md` | Missing dependencies: `loguru` |
