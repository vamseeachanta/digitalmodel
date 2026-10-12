"""Platform gate for ANSYS tests that need the licensed Windows host (owner decision M05).

The listed tests exercise hash-bound ANSYS host modules that call Windows-only or
Python 3.12-only APIs. Those modules are pinned by digest in retained evidence and
are not edited. Off Windows the tests are skipped with the reason below; on Windows
(the licensed ANSYS host) they run unchanged. The list is exact: it is the set of
test functions that failed on the Linux Python 3.11 CI lane (tests-structural) at
PR #2114 head e5cbd6d7, by function name. Every other test in these modules still
runs on Linux. tests/ansys/test_windows_host_gate.py keeps the list honest.
"""
import sys
from pathlib import Path

import pytest

IS_JUNCTION = ('Path.is_junction is Python 3.12+; the hash-bound module refuses Windows '
               'directory junctions on the licensed Windows ANSYS host')
JUNCTION_GUARDED_PATH = ('outcome depends on a Windows junction check (Path.is_junction, '
                         'Python 3.12+) inside the hash-bound host code path')
CREATE_NO_WINDOW = 'subprocess.CREATE_NO_WINDOW is Windows-only (licensed ANSYS host)'
ST_FILE_ATTRIBUTES = 'os.stat_result.st_file_attributes is Windows-only (licensed ANSYS host)'
PPID_MAP = 'the process parent-map backend is Windows-only (licensed ANSYS host)'
WINDOWS_HOST_SEMANTICS = ('Windows-only host semantics: the entry point requires os.name == "nt" or the '
                          'interpreter alias must be an NTFS junction (licensed ANSYS host)')

WINDOWS_HOST_ONLY = {
    'test_cylinder_absence_collection.py': {
        'test_targeted_powershell_output_failures_retain_evidence': CREATE_NO_WINDOW,
        'test_targeted_powershell_runner_retains_exact_evidence': CREATE_NO_WINDOW,
        'test_targeted_powershell_timeout_refuses': CREATE_NO_WINDOW,
    },
    'test_cylinder_absence_population_integration.py': {
        'test_powershell_identity_requires_regular_executable_and_records_digest': ST_FILE_ATTRIBUTES,
    },
    'test_cylinder_canary.py': {
        'test_approval_cannot_run_four_more_in_another_directory': JUNCTION_GUARDED_PATH,
        'test_capture_allowance_exceeded_stops_and_preserves_bytes': JUNCTION_GUARDED_PATH,
        'test_failed_control_stops_but_coarse_diagnostics_continue': JUNCTION_GUARDED_PATH,
        'test_failed_final_adjudication_cannot_be_pass': JUNCTION_GUARDED_PATH,
        'test_four_attempts_are_journaled_before_launch_and_no_repeat': JUNCTION_GUARDED_PATH,
        'test_receipt_writes_call_fsync': JUNCTION_GUARDED_PATH,
        'test_runner_wall_duration_is_retained_as_decimal_text': JUNCTION_GUARDED_PATH,
    },
    'test_cylinder_canary_boundaries.py': {
        'test_assessment_cannot_mutate_retained_records': JUNCTION_GUARDED_PATH,
        'test_noncanonical_callback_retains_incomplete_receipt': JUNCTION_GUARDED_PATH,
    },
    'test_cylinder_console_preflight.py': {
        'test_console_disagreement_refuses_before_licence': IS_JUNCTION,
        'test_console_owner_disagreement_on_recheck_refuses': IS_JUNCTION,
        'test_operation_supplement_requires_v2_binding': IS_JUNCTION,
    },
    'test_cylinder_diagnostic_admission.py': {
        'test_adjudication_cannot_fabricate_engineering_proof': IS_JUNCTION,
        'test_approval_cannot_relocate_or_reidentify_scope': IS_JUNCTION,
        'test_approval_requires_exact_typed_object': IS_JUNCTION,
        'test_changed_bound_bytes_refuse_at_factory_and_next_preflight': IS_JUNCTION,
        'test_durable_claim_stops_next_preflight_and_process_restart': IS_JUNCTION,
        'test_even_reviewed_config_cannot_expand_diagnostic_scope': IS_JUNCTION,
        'test_existing_even_partial_claim_refuses_before_runner': IS_JUNCTION,
        'test_extra_review_context_file_is_also_bound_to_current_bytes': IS_JUNCTION,
        'test_hashed_but_inadequate_provider_evidence_refuses': IS_JUNCTION,
        'test_helper_bom_content_and_raw_hash_are_distinct': IS_JUNCTION,
        'test_minor_verdict_cannot_hide_blocking_or_invalid_findings': IS_JUNCTION,
        'test_production_inventory_refuses_caller_selected_source_root': IS_JUNCTION,
        'test_real_helper_string_findings_and_transport_telemetry_supported': IS_JUNCTION,
        'test_redirected_ledger_refuses_without_claim': IS_JUNCTION,
        'test_rehashed_bundle_cannot_substitute_reviewed_file_bindings': IS_JUNCTION,
        'test_reviewed_execution_object_still_requires_fixed_typed_profile': IS_JUNCTION,
        'test_reviewed_narrowed_inventory_cannot_omit_runtime_runner_or_entrypoint': IS_JUNCTION,
        'test_top_level_operational_object_is_config_bound_not_approval_field': IS_JUNCTION,
        'test_unchanged_claimed_digest_does_not_hide_bundle_context_substitution': IS_JUNCTION,
        'test_unconsumed_scope_uses_actual_retained_review_identity': IS_JUNCTION,
        'test_whole_execution_binding_cannot_change': IS_JUNCTION,
    },
    'test_cylinder_diagnostic_driver.py': {
        'test_environment_changed_after_claim_consumes_without_native': IS_JUNCTION,
        'test_environment_changed_during_preflight_is_checked_again': IS_JUNCTION,
        'test_environment_refusal_before_launch_releases_without_consumption': IS_JUNCTION,
        'test_failed_preflight_observation_is_retained': IS_JUNCTION,
        'test_lock_changed_during_preflight_prevents_launch': IS_JUNCTION,
        'test_main_passes_pinned_operational_object_to_preflight': IS_JUNCTION,
        'test_settled_zero_releases_only_after_one_native_adapter_return': IS_JUNCTION,
        'test_uncertain_launch_retains_reservation': IS_JUNCTION,
    },
    'test_cylinder_diagnostic_integration.py': {
        'test_changed_approval_refuses_before_claim': IS_JUNCTION,
        'test_claim_survives_journal_exception_and_refuses_restart': IS_JUNCTION,
        'test_nonnumerical_launch_failure_consumes_scope_without_readmission': IS_JUNCTION,
        'test_preflight_refusal_does_not_consume_claim_or_launch': IS_JUNCTION,
        'test_valid_zero_then_durable_scope_stop_records_one_launch': IS_JUNCTION,
        'test_zero_failure_is_fail_not_planned_scope_incomplete': IS_JUNCTION,
    },
    'test_cylinder_diagnostic_preflight.py': {
        'test_before_launch_rechecks_and_never_retries': IS_JUNCTION,
        'test_historical_scope_retains_v1_binding_route': IS_JUNCTION,
        'test_long_postclaim_query_cannot_age_prior_observations_past_bound': IS_JUNCTION,
        'test_owner_reader_rejects_oversize_before_open': IS_JUNCTION,
        'test_preflight_refuses_binding_changes': IS_JUNCTION,
        'test_preflight_retains_original_license_bytes_and_scoped_observations': IS_JUNCTION,
        'test_pressure_owner_recheck_runs_in_actual_phase2': IS_JUNCTION,
        'test_pressure_scope_refuses_v1_in_nested_operational_config': IS_JUNCTION,
        'test_successful_license_stdout_does_not_override_stderr': IS_JUNCTION,
        'test_timeout_keeps_original_partial_query_bytes': IS_JUNCTION,
        'test_unknown_process_projection_cannot_enable_before_launch': IS_JUNCTION,
        'test_v2_missing_owner_evidence_refuses': IS_JUNCTION,
        'test_v2_owner_resolution_repeats_and_retains_evidence': IS_JUNCTION,
    },
    'test_cylinder_forwarder_resources.py': {
        'test_real_filesystem_without_required_junction_refuses': WINDOWS_HOST_SEMANTICS,
    },
    'test_cylinder_intermediate_admission.py': {
        'test_intermediate_factory_checks_history_after_review': IS_JUNCTION,
        'test_intermediate_preflight_refuses_missing_version_two': IS_JUNCTION,
        'test_new_schema_required_for_intermediate': IS_JUNCTION,
        'test_unlabelled_findings_refuse': IS_JUNCTION,
    },
    'test_cylinder_intermediate_driver.py': {
        'test_both_ordinal_ledger_sets_only': IS_JUNCTION,
        'test_coarse_pins_join_fast_binding_and_detect_change': IS_JUNCTION,
    },
    'test_cylinder_intermediate_journal_storage.py': {
        'test_any_intermediate_residue_refuses': IS_JUNCTION,
        'test_coarse_terminal_retains_existing_failure_reporting_semantics': IS_JUNCTION,
        'test_default_coarse_behavior_unchanged': IS_JUNCTION,
        'test_failed_claim_write_consumes_and_refuses_restart': IS_JUNCTION,
        'test_intermediate_names_and_no_restart': IS_JUNCTION,
        'test_predecessor_descriptor_is_copied': IS_JUNCTION,
        'test_predecessor_rechecked_each_transition': IS_JUNCTION,
        'test_predecessor_refusal': IS_JUNCTION,
    },
    'test_cylinder_intermediate_resume.py': {
        'test_changed_frozen_case_order_refuses_before_launch': IS_JUNCTION,
        'test_changed_predecessor_after_consumption_retains_outcome': IS_JUNCTION,
        'test_combined_capture_failures_have_unique_reasons': IS_JUNCTION,
        'test_failed_phase2_retains_last_preflight_observation': IS_JUNCTION,
        'test_intermediate_capture_contract_mismatch_is_incomplete': IS_JUNCTION,
        'test_intermediate_uses_third_claim_and_exact_case': IS_JUNCTION,
        'test_outcome_write_failure_still_returns_failure_record': IS_JUNCTION,
        'test_postclaim_failure_never_reopens_attempt': IS_JUNCTION,
        'test_preclaim_failure_leaves_two_consumed': IS_JUNCTION,
    },
    'test_cylinder_operational_reservation.py': {
        'test_acquisition_is_compatible_with_existing_pid_lock': IS_JUNCTION,
        'test_busy_lock_is_not_adopted_or_rewritten': IS_JUNCTION,
        'test_evidence_binds_current_pid_file_identity_and_raw_bytes': IS_JUNCTION,
        'test_evidence_refuses_lost_owner_identity': IS_JUNCTION,
        'test_exception_after_launch_does_not_implicitly_release': IS_JUNCTION,
        'test_missing_owner_file_is_not_reported_as_successful_release': IS_JUNCTION,
        'test_old_handle_cannot_delete_successor_reservation': IS_JUNCTION,
        'test_prelaunch_exception_can_release_when_no_owned_process_exists': IS_JUNCTION,
        'test_replaced_file_with_identical_pid_is_not_owned': IS_JUNCTION,
        'test_replaced_owner_bytes_are_never_deleted': IS_JUNCTION,
        'test_same_pid_existing_lock_is_still_busy': IS_JUNCTION,
        'test_settlement_flag_requires_explicit_boolean': IS_JUNCTION,
        'test_uncertain_settlement_retains_owner_then_settlement_releases': IS_JUNCTION,
    },
    'test_cylinder_parent_map.py': {
        'test_backend_error_refuses': PPID_MAP,
        'test_backend_once_returns_independent_copy': PPID_MAP,
        'test_dependency_identity_refuses_before_backend': PPID_MAP,
        'test_invalid_map_refuses': PPID_MAP,
        'test_non_windows_dependency_refuses': PPID_MAP,
        'test_transitive_dependency_digest_refuses': PPID_MAP,
    },
    'test_cylinder_parent_refusal_evidence.py': {
        'test_disappearance_uses_validated_nonempty_parent_tables': PPID_MAP,
    },
    'test_cylinder_preclaim_probe.py': {
        'test_measured_probe_threshold_precedes_all_ledger_records': IS_JUNCTION,
        'test_observation_staleness_after_probe_refuses_without_invocation': IS_JUNCTION,
        'test_preparation_receipt_failure_is_explicit': IS_JUNCTION,
        'test_probe_and_postclaim_evidence_are_separate': IS_JUNCTION,
        'test_probe_cannot_refresh_observation_authority': IS_JUNCTION,
        'test_probe_error_retains_release_and_fresh_output_can_be_prepared': IS_JUNCTION,
        'test_probe_storage_time_is_measured': IS_JUNCTION,
        'test_refusal_does_not_claim_unverified_stream_binding': IS_JUNCTION,
        'test_slow_claim_write_still_stops_before_native': IS_JUNCTION,
    },
    'test_cylinder_preparation_driver.py': {
        'test_main_checks_streams_before_seat_acquisition': WINDOWS_HOST_SEMANTICS,
    },
    'test_cylinder_preparation_history.py': {
        'test_case_variant_orphan_stream_is_detected': IS_JUNCTION,
        'test_complete_history_returns_storage_declarations': IS_JUNCTION,
        'test_incomplete_or_unsettled_history_refuses': IS_JUNCTION,
        'test_incomplete_terminal_evidence_refuses': IS_JUNCTION,
        'test_membership_is_rechecked_after_initial_validation': IS_JUNCTION,
        'test_orphan_streams_require_history_even_without_preparation_roots': IS_JUNCTION,
        'test_oversized_metadata_is_refused_before_read': IS_JUNCTION,
        'test_pinned_hardlink_is_refused': IS_JUNCTION,
        'test_replica_stream_directory_cannot_replace_originals': IS_JUNCTION,
        'test_transplanted_refusal_does_not_match_configuration': IS_JUNCTION,
        'test_unrecognized_legacy_refusal_is_rejected': IS_JUNCTION,
    },
    'test_cylinder_pressure_absence_integration.py': {
        'test_absence_does_not_accept_owner_or_wrapper_exemption': IS_JUNCTION,
        'test_collection_refusal_retains_original_query_evidence': IS_JUNCTION,
        'test_independent_verifier_rejects_competitor_only_in_history': IS_JUNCTION,
        'test_orphan_fluid_solver_refuses_at_each_observation': IS_JUNCTION,
        'test_preclaim_probe_has_distinct_stage_and_preserves_observation_times': IS_JUNCTION,
        'test_three_fresh_empty_observations': IS_JUNCTION,
    },
    'test_cylinder_pressure_admission.py': {
        'test_approval_cannot_be_modified': IS_JUNCTION,
        'test_baseline_bytes_mutated_after_factory_refuse': IS_JUNCTION,
        'test_blocking_review_precedes_lineage_work': IS_JUNCTION,
        'test_bracketed_prose_reference_is_not_an_unknown_severity': IS_JUNCTION,
        'test_changed_approval_is_rejected_even_with_reviewed_repin': IS_JUNCTION,
        'test_changed_successor_artifact_is_rejected': IS_JUNCTION,
        'test_complete_original_inventory_required': IS_JUNCTION,
        'test_entrypoint_cannot_be_omitted': IS_JUNCTION,
        'test_every_reviewed_source_must_match_committed_blob': IS_JUNCTION,
        'test_explicit_and_unknown_severity_labels_refuse': IS_JUNCTION,
        'test_missing_original_parent_claim_refuses': IS_JUNCTION,
        'test_negated_or_historical_prose_is_not_a_blocking_label': IS_JUNCTION,
        'test_nonblocking_verdict_cannot_hide_blocking_finding': IS_JUNCTION,
        'test_offline_authority_revalidates_without_claims': IS_JUNCTION,
        'test_operator_cannot_be_independent_reviewer': IS_JUNCTION,
        'test_parent_claim_relocated_refuses': IS_JUNCTION,
        'test_required_inventory_includes_parent_compatibility_module': IS_JUNCTION,
        'test_required_inventory_uses_resolved_module_path': IS_JUNCTION,
        'test_review_transport_session_mutation_refuses': IS_JUNCTION,
        'test_reviewed_repin_cannot_substitute_historical_baseline': IS_JUNCTION,
        'test_scope_cannot_expand_even_under_new_review': IS_JUNCTION,
        'test_source_mutation_after_factory_refuses': IS_JUNCTION,
        'test_source_revision_requires_full_commit_identifier': IS_JUNCTION,
        'test_successor_stale_runtime_inventory_refuses': IS_JUNCTION,
        'test_unknown_explicit_labels_are_case_insensitive': IS_JUNCTION,
    },
    'test_cylinder_pressure_journal.py': {
        'test_alternate_claim_name_refuses': IS_JUNCTION,
        'test_changed_parent_refuses_successor': IS_JUNCTION,
        'test_concurrent_invocation_writers_have_one_winner': IS_JUNCTION,
        'test_exclusive_records_retain_original_and_refuse_restart': IS_JUNCTION,
        'test_partial_invocation_refuses_without_replacement': IS_JUNCTION,
        'test_partial_successor_consumes_even_when_writer_fails': IS_JUNCTION,
    },
    'test_cylinder_pressure_resume.py': {
        'test_360_second_launch_and_five_second_phase2_boundaries': IS_JUNCTION,
        'test_365_second_preclaim_boundary': IS_JUNCTION,
        'test_crash_after_each_durable_write_refuses_restart': IS_JUNCTION,
        'test_duplicate_prefix_check_is_rejected': IS_JUNCTION,
        'test_entire_preclaim_interval_counts_against_freshness': IS_JUNCTION,
        'test_execution_failure_and_deadline_remain_distinct': IS_JUNCTION,
        'test_existing_or_partial_record_never_overwritten': IS_JUNCTION,
        'test_final_storage_can_refuse_after_capture': IS_JUNCTION,
        'test_final_storage_observed_after_partial_failure': IS_JUNCTION,
        'test_foreign_prefix_response_is_rejected': IS_JUNCTION,
        'test_launch_exception_does_not_invent_native_completion': IS_JUNCTION,
        'test_one_n4_after_verified_zero_and_no_numerical_acceptance': IS_JUNCTION,
        'test_postclaim_failure_consumes_without_readmission': IS_JUNCTION,
        'test_preclaim_refusal_never_launches_or_creates_successor': IS_JUNCTION,
        'test_prefix_requires_concrete_64_zero_checks': IS_JUNCTION,
        'test_real_criterion_receipt_key_shape_is_accepted': IS_JUNCTION,
        'test_restart_cannot_change_output_or_campaign_to_readmit': IS_JUNCTION,
        'test_retention_precedes_deadline_disposition': IS_JUNCTION,
        'test_stale_preclaim_observation_refuses': IS_JUNCTION,
        'test_storage_failure_status_is_not_accepted': IS_JUNCTION,
        'test_unknown_pressure_format_retention_is_not_solver_failure': IS_JUNCTION,
        'test_unsettled_owned_process_never_releases_reservation': IS_JUNCTION,
    },
    'test_cylinder_runtime_bundle.py': {
        'test_copy_time_drift_preserves_partial_without_manifest': IS_JUNCTION,
        'test_existing_destination_never_overwritten': IS_JUNCTION,
        'test_final_copy_drift_prevents_manifest_publication': IS_JUNCTION,
        'test_inventory_hash_mutation_has_explicit_refusal': IS_JUNCTION,
        'test_malformed_original_refuses': IS_JUNCTION,
        'test_output_root_redirection_after_first_copy_refuses': IS_JUNCTION,
        'test_pending_alias_unlink_failure_preserves_published_bundle': IS_JUNCTION,
        'test_publication_never_replaces_existing_manifest': IS_JUNCTION,
        'test_redirected_ancestor_refuses_without_copy': IS_JUNCTION,
        'test_source_revision_bytes_must_match': IS_JUNCTION,
        'test_success_preserves_original_and_all_artifacts': IS_JUNCTION,
        'test_wrong_pins_refuse': IS_JUNCTION,
    },
    'test_cylinder_workflow.py': {
        'test_real_operator_adapter_validator_and_criteria_four_case_pass': IS_JUNCTION,
    },
    'test_run_pressure_diagnostic.py': {
        'test_bound_config_mismatch_refuses_before_admission': IS_JUNCTION,
        'test_bound_inputs_calls_real_factory_interface_with_external_pins': IS_JUNCTION,
        'test_fast_checker_binds_sources_config_and_approval_without_git': IS_JUNCTION,
        'test_ledger_names_are_parent_linked_and_only_existing': IS_JUNCTION,
        'test_main_does_not_duplicate_resume_owned_early_release': WINDOWS_HOST_SEMANTICS,
        'test_replay_calls_actual_derivation_with_pinned_dictionary': IS_JUNCTION,
        'test_replay_changed_pin_refuses_before_derivation': IS_JUNCTION,
    },
}


def windows_host_reason(module: str, function: str):
    """Return the skip reason for a gated test, or None if it is not gated."""
    reason = WINDOWS_HOST_ONLY.get(module, {}).get(function)
    return None if reason is None else f'requires the licensed Windows ANSYS host: {reason}'


def pytest_collection_modifyitems(config, items):
    if sys.platform == 'win32':
        return
    here = Path(__file__).resolve().parent
    for item in items:
        if Path(item.path).resolve().parent != here:
            continue
        name = getattr(item, 'originalname', None) or item.name.split('[')[0]
        reason = windows_host_reason(item.path.name, name)
        if reason is not None:
            item.add_marker(pytest.mark.skip(reason=reason))
