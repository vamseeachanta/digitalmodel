---
description: Enhanced Task Execution Rules for Agent OS
globs:
alwaysApply: false
version: 2.0
encoding: UTF-8
---

# Enhanced Task Execution Rules

<ai_meta>
  <parsing_rules>
    - Process XML blocks first for structured data
    - Execute instructions in sequential order
    - Use templates as exact patterns
    - Request missing data rather than assuming
    - Support both traditional and enhanced spec workflows
  </parsing_rules>
  <file_conventions>
    - encoding: UTF-8
    - line_endings: LF
    - indent: 2 spaces
    - markdown_headers: no indentation
  </file_conventions>
</ai_meta>

## Overview

<purpose>
  - Execute spec tasks with enhanced tracking and documentation
  - Support task summary completion for enhanced specs
  - Integrate with enhanced documentation system
  - Maintain full compatibility with existing workflows
  - Provide comprehensive implementation tracking
</purpose>

<context>
  - Enhanced version of Agent OS framework
  - Executed after spec planning is complete
  - Follows tasks defined in spec tasks.md
  - Integrates with cross-repository shared components
  - Supports both traditional and module-based organization
</context>

<prerequisites>
  - Spec documentation exists in specs/ or specs/modules/
  - Tasks defined in spec's tasks.md
  - Development environment configured
  - Git repository initialized
  - Optional: Cross-repository configuration for shared components
  - Optional: Enhanced spec documentation for task summaries
</prerequisites>

## Enhanced Features

### Task Summary Completion
- Automatically populate task_summary.md for enhanced specs
- Track implementation details and technical decisions
- Document testing and validation results
- Record performance metrics and benchmarks
- Capture lessons learned and future improvements

### Enhanced Progress Tracking
- Real-time task status updates with timestamps
- Cross-reference validation during execution
- Performance metrics collection
- Shared component integration verification
- Automated documentation updates

### Cross-Repository Integration
- Validate shared component accessibility
- Check version compatibility during execution
- Handle offline fallback scenarios
- Update component cache when necessary
- Document integration points and dependencies

### Visual Documentation Validation
- Verify mermaid diagram rendering
- Validate system architecture accuracy
- Update diagrams based on implementation changes
- Generate implementation-specific visualizations

## Enhanced Process Flow

<process_flow>

<step number="1" name="enhanced_task_assignment">

### Step 1: Enhanced Task Assignment

<step_metadata>
  <inputs>
    - spec_reference: file path to spec or spec directory
    - specific_tasks: array[string] (optional)
    - execution_mode: string (traditional|enhanced)
  </inputs>
  <default>next uncompleted parent task</default>
  <enhanced_detection>automatic based on spec structure</enhanced_detection>
</step_metadata>

<enhanced_task_selection>
  <spec_detection>
    1. CHECK if spec is in specs/modules/ (module-based)
    2. CHECK if spec.md contains enhanced sections (prompt_summary, executive_summary)
    3. CHECK if task_summary.md template exists
    4. DETERMINE execution mode based on spec structure
  </spec_detection>
  
  <task_identification>
    <explicit>user specifies exact task(s) to execute</explicit>
    <implicit>find next uncompleted task in tasks.md</implicit>
    <intelligent>analyze dependencies and suggest optimal execution order</intelligent>
  </task_identification>
  
  <enhanced_context>
    1. LOAD spec documentation and sub-specs
    2. IDENTIFY cross-repository dependencies
    3. CHECK shared component versions
    4. ANALYZE task complexity and resource requirements
  </enhanced_context>
</enhanced_task_selection>

<instructions>
  ACTION: Identify task(s) to execute with enhanced context awareness
  DEFAULT: Select next uncompleted parent task if not specified
  DETECT: Enhanced spec features and adjust execution accordingly
  CONFIRM: Task selection and execution mode with user
</instructions>

</step>

<step number="2" name="enhanced_context_analysis">

### Step 2: Enhanced Context Analysis

<step_metadata>
  <reads>
    - spec SRD file and all sub-specs
    - spec tasks.md
    - task_summary.md (if exists)
    - cross-repo-config.yaml (if exists)
    - @.agent-os/product/mission.md
    - @.agent-os/product/tech-stack.md
  </reads>
  <purpose>comprehensive understanding of requirements and context</purpose>
</step_metadata>

<enhanced_context_gathering>
  <spec_level>
    - requirements from SRD and sub-specs
    - technical specifications and constraints
    - test specifications and coverage requirements
    - API specifications and endpoint definitions
    - database schema changes and migrations
  </spec_level>
  
  <cross_repo_level>
    - shared component dependencies and versions
    - hub repository accessibility and status
    - cached component availability and freshness
    - version compatibility matrix validation
  </cross_repo_level>
  
  <product_level>
    - overall mission alignment and business goals
    - technical standards and coding conventions
    - best practices and architectural patterns
    - performance requirements and constraints
  </product_level>
  
  <execution_level>
    - previous task completion status and results
    - implementation decisions and technical debt
    - testing results and coverage metrics
    - performance benchmarks and optimization notes
  </execution_level>
</enhanced_context_gathering>

<context_validation>
  <shared_components>
    1. VALIDATE all cross-repository references are accessible
    2. CHECK version compatibility with requirements
    3. UPDATE component cache if necessary
    4. PREPARE fallback strategies for offline scenarios
  </shared_components>
  
  <dependencies>
    1. ANALYZE task dependencies and prerequisites
    2. VALIDATE external service availability
    3. CHECK development environment setup
    4. ENSURE required tools and libraries are available
  </dependencies>
</context_validation>

<instructions>
  ACTION: Read and analyze all relevant documentation and context
  VALIDATE: Cross-repository dependencies and shared components
  UNDERSTAND: How current task fits into overall spec and product goals
  PREPARE: Execution environment and dependency resolution
</instructions>

</step>

<step number="3" name="enhanced_implementation_planning">

### Step 3: Enhanced Implementation Planning

<step_metadata>
  <creates>comprehensive execution plan</creates>
  <requires>user approval for complex changes</requires>
  <integrates>cross-repository components and shared resources</integrates>
</step_metadata>

<enhanced_planning_structure>
  <format>numbered list with sub-bullets and resource allocation</format>
  <includes>
    - all subtasks from tasks.md with time estimates
    - implementation approach with alternatives considered
    - shared component integration points
    - testing strategy with coverage targets
    - performance benchmarks and validation criteria
    - documentation update requirements
  </includes>
</enhanced_planning_structure>

<enhanced_plan_template>
  ## Enhanced Implementation Plan for [TASK_NAME]

  ### Execution Overview
  - **Estimated Duration:** [TIME_ESTIMATE]
  - **Complexity Level:** [LOW|MEDIUM|HIGH]
  - **Shared Components:** [LIST_OF_COMPONENTS]
  - **Testing Strategy:** [TDD|BDD|INTEGRATION_FIRST]

  ### Implementation Steps

  1. **[MAJOR_STEP_1]** (Estimated: [TIME])
     - [SPECIFIC_ACTION_WITH_DETAILS]
     - [SPECIFIC_ACTION_WITH_DETAILS]
     - **Validation Criteria:** [SUCCESS_METRICS]

  2. **[MAJOR_STEP_2]** (Estimated: [TIME])
     - [SPECIFIC_ACTION_WITH_DETAILS]
     - [SPECIFIC_ACTION_WITH_DETAILS]
     - **Cross-Repository Integration:** [COMPONENT_USAGE]

  ### Shared Component Integration
  - **[COMPONENT_NAME]** - [PURPOSE_AND_INTEGRATION_DETAILS]
  - **Version Requirements:** [VERSION_CONSTRAINTS]
  - **Fallback Strategy:** [OFFLINE_ALTERNATIVE]

  ### Testing Strategy
  - **Unit Tests:** [COVERAGE_TARGET] coverage
  - **Integration Tests:** [INTEGRATION_POINTS]
  - **Performance Tests:** [BENCHMARK_TARGETS]
  - **Cross-Repository Tests:** [SHARED_COMPONENT_VALIDATION]

  ### Documentation Updates
  - **Task Summary:** Real-time updates to task_summary.md
  - **Technical Decisions:** Document architecture choices
  - **Performance Metrics:** Record benchmark results
  - **Cross-References:** Update spec documentation

  ### Risk Mitigation
  - **Technical Risks:** [IDENTIFIED_RISKS_AND_MITIGATION]
  - **Dependency Risks:** [EXTERNAL_DEPENDENCY_FALLBACKS]
  - **Timeline Risks:** [SCHEDULE_CONTINGENCIES]
</enhanced_plan_template>

<approval_process>
  <simple_tasks>
    <criteria>single subtask, no shared components, low complexity</criteria>
    <process>present brief plan, proceed with implicit approval</process>
  </simple_tasks>
  
  <complex_tasks>
    <criteria>multiple subtasks, shared components, high complexity</criteria>
    <process>present detailed plan, wait for explicit approval</process>
  </complex_tasks>
  
  <cross_repo_tasks>
    <criteria>involves shared components or cross-repository changes</criteria>
    <process>present integration plan, validate component access, explicit approval</process>
  </cross_repo_tasks>
</approval_process>

<instructions>
  ACTION: Create comprehensive execution plan based on task complexity
  INCLUDE: All implementation details, testing strategy, and documentation
  INTEGRATE: Shared component usage and cross-repository dependencies
  REQUEST: Appropriate level of approval based on task complexity
</instructions>

</step>

<step number="4-6" name="enhanced_execution_preparation">

### Steps 4-6: Enhanced Execution Preparation

<development_server_management>
  <enhanced_checking>
    1. CHECK for running development servers (all common ports)
    2. IDENTIFY server types (web, API, database, cache)
    3. DETERMINE if shutdown is necessary for current task
    4. COORDINATE with user for minimal disruption
  </enhanced_checking>
</development_server_management>

<enhanced_git_management>
  <branch_strategy>
    <module_based_specs>
      <pattern>module-name/spec-name</pattern>
      <example>authentication/enhanced-auth-system</example>
    </module_based_specs>
    
    <traditional_specs>
      <pattern>spec-name</pattern>
      <example>password-reset-flow</example>
    </traditional_specs>
  </branch_strategy>
  
  <shared_component_tracking>
    1. CREATE branch with appropriate naming
    2. DOCUMENT shared component versions in commit messages
    3. TAG commits with cross-repository integration points
    4. TRACK component updates and compatibility changes
  </shared_component_tracking>
</enhanced_git_management>

<shared_component_preparation>
  <accessibility_validation>
    1. VERIFY all shared components are accessible
    2. CHECK version compatibility with current requirements
    3. UPDATE local cache if components are stale
    4. PREPARE fallback implementations if needed
  </accessibility_validation>
  
  <integration_setup>
    1. CONFIGURE import paths for shared components
    2. SET UP development environment for cross-repository work
    3. VALIDATE shared component functionality in local context
    4. PREPARE testing infrastructure for integrated components
  </integration_setup>
</shared_component_preparation>

</step>

<step number="7" name="enhanced_development_execution">

### Step 7: Enhanced Development Execution

<step_metadata>
  <follows>approved implementation plan with enhanced tracking</follows>
  <integrates>shared components and cross-repository resources</integrates>
  <documents>real-time progress and decisions</documents>
</step_metadata>

<enhanced_execution_standards>
  <follow_exactly>
    - approved implementation plan with time tracking
    - spec specifications including visual documentation
    - @.agent-os/product/code-style.md
    - @.agent-os/product/dev-best-practices.md
    - shared component integration patterns
  </follow_exactly>
  
  <approach>enhanced test-driven development (TDD+)</approach>
  <documentation>real-time task summary updates</documentation>
  <performance>benchmark and metrics collection</performance>
</enhanced_execution_standards>

<enhanced_tdd_workflow>
  <traditional_tdd>
    1. Write failing tests first
    2. Implement minimal code to pass
    3. Refactor while keeping tests green
    4. Repeat for each feature
  </traditional_tdd>
  
  <enhanced_additions>
    5. DOCUMENT technical decisions in task_summary.md
    6. BENCHMARK performance against requirements
    7. VALIDATE shared component integration
    8. UPDATE cross-references and documentation
    9. VERIFY visual documentation accuracy
  </enhanced_additions>
</enhanced_tdd_workflow>

<real_time_documentation>
  <task_summary_updates>
    <trigger>after each major milestone or decision</trigger>
    <content>
      - implementation approach taken
      - technical decisions and rationale
      - performance metrics achieved
      - integration challenges and solutions
      - testing results and coverage
    </content>
  </task_summary_updates>
  
  <cross_reference_updates>
    <trigger>when adding dependencies or integration points</trigger>
    <content>
      - new shared component usage
      - internal spec dependencies
      - external resource references
      - architecture diagram updates
    </content>
  </cross_reference_updates>
</real_time_documentation>

<performance_tracking>
  <metrics_collection>
    - execution time for each major task
    - code quality metrics (complexity, coverage)
    - performance benchmarks (response time, throughput)
    - shared component integration efficiency
  </metrics_collection>
  
  <continuous_validation>
    - run performance tests during development
    - validate against specification requirements
    - ensure no regression in existing functionality
    - verify cross-repository integration stability
  </continuous_validation>
</performance_tracking>

<instructions>
  ACTION: Execute development plan with enhanced tracking and documentation
  FOLLOW: All coding standards and specifications rigorously
  IMPLEMENT: Enhanced TDD approach with continuous documentation
  TRACK: Performance metrics and integration points throughout
  UPDATE: Task summary and cross-references in real-time
</instructions>

</step>

<step number="8" name="enhanced_task_status_management">

### Step 8: Enhanced Task Status Management

<step_metadata>
  <updates>tasks.md with enhanced status tracking</updates>
  <creates>task_summary.md updates with implementation details</creates>
  <timing>immediately after completion with comprehensive documentation</timing>
</step_metadata>

<enhanced_status_tracking>
  <status_format>
    <completed>- [x] Task description (Completed: YYYY-MM-DD HH:MM)</completed>
    <in_progress>- [⚠️] Task description (Started: YYYY-MM-DD HH:MM, Estimated completion: HH:MM)</in_progress>
    <blocked>- [🚫] Task description ⚠️ Blocking issue: [DESCRIPTION] (Since: YYYY-MM-DD HH:MM)</blocked>
    <skipped>- [➖] Task description (Skipped: REASON)</skipped>
  </status_format>
  
  <metadata_tracking>
    - start and completion timestamps
    - estimated vs actual duration
    - complexity assessment (actual vs planned)  
    - resource utilization metrics
    - shared component integration notes
  </metadata_tracking>
</enhanced_status_tracking>

<task_summary_integration>
  <enhanced_specs_only>
    <trigger>task completion in enhanced spec workflows</trigger>
    <content>
      ## Implementation Details
      
      ### Task: [TASK_NAME]
      **Completed:** [TIMESTAMP]
      **Duration:** [ACTUAL_TIME] (Estimated: [PLANNED_TIME])
      **Approach:** [IMPLEMENTATION_APPROACH_SUMMARY]
      
      ### Technical Decisions
      - [DECISION_1]: [RATIONALE_AND_IMPLICATIONS]
      - [DECISION_2]: [RATIONALE_AND_IMPLICATIONS]
      
      ### Shared Component Integration
      - [COMPONENT_NAME]: [USAGE_DETAILS_AND_VERSION]
      - [INTEGRATION_CHALLENGES]: [SOLUTIONS_IMPLEMENTED]
      
      ### Testing Results
      **Coverage:** [PERCENTAGE]% ([LINES_COVERED]/[TOTAL_LINES])
      **Performance:** [BENCHMARK_RESULTS]
      **Integration Tests:** [PASS_COUNT]/[TOTAL_COUNT] passing
      
      ### Performance Metrics
      - **Response Time:** [MEASUREMENT] (Target: [TARGET])
      - **Memory Usage:** [MEASUREMENT] (Target: [TARGET])
      - **CPU Utilization:** [MEASUREMENT] (Target: [TARGET])
      
      ### Lessons Learned
      - [INSIGHT_1]: [DESCRIPTION_AND_FUTURE_APPLICATION]
      - [INSIGHT_2]: [DESCRIPTION_AND_FUTURE_APPLICATION]
    </content>
  </enhanced_specs_only>
  
  <traditional_specs>
    <trigger>task completion in traditional spec workflows</trigger>
    <content>basic task completion logging without detailed summary</content>
  </traditional_specs>
</task_summary_integration>

<blocking_issue_management>
  <enhanced_documentation>
    1. DOCUMENT blocking issue with full context
    2. RESEARCH attempted solutions and their results
    3. IDENTIFY required resources or information to resolve
    4. ESTIMATE impact on overall timeline
    5. CREATE follow-up tasks or alternative approaches
  </enhanced_documentation>
  
  <escalation_process>
    <criteria>blocking issue affects critical path or multiple tasks</criteria>
    <actions>
      1. DOCUMENT comprehensive problem analysis
      2. IDENTIFY subject matter experts or resources needed
      3. CREATE alternative implementation paths
      4. COMMUNICATE impact to user with options
    </actions>
  </escalation_process>
</blocking_issue_management>

<instructions>
  ACTION: Update tasks.md with enhanced status and metadata
  INTEGRATE: Task summary updates for enhanced specs
  DOCUMENT: Comprehensive implementation details and decisions
  TRACK: Performance metrics and lessons learned
  ESCALATE: Blocking issues with full context and alternatives
</instructions>

</step>

<step number="9-12" name="enhanced_completion_workflow">

### Steps 9-12: Enhanced Completion Workflow

<enhanced_testing_validation>
  <comprehensive_test_execution>
    1. VERIFY new tests pass with expected coverage
    2. RUN entire test suite including integration tests
    3. VALIDATE shared component integration tests
    4. EXECUTE performance benchmarks and validate against targets
    5. VERIFY cross-repository compatibility tests
    6. CONFIRM visual documentation rendering (mermaid diagrams)
  </comprehensive_test_execution>
  
  <test_result_documentation>
    - document test coverage improvements
    - record performance benchmark results
    - note any test failures and their resolution
    - validate shared component integration stability
  </test_result_documentation>
</enhanced_testing_validation>

<enhanced_git_workflow>
  <comprehensive_commit_process>
    <commit_message_format>
      feat(module): implement [FEATURE_NAME]
      
      - [BULLET_POINT_SUMMARY_OF_CHANGES]
      - Shared components: [LIST_WITH_VERSIONS]
      - Performance: [BENCHMARK_IMPROVEMENTS]
      - Tests: [COVERAGE_METRICS]
      
      Closes: [TASK_REFERENCE]
      
      Co-authored-by: AssetUtilities Hub <hub@assetutilities.org>
    </commit_message_format>
    
    <enhanced_metadata>
      - include shared component versions in commit
      - tag performance improvements and benchmarks
      - reference task completion and metrics
      - document cross-repository integration points
    </enhanced_metadata>
  </comprehensive_commit_process>
  
  <pull_request_enhancement>
    <template_extension>
      ## Summary
      
      [BRIEF_DESCRIPTION_OF_CHANGES]
      
      ## Changes Made
      
      - [CHANGE_1_WITH_IMPACT_DESCRIPTION]
      - [CHANGE_2_WITH_IMPACT_DESCRIPTION]
      
      ## Shared Component Integration
      
      - **[COMPONENT_NAME]**: [VERSION] - [USAGE_DESCRIPTION]
      - **Integration Notes**: [COMPATIBILITY_AND_PERFORMANCE_NOTES]
      
      ## Performance Impact
      
      - **Benchmarks**: [PERFORMANCE_IMPROVEMENTS_OR_IMPACT]
      - **Memory Usage**: [BASELINE_VS_CURRENT]
      - **Response Time**: [BASELINE_VS_CURRENT]
      
      ## Testing
      
      - Test coverage: [PERCENTAGE]% ([INCREASE/DECREASE])
      - All tests passing ✓
      - Integration tests: [PASS_COUNT]/[TOTAL_COUNT] ✓
      - Performance tests: [BENCHMARK_RESULTS] ✓
      
      ## Visual Documentation
      
      - [x] Mermaid diagrams render correctly
      - [x] Architecture documentation updated
      - [x] Cross-references validated
    </template_extension>
  </pull_request_enhancement>
</enhanced_git_workflow>

<enhanced_completion_notification>
  <comprehensive_summary>
    ## ✅ Enhanced Task Completion Summary
    
    ### What's been implemented
    
    1. **[FEATURE_1]** - [DETAILED_DESCRIPTION_WITH_BUSINESS_VALUE]
    2. **[FEATURE_2]** - [DETAILED_DESCRIPTION_WITH_BUSINESS_VALUE]
    
    ### 🔧 Technical Implementation
    
    - **Architecture:** [IMPLEMENTATION_APPROACH]
    - **Shared Components:** [LIST_WITH_VERSIONS_AND_PURPOSES]
    - **Performance:** [BENCHMARK_RESULTS_AND_IMPROVEMENTS]
    - **Testing:** [COVERAGE_METRICS_AND_QUALITY_INDICATORS]
    
    ### 📊 Performance Metrics
    
    - **Response Time:** [MEASUREMENT] (Improvement: [DELTA])
    - **Memory Usage:** [MEASUREMENT] (Optimization: [DELTA])
    - **Test Coverage:** [PERCENTAGE]% (Increase: [DELTA])
    - **Code Quality:** [METRICS_AND_IMPROVEMENTS]
    
    ### ⚠️ Issues encountered and resolved
    
    [ONLY_IF_APPLICABLE]
    - **[ISSUE_1]** - [DESCRIPTION_SOLUTION_AND_IMPACT]
    - **[ISSUE_2]** - [DESCRIPTION_SOLUTION_AND_IMPACT]
    
    ### 🔗 Cross-Repository Integration
    
    [ONLY_IF_APPLICABLE]
    - **Shared Components Used:** [LIST_WITH_INTEGRATION_NOTES]
    - **Version Compatibility:** [STATUS_AND_NOTES]
    - **Fallback Mechanisms:** [OFFLINE_SUPPORT_STATUS]
    
    ### 👀 Ready to test in browser
    
    [ONLY_IF_APPLICABLE]
    1. [STEP_1_TO_TEST_WITH_EXPECTED_BEHAVIOR]
    2. [STEP_2_TO_TEST_WITH_EXPECTED_BEHAVIOR]
    3. [STEP_3_TO_TEST_WITH_EXPECTED_BEHAVIOR]
    
    ### 📦 Pull Request and Documentation
    
    - **PR:** [GITHUB_PR_URL]
    - **Task Summary:** Updated with implementation details
    - **Cross-References:** Validated and updated
    - **Performance Benchmarks:** Documented and archived
    
    ### 🎓 Lessons Learned
    
    - [INSIGHT_1_FOR_FUTURE_DEVELOPMENT]
    - [INSIGHT_2_FOR_TEAM_KNOWLEDGE_SHARING]
    
    ### 🚀 Next Steps
    
    [ONLY_IF_APPLICABLE]
    - [RECOMMENDED_FOLLOW_UP_TASK_1]
    - [RECOMMENDED_FOLLOW_UP_TASK_2]
  </comprehensive_summary>
  
  <notification_sound>
    <command>afplay /System/Library/Sounds/Glass.aiff</command>
    <fallback>echo -e "\a"</fallback>
  </notification_sound>
</enhanced_completion_notification>

</step>

</process_flow>

## Backward Compatibility

<compatibility_assurance>
  - All existing execute-tasks workflows remain fully supported
  - Traditional task execution without task summaries continues to work
  - Standard task status updates are preserved for non-enhanced specs
  - Existing git workflow patterns and commit formats are maintained
  - No breaking changes to Agent OS framework integration
</compatibility_assurance>

<feature_detection>
  <enhanced_mode_triggers>
    - task_summary.md template exists in spec directory
    - spec.md contains enhanced sections (prompt_summary, executive_summary)
    - cross-repo-config.yaml exists with shared components
    - user explicitly requests enhanced execution mode
  </enhanced_mode_triggers>
  
  <traditional_mode_default>
    - used when enhanced triggers are not detected
    - maintains existing behavior exactly
    - provides upgrade path messaging
    - offers optional enhanced features
  </traditional_mode_default>
</feature_detection>

## Integration Points

<agent_os_framework>
  - Seamlessly integrates with enhanced create-spec workflow
  - Supports both traditional and module-based spec organization
  - Maintains compatibility with existing CLAUDE.md templates
  - Works with current product documentation and standards
</agent_os_framework>

<assetutilities_hub>
  - Validates shared component accessibility during execution
  - Manages version compatibility and updates
  - Handles offline scenarios with cached components
  - Documents integration points and dependencies
</assetutilities_hub>

<development_workflow>
  - Integrates with existing development server management
  - Supports standard git branching and PR workflows
  - Maintains code quality and testing standards
  - Provides comprehensive progress tracking and documentation
</development_workflow>

## Usage Examples

<command_examples>
  # Traditional execution (backward compatible)
  /execute-tasks @specs/2025-08-05-user-auth/tasks.md
  
  # Enhanced execution with specific task
  /execute-tasks @specs/modules/auth/2025-08-05-enhanced-auth/tasks.md task-1.2
  
  # Cross-repository component execution
  /execute-tasks @specs/modules/api/2025-08-05-service-integration/tasks.md --validate-shared-components
</command_examples>

<configuration_examples>
  # Enhanced execution preferences
  # In .agent-os/user-preferences.yaml
  execution_mode: "enhanced"
  task_summary_updates: true
  performance_tracking: true
  shared_component_validation: true
  real_time_documentation: true
</configuration_examples>

## Error Handling and Recovery

<enhanced_error_management>
  <shared_component_failures>
    <detection>automatic during component access</detection>
    <recovery>
      1. ATTEMPT cache lookup for offline version
      2. PROVIDE fallback implementation guidance
      3. DOCUMENT component unavailability
      4. CONTINUE with local alternatives
    </recovery>
  </shared_component_failures>
  
  <version_compatibility_issues>
    <detection>during component integration</detection>
    <recovery>
      1. IDENTIFY compatible version range
      2. SUGGEST upgrade or downgrade options
      3. PROVIDE migration guidance
      4. DOCUMENT compatibility constraints
    </recovery>
  </version_compatibility_issues>
  
  <performance_benchmark_failures>
    <detection>during testing and validation</detection>
    <recovery>
      1. ANALYZE performance bottlenecks
      2. SUGGEST optimization strategies
      3. DOCUMENT performance constraints
      4. PROVIDE performance improvement roadmap
    </recovery>
  </performance_benchmark_failures>
</enhanced_error_management>

## Final Verification Checklist

<enhanced_completion_checklist>
  <verify>
    - [ ] Task implementation completed according to specification
    - [ ] All tests passing including integration and performance tests
    - [ ] Task summary updated with comprehensive implementation details
    - [ ] Shared component integration validated and documented
    - [ ] Performance benchmarks met and recorded
    - [ ] Cross-references validated and updated
    - [ ] Visual documentation verified (mermaid diagrams render correctly)
    - [ ] Code committed with enhanced metadata and shared component versions
    - [ ] Pull request created with comprehensive template
    - [ ] Module indexes updated (if module-based organization)
    - [ ] Task status updated in tasks.md with enhanced metadata
    - [ ] Lessons learned documented for future reference
    - [ ] Next steps identified and communicated
  </verify>
</enhanced_completion_checklist>