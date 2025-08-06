---
description: Enhanced Spec Creation Rules for Agent OS
globs:
alwaysApply: false
version: 2.0
encoding: UTF-8
---

# Enhanced Spec Creation Rules

<ai_meta>
  <parsing_rules>
    - Process XML blocks first for structured data
    - Execute instructions in sequential order
    - Use templates as exact patterns
    - Request missing data rather than assuming
    - Support both traditional and enhanced workflows
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
  - Create detailed spec plans with enhanced features
  - Generate structured documentation with visual elements
  - Support both traditional and module-based organization
  - Maintain full backward compatibility with existing Agent OS workflows
</purpose>

<context>
  - Enhanced version of Agent OS framework
  - Executed when implementing roadmap items with advanced features
  - Creates comprehensive spec-specific documentation
  - Integrates with AssetUtilities hub for cross-repository sharing
</context>

<prerequisites>
  - Product documentation exists in .agent-os/product/
  - Access to:
    - @.agent-os/product/mission.md,
    - @.agent-os/product/roadmap.md,
    - @.agent-os/product/tech-stack.md
  - User has spec idea or roadmap reference
  - Optional: Cross-repository configuration in @.agent-os/cross-repo-config.yaml
  - Optional: User preferences in @.agent-os/user-preferences.yaml
</prerequisites>

## Enhanced Features

### Prompt Summary Capture
- Capture original request and context provided
- Track clarifications made during spec development
- Document reuse opportunities and evolution notes
- Maintain prompt history for iterative improvements

### Executive Summary Generation
- Business impact analysis with measurable metrics
- Technical overview with architecture implications
- Risk assessment and mitigation strategies
- Resource requirements and timeline estimates

### Visual Documentation
- Auto-generate mermaid diagrams based on system type
- Create architecture overviews and workflow diagrams
- Support for sequence, flowchart, and component diagrams
- Visual representation of data flows and user journeys

### Module-Based Organization
- Organize specs by functional modules for better structure
- Support subcategory hierarchies for large module collections
- Maintain cross-module references and dependencies
- Generate module indexes and documentation automatically

### Template Variants
- **minimal**: Basic spec with core sections only (overview, user stories, scope)
- **standard**: Traditional Agent OS spec format with all standard sections
- **enhanced**: Full featured spec with all enhancements (prompt summary, executive summary, mermaid diagrams)
- **api_focused**: Specialized for API development with detailed endpoint specifications
- **research**: For exploratory work with hypothesis and findings sections

### Cross-Repository Integration
- Reference shared components from AssetUtilities hub
- Validate cross-repository references automatically
- Cache shared components for offline usage
- Version compatibility checking and management

## Enhanced Process Flow

<process_flow>

<step number="1" name="enhanced_spec_initiation">

### Step 1: Enhanced Spec Initiation

<step_metadata>
  <trigger_options>
    - option_a: user_asks_whats_next
    - option_b: user_provides_specific_spec
    - option_c: user_specifies_variant_or_module
  </trigger_options>
  <enhanced_features>
    - prompt_capture: true
    - variant_detection: true
    - module_organization: true
  </enhanced_features>
</step_metadata>

<option_a_flow>
  <trigger_phrases>
    - "what's next?"
    - "what should we work on next?"
  </trigger_phrases>
  <actions>
    1. CHECK @.agent-os/product/roadmap.md
    2. FIND next uncompleted item
    3. SUGGEST item to user with recommended variant
    4. WAIT for approval
  </actions>
</option_a_flow>

<option_b_flow>
  <trigger>user describes specific spec idea</trigger>
  <accept>any format, length, or detail level</accept>
  <enhanced_actions>
    1. CAPTURE original request verbatim
    2. IDENTIFY context provided by user
    3. DETECT suggested organization (traditional vs module-based)
    4. PROCEED to context gathering
  </enhanced_actions>
</option_b_flow>

<option_c_flow>
  <trigger>user specifies variant or module explicitly</trigger>
  <patterns>
    - "/create-spec spec-name module-name variant"
    - "create enhanced spec for authentication module"
    - "minimal spec for bug fix in utils"
  </patterns>
  <actions>
    1. PARSE command format
    2. EXTRACT spec name, module, and variant
    3. VALIDATE variant and module names
    4. PROCEED with specified configuration
  </actions>
</option_c_flow>

<instructions>
  ACTION: Identify spec initiation method and capture prompt details
  ROUTE: Follow appropriate flow based on trigger
  CAPTURE: Original request for prompt summary
  WAIT: Ensure user agreement before proceeding
</instructions>

</step>

<step number="2" name="enhanced_context_gathering">

### Step 2: Enhanced Context Gathering

<step_metadata>
  <reads>
    - @.agent-os/product/mission.md
    - @.agent-os/product/roadmap.md
    - @.agent-os/product/tech-stack.md
    - @.agent-os/user-preferences.yaml (optional)
    - @.agent-os/cross-repo-config.yaml (optional)
  </reads>
  <purpose>understand spec alignment and user preferences</purpose>
</step_metadata>

<enhanced_context_analysis>
  <mission>overall product vision and business goals</mission>
  <roadmap>current progress, priorities, and dependencies</roadmap>
  <tech_stack>technical requirements and constraints</tech_stack>
  <user_preferences>preferred variant, organization, and features</user_preferences>
  <cross_repo_config>available shared components and version requirements</cross_repo_config>
</enhanced_context_analysis>

<preference_resolution>
  <variant_selection>
    1. IF user specified variant: USE specified variant
    2. ELSE IF user preferences exist: USE preferred_variant
    3. ELSE: DEFAULT to "standard" for backward compatibility
  </variant_selection>
  <organization_selection>
    1. IF user specified module: USE module-based organization
    2. ELSE IF preferences specify organization_type: USE specified type
    3. ELSE: DEFAULT to traditional date-based organization
  </organization_selection>
</preference_resolution>

<instructions>
  ACTION: Read all product documents and configuration files
  ANALYZE: Spec alignment with product context
  RESOLVE: User preferences and organizational choices
  NOTE: Consider implications for implementation and shared components
</instructions>

</step>

<step number="3" name="enhanced_requirements_clarification">

### Step 3: Enhanced Requirements Clarification

<step_metadata>
  <required_clarifications>
    - scope_boundaries: string
    - technical_considerations: array[string]
    - variant_confirmation: string
    - module_assignment: string (if module-based)
  </required_clarifications>
  <enhanced_features>
    - business_impact_assessment: boolean
    - visual_documentation_needs: boolean
    - cross_repository_dependencies: boolean
  </enhanced_features>
</step_metadata>

<enhanced_clarification_areas>
  <scope>
    - in_scope: what is included
    - out_of_scope: what is excluded (optional)
    - business_impact: expected outcomes and metrics
  </scope>
  <technical>
    - functionality specifics
    - UI/UX requirements
    - integration points
    - performance requirements
    - security considerations
  </technical>
  <visual>
    - system architecture complexity
    - user workflow visualization needs
    - data flow documentation requirements
  </visual>
  <cross_repo>
    - shared component dependencies
    - version compatibility requirements
    - offline fallback needs
  </cross_repo>
</enhanced_clarification_areas>

<enhanced_question_template>
  Based on the spec description, I need clarification on:

  1. [SPECIFIC_QUESTION_ABOUT_SCOPE]
  2. [SPECIFIC_QUESTION_ABOUT_TECHNICAL_APPROACH]
  3. [SPECIFIC_QUESTION_ABOUT_USER_EXPERIENCE]
  
  Enhanced features available:
  4. Would you like business impact analysis in the executive summary?
  5. Should I generate system architecture diagrams?
  6. Are there any shared components from other repositories to reference?
</enhanced_question_template>

<instructions>
  ACTION: Evaluate need for clarification including enhanced features
  ASK: Numbered questions covering traditional and enhanced aspects
  CAPTURE: All clarifications for prompt summary
  PROCEED: Only with clear requirements and preferences
</instructions>

</step>

<step number="4" name="enhanced_date_and_organization">

### Step 4: Enhanced Date and Organization Determination

<step_metadata>
  <purpose>Determine date and organizational structure</purpose>
  <priority>high</priority>
  <creates>appropriate directory structure</creates>
  <supports>both traditional and module-based organization</supports>
</step_metadata>

<organization_determination>
  <traditional_organization>
    <pattern>specs/YYYY-MM-DD-spec-name/</pattern>
    <when>user preferences specify "traditional" or no preferences set</when>
    <process>
      1. DETERMINE date using file system timestamp
      2. CREATE traditional spec directory structure
      3. MAINTAIN backward compatibility
    </process>
  </traditional_organization>
  
  <module_based_organization>
    <pattern>specs/modules/module-name/YYYY-MM-DD-spec-name/</pattern>
    <when>user preferences specify "module-based" or module explicitly provided</when>
    <process>
      1. DETERMINE date using file system timestamp
      2. VALIDATE module name (kebab-case, descriptive)
      3. CREATE module directory structure if not exists
      4. CREATE spec directory within module
    </process>
  </module_based_organization>
  
  <subcategory_support>
    <pattern>specs/modules/module-name/subcategory/YYYY-MM-DD-spec-name/</pattern>
    <when>module has >5 specs and subcategory provided</when>
    <process>
      1. COUNT existing specs in module
      2. IF >5 specs: SUGGEST subcategory organization
      3. CREATE subcategory structure if approved
    </process>
  </subcategory_support>
</organization_determination>

<date_determination_process>
  <primary_method>
    <name>File System Timestamp</name>
    <process>
      1. CREATE directory if not exists: specs/ or specs/modules/
      2. CREATE temporary file: .date-check
      3. READ file creation timestamp from filesystem
      4. EXTRACT date in YYYY-MM-DD format
      5. DELETE temporary file
      6. STORE date in variable for folder naming
    </process>
  </primary_method>

  <fallback_method>
    <trigger>if file system method fails</trigger>
    <name>User Confirmation</name>
    <process>
      1. STATE: "I need to confirm today's date for the spec folder"
      2. ASK: "What is today's date? (YYYY-MM-DD format)"
      3. WAIT for user response
      4. VALIDATE format matches YYYY-MM-DD
      5. STORE date for folder naming
    </process>
  </fallback_method>
</date_determination_process>

<instructions>
  ACTION: Determine organizational structure and date
  CREATE: Appropriate directory structure based on preferences
  SUPPORT: Both traditional and module-based organization
  VALIDATE: All naming conventions and formats
</instructions>

</step>

<step number="5" name="enhanced_spec_creation">

### Step 5: Enhanced Spec Creation

<step_metadata>
  <creates>
    - file: spec.md with variant-specific sections
  </creates>
  <supports>
    - minimal, standard, enhanced, api_focused, research variants
  </supports>
</step_metadata>

<variant_templates>
  <minimal_variant>
    <sections>
      - header
      - overview
      - user_stories
      - spec_scope
      - expected_deliverable
    </sections>
    <use_case>simple features, bug fixes, minor enhancements</use_case>
  </minimal_variant>
  
  <standard_variant>
    <sections>
      - header
      - overview
      - user_stories
      - spec_scope
      - out_of_scope
      - expected_deliverable
    </sections>
    <use_case>traditional Agent OS specs, backward compatibility</use_case>
  </standard_variant>
  
  <enhanced_variant>
    <sections>
      - header
      - prompt_summary
      - executive_summary
      - system_overview (with mermaid diagram)
      - overview
      - user_stories
      - spec_scope
      - out_of_scope
      - expected_deliverable
    </sections>
    <use_case>comprehensive features, new modules, complex integrations</use_case>
  </enhanced_variant>
  
  <api_focused_variant>
    <sections>
      - header
      - prompt_summary
      - executive_summary
      - api_overview (with endpoint diagram)
      - overview
      - user_stories
      - spec_scope
      - expected_deliverable
    </sections>
    <use_case>API development, service integrations, microservices</use_case>
  </api_focused_variant>
  
  <research_variant>
    <sections>
      - header
      - prompt_summary
      - research_context
      - hypothesis
      - methodology
      - expected_findings
      - success_criteria
    </sections>
    <use_case>exploratory work, proof of concepts, investigation tasks</use_case>
  </research_variant>
</variant_templates>

<enhanced_sections>
  <prompt_summary>
    <template>
      ## Prompt Summary
      
      **Original Request:** [CAPTURED_USER_REQUEST]
      **Context Provided:** [USER_CONTEXT_SUMMARY]
      **Clarifications Made:**
      [NUMBERED_LIST_OF_CLARIFICATIONS]
      **Reuse Notes:** [OPPORTUNITIES_FOR_COMPONENT_REUSE]
      **Prompt Evolution:** [CHANGES_FROM_ORIGINAL_TO_FINAL_SPEC]
    </template>
  </prompt_summary>
  
  <executive_summary>
    <template>
      ## Executive Summary
      
      ### Business Impact
      [BUSINESS_VALUE_AND_EXPECTED_OUTCOMES]
      
      ### Technical Overview
      [HIGH_LEVEL_ARCHITECTURE_AND_APPROACH]
      
      ### Resource Requirements
      [ESTIMATED_EFFORT_AND_DEPENDENCIES]
      
      ### Risk Assessment
      [POTENTIAL_RISKS_AND_MITIGATION_STRATEGIES]
    </template>
  </executive_summary>
  
  <system_overview>
    <template>
      ## System Overview
      
      [SYSTEM_DESCRIPTION_AND_CONTEXT]
      
      ```mermaid
      [AUTO_GENERATED_MERMAID_DIAGRAM]
      ```
      
      ### Architecture Notes
      [ARCHITECTURE_DECISIONS_AND_RATIONALE]
    </template>
  </system_overview>
</enhanced_sections>

<mermaid_generation>
  <diagram_types>
    <flowchart>
      <when>user workflows, business processes, decision trees</when>
      <template>
        graph TD
            A[Start] --> B{Decision}
            B -->|Yes| C[Action 1]
            B -->|No| D[Action 2]
            C --> E[Result]
            D --> E
      </template>
    </flowchart>
    
    <sequence>
      <when>API interactions, user journeys, system communications</when>
      <template>
        sequenceDiagram
            participant User
            participant System
            participant Database
            User->>System: Request
            System->>Database: Query
            Database-->>System: Result
            System-->>User: Response
      </template>
    </sequence>
    
    <component>
      <when>system architecture, module relationships, service dependencies</when>
      <template>
        graph TB
            subgraph "Frontend"
                A[User Interface]
                B[State Management]
            end
            subgraph "Backend"
                C[API Layer]
                D[Business Logic]
                E[Data Access]
            end
            A --> C
            B --> C
            C --> D
            D --> E
      </template>
    </component>
  </diagram_types>
</mermaid_generation>

<instructions>
  ACTION: Create spec.md with variant-specific sections
  GENERATE: Mermaid diagrams based on system type
  POPULATE: All sections with contextual information
  MAINTAIN: High quality and consistency across variants
</instructions>

</step>

<step number="6" name="enhanced_sub_spec_generation">

### Step 6: Enhanced Sub-Spec Generation

<step_metadata>
  <creates>
    - directory: sub-specs/
    - conditional files based on requirements
  </creates>
  <enhanced_features>
    - intelligent sub-spec detection
    - template customization per sub-spec type
    - cross-reference validation
  </enhanced_features>
</step_metadata>

<intelligent_detection>
  <technical_spec>
    <always_create>true</always_create>
    <enhanced_sections>
      - technical_requirements
      - approach_options_with_evaluation
      - external_dependencies_with_justification
      - performance_considerations
      - security_requirements
      - testing_strategy
    </enhanced_sections>
  </technical_spec>
  
  <api_spec>
    <create_when>
      - spec mentions API, endpoints, REST, GraphQL
      - variant is "api_focused"
      - user explicitly requests API specification
    </create_when>
    <enhanced_sections>
      - endpoint_specifications
      - authentication_requirements
      - rate_limiting_strategy
      - error_handling_patterns
      - versioning_strategy
      - documentation_generation
    </enhanced_sections>
  </api_spec>
  
  <database_schema>
    <create_when>
      - spec mentions database, schema, migrations, models
      - technical requirements include data persistence
      - user explicitly requests database specification
    </create_when>
    <enhanced_sections>
      - schema_changes_with_migrations
      - indexing_strategy
      - performance_optimization
      - data_migration_plan
      - backup_and_recovery_considerations
    </enhanced_sections>
  </database_schema>
  
  <tests_spec>
    <always_create>true</always_create>
    <enhanced_sections>
      - test_coverage_requirements
      - testing_strategy_by_layer
      - mocking_and_stubbing_strategy
      - performance_testing_requirements
      - security_testing_considerations
      - automation_and_ci_integration
    </enhanced_sections>
  </tests_spec>
</intelligent_detection>

<cross_reference_integration>
  <shared_components>
    <process>
      1. CHECK cross-repo-config.yaml for available components
      2. IDENTIFY relevant shared components for this spec
      3. VALIDATE version compatibility
      4. DOCUMENT shared component usage in technical-spec
      5. CREATE fallback plan for offline usage
    </process>
  </shared_components>
  
  <internal_references>
    <process>
      1. SCAN existing specs for related functionality
      2. IDENTIFY dependencies and integration points
      3. CREATE cross-references between related specs
      4. VALIDATE reference accessibility
    </process>
  </internal_references>
</cross_reference_integration>

<instructions>
  ACTION: Generate sub-specs based on intelligent detection
  ENHANCE: Each sub-spec with comprehensive sections
  VALIDATE: Cross-references and shared component usage
  DOCUMENT: Integration points and dependencies
</instructions>

</step>

<step number="7" name="enhanced_cross_reference_management">

### Step 7: Enhanced Cross-Reference Management

<step_metadata>
  <creates>comprehensive cross-reference system</creates>
  <validates>all reference types and accessibility</validates>
  <supports>
    - internal references (@specs/, @.agent-os/)
    - cross-repository references (@assetutilities:)
    - external references (https://)
  </supports>
</step_metadata>

<reference_validation>
  <internal_references>
    <pattern>@(specs|src|docs|tests|.agent-os)/.*</pattern>
    <validation>
      1. PARSE reference path
      2. RESOLVE to absolute file system path
      3. CHECK file existence and accessibility
      4. VALIDATE content relevance
    </validation>
  </internal_references>
  
  <cross_repo_references>
    <pattern>@\w+:.*</pattern>
    <validation>
      1. PARSE repository name and path
      2. CHECK cross-repo-config.yaml for repository configuration
      3. RESOLVE to hub repository path
      4. VALIDATE shared component accessibility
      5. CHECK version compatibility
    </validation>
  </cross_repo_references>
  
  <external_references>
    <pattern>https?://.*</pattern>
    <validation>
      1. VALIDATE URL format
      2. CHECK accessibility (optional, network dependent)
      3. DOCUMENT as external dependency
    </validation>
  </external_references>
</reference_validation>

<reference_documentation>
  <template>
    ## Spec Documentation
    
    ### Primary Documents
    - Tasks: @[SPEC_PATH]/tasks.md
    - Technical Specification: @[SPEC_PATH]/sub-specs/technical-spec.md
    
    ### Sub-Specifications
    [LIST_CREATED_SUB_SPECS_WITH_REFERENCES]
    
    ### Cross-Repository Dependencies
    [LIST_SHARED_COMPONENTS_WITH_VERSIONS]
    
    ### Related Specifications
    [LIST_INTERNAL_SPEC_REFERENCES]
    
    ### External Resources
    [LIST_EXTERNAL_DOCUMENTATION_LINKS]
  </template>
</reference_documentation>

<instructions>
  ACTION: Validate all references in generated documentation
  CREATE: Comprehensive cross-reference index
  DOCUMENT: All dependencies and integration points
  ENSURE: Reference accessibility and version compatibility
</instructions>

</step>

<step number="8-15" name="remaining_workflow_steps">

### Steps 8-15: Enhanced Workflow Completion

<backward_compatibility>
  All remaining steps (8-15) from the original create-spec.md workflow are preserved:
  - User review process
  - Task creation with TDD approach
  - Decision documentation
  - Execution readiness check
  
  Enhanced with:
  - Task summary template creation for enhanced variants
  - Cross-repository reference validation
  - Module index updates for module-based organization
  - Version compatibility checking for shared components
</backward_compatibility>

<enhanced_task_creation>
  <template_additions>
    - Include task summary completion for enhanced variants
    - Add cross-reference validation tasks
    - Include shared component integration verification
    - Add visual documentation validation steps
  </template_additions>
</enhanced_task_creation>

<enhanced_execution_readiness>
  <additional_checks>
    - Shared component accessibility validation
    - Cross-repository version compatibility
    - Module organization consistency
    - Visual documentation rendering verification
  </additional_checks>
</enhanced_execution_readiness>

</step>

</process_flow>

## Backward Compatibility

<compatibility_guarantee>
  - All existing create-spec workflows remain fully supported
  - Traditional date-based organization continues to work
  - Standard spec template is default for non-enhanced usage
  - Existing CLAUDE.md references and patterns are preserved
  - No breaking changes to Agent OS framework integration
</compatibility_guarantee>

<migration_support>
  <existing_specs>
    - Can be upgraded to enhanced format individually
    - Cross-references automatically include existing specs
    - Module organization is optional upgrade path
  </existing_specs>
  
  <gradual_adoption>
    - Teams can adopt enhanced features incrementally
    - Projects can mix traditional and enhanced specs
    - User preferences allow personal customization
  </gradual_adoption>
</migration_support>

## Integration Points

<agent_os_framework>
  - Maintains full compatibility with execute-tasks.md workflow
  - Integrates with CLAUDE.md template system
  - Supports existing product documentation structure
  - Works with current roadmap and mission alignment processes
</agent_os_framework>

<assetutilities_hub>
  - References shared components from hub repository
  - Validates cross-repository dependencies
  - Maintains version compatibility matrix
  - Supports offline fallback mechanisms
</assetutilities_hub>

<user_experience>
  - Respects user preferences and organizational standards
  - Provides intelligent defaults based on context
  - Offers template variants for different use cases
  - Maintains consistent command interfaces
</user_experience>

## Usage Examples

<command_formats>
  # Traditional Agent OS (backward compatible)
  /create-spec user-authentication
  
  # Enhanced with variant specification
  /create-spec user-authentication enhanced
  
  # Module-based organization
  /create-spec auth-system authentication enhanced
  
  # Minimal variant for simple changes
  /create-spec bug-fix utils minimal
  
  # API-focused development
  /create-spec user-api services api_focused
  
  # Research and exploration
  /create-spec performance-investigation optimization research
</command_formats>

<preference_configuration>
  # In .agent-os/user-preferences.yaml
  preferred_variant: "enhanced"
  organization_type: "module-based"
  default_sections:
    - "prompt_summary"
    - "executive_summary"
    - "system_overview"
  enable_mermaid_diagrams: true
  enable_cross_references: true
  auto_detect_sub_specs: true
</preference_configuration>

## Final Checklist

<enhanced_verification>
  <verify>
    - [ ] Variant selection respected (minimal/standard/enhanced/api_focused/research)
    - [ ] Organization structure created (traditional or module-based)
    - [ ] Prompt summary captured with all clarifications
    - [ ] Executive summary generated with business impact
    - [ ] Mermaid diagrams generated for visual documentation
    - [ ] Sub-specs created based on intelligent detection
    - [ ] Cross-references validated and documented
    - [ ] Shared components integrated and version-checked
    - [ ] Module indexes updated (if module-based)
    - [ ] Task summary template created (for enhanced variants)
    - [ ] Backward compatibility maintained throughout
  </verify>
</enhanced_verification>