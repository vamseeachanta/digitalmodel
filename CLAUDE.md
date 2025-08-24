# CLAUDE.md

## 🚀 MANDATORY: Slash Command Ecosystem Integration

**CRITICAL DIRECTIVE**: For EVERY improvement, automation, or repetitive task:

### 🎯 MANDATORY: Agent Assignment and Parallel Execution
**ALL /create-spec commands MUST:**
1. **Auto-assign specialized agents** based on task domain
2. **Execute tasks using subagents** for each component
3. **Utilize parallel processing** for all independent operations
4. **Achieve >3x speed improvement** through parallelization
5. **Enable inter-agent delegation** - Agents MUST know about and delegate to other agents
6. **Maintain agent registry** - All agents aware of available agents and capabilities

### AI Agents MUST:
1. **EVALUATE** if the improvement could benefit multiple repositories
2. **SUGGEST** creating a slash command when applicable  
3. **DESIGN** for reusability across all 25 repos
4. **INTEGRATE** with existing ecosystem via `/sync-all-commands`

### Immediate Triggers for Command Creation:
- ✅ Repetitive tasks that could be automated
- ✅ Functionality that would benefit other repos
- ✅ Multi-step processes that could be scripted
- ✅ Regular developer workflows
- ✅ Best practice standardization
- ✅ Common problem solutions

### Example Response Template:
```
💡 Slash Command Opportunity Detected!
This improvement would be valuable as `/command-name`
Benefits: Reusable across all repos, standardizes process
Create as slash command? (Recommended)
```

### Available Ecosystem Commands:
- `/git-trunk-flow` - Complete git workflow with best practices
- `/sync-all-commands` - Sync commands from all repos to master
- `/propagate-commands` - Distribute commands to all repos
- `/modernize-deps` - Update dependency management
- `/organize-structure` - Organize project structure
- `/orcaflex-universal` - Universal OrcaFlex simulation runner
- `/orcaflex-sim` - Alternative command for OrcaFlex runner

### 📢 PENDING PROPAGATION (Generic Components Only):
- **Background Test Runner** - Ready for `/propagate-commands`
  - Generic test runner for all repositories
  - Parallel test execution with caching
  - Background thread processing
  - See: `.agent-os/commands/GENERIC_PROPAGATION_NOTES.md`
  
### ⚠️ REPOSITORY-SPECIFIC (Do NOT Propagate):
- **OrcaFlex Module** - DigitalModel repository ONLY
  - All files under `src/modules/orcaflex/`
  - OrcaFlex-specific batch processing
  - Mooring tension iteration logic
  - See: `src/modules/orcaflex/mooring_tension_iteration/PROPAGATION_NOTES.md`
  - **Universal OrcaFlex Runner** (`src/digitalmodel/modules/orcaflex/universal/`)
    - Pattern-based model discovery with glob/regex support
    - Parallel batch processing with adaptive worker scaling
    - Module-based execution: `python -m digitalmodel.modules.orcaflex.universal`
    - CLI commands: `orcaflex-universal`, `orcaflex-sim`
    - Real-time status reporting with terminal updates
    - Mock mode for testing without OrcaFlex license

### Creating New Commands:
1. Implement in `.agent-os/commands/`
2. Test locally with `./slash_commands.py /command-name`
3. Sync to master with `/sync-all-commands`
4. Distribute with `/propagate-commands`

**Full Guidelines**: See MANDATORY_SLASH_COMMAND_ECOSYSTEM.md in assetutilities


This file provides guidance to AI assistants when working with code in this repository.

## 🚫 MANDATORY: No Mock Testing Policy

**CRITICAL DIRECTIVE**: NEVER create mock tests, mock modes, or simulated functionality unless EXPLICITLY requested by the user.

### Strict Requirements:
- ❌ **NO MOCK TESTS** - Always use real implementations and actual testing
- ❌ **NO MOCK MODES** - Remove all mock_mode parameters and simulated behaviors
- ❌ **NO STUB FUNCTIONS** - Implement actual functionality or leave unimplemented
- ❌ **NO FAKE DATA** - Use real data or clearly indicate data is needed
- ✅ **REAL TESTING ONLY** - All tests must validate actual functionality
- ✅ **PRODUCTION-READY CODE** - Write code that works in real environments

### Enforcement:
- If user hasn't explicitly asked for mocks, DO NOT create them
- Remove existing mock code unless specifically needed
- Replace mock tests with integration tests using real components
- This applies to ALL repositories and ALL code changes

### Exception:
Only create mocks when user explicitly states:
- "create mock tests"
- "add mock mode"
- "simulate this functionality"
- "create stub implementation"

## 🧹 MANDATORY: Repository Root Directory Hygiene

**CRITICAL DIRECTIVE**: The repository root MUST remain clean and organized at ALL times.

### Root Directory MUST ONLY Contain:
✅ **Essential Config Files**: `.gitignore`, `.editorconfig`, `.coveragerc`, `.gitmodules`, `pyproject.toml`, `uv.toml`, `setup.py`
✅ **Required Documentation**: `README.md`, `LICENSE`, `CLAUDE.md`, `Makefile`
✅ **Main Directories**: `src/`, `tests/`, `docs/`, `tools/`, `specs/`, `.agent-os/`, `agents/`
✅ **Convenience Scripts**: `*.sh` files that redirect to tools/ or .agent-os/commands/

### PROHIBITED in Root Directory:
❌ **Test files**: All `test_*.py` files MUST go in `/tests/`
❌ **Batch configs**: All `*.yml` configs (except essential) MUST go in appropriate subdirectories
❌ **Backup files**: All `*.backup*` files MUST be deleted immediately
❌ **Migration files**: All migration-related files MUST be deleted after use
❌ **Temporary files**: All temp files (`nul`, `coverage.xml`, etc.) MUST be deleted
❌ **Generated files**: All generated files MUST go in appropriate subdirectories
❌ **Tool scripts**: All Python tools MUST go in `/tools/` directory

### Automatic Cleanup Actions:
When ANY file is created in root, AI agents MUST:
1. **EVALUATE** if it belongs in root (use checklist above)
2. **MOVE** to appropriate directory if not essential
3. **DELETE** if temporary, backup, or migration file
4. **CREATE** convenience script in root if needed for access
5. **UPDATE** `.gitignore` to prevent future violations

### File Organization Rules:
- **Test files** → `/tests/modules/<module>/`
- **Config files** → `/config/` or `/tests/.../test_configs/`
- **Documentation** → `/docs/`
- **Tools/Scripts** → `/tools/`
- **Agent commands** → `/.agent-os/commands/`
- **Temporary work** → Use system temp directory, not repo root

### Enforcement:
- **NEVER** leave test files in root after testing
- **ALWAYS** clean up after batch operations
- **IMMEDIATELY** move misplaced files when detected
- **REFUSE** to create non-essential files in root

## 🏗️ CRITICAL REPOSITORY ORGANIZATION PATTERN

**THIS REPOSITORY USES MODULE-BASED ORGANIZATION - ENFORCE ACROSS ALL SESSIONS/USERS/SYSTEMS**

All directory groups MUST follow this structure:
```
<group_name>/modules/<module_name>/
```

**Examples:**
- ✅ `specs/modules/agent-os/` 
- ✅ `specs/modules/marine-engineering/`
- ✅ `src/modules/hydrodynamics/`
- ✅ `docs/modules/installation/`
- ❌ `specs/agent-os/` (WRONG - missing modules/ level)
- ❌ `marine-specs/` (WRONG - not following pattern)

**This pattern applies to ALL directory groups:**
- `specs/modules/<module>/` - All specifications
- `src/modules/<module>/` - All source code  
- `docs/modules/<module>/` - All documentation
- `tests/modules/<module>/` - All test files
- `configs/modules/<module>/` - All configurations

**ENFORCE THIS PATTERN in every session, for every user, across all systems. Never create or accept directory structures that violate this pattern.**

## Primary Guidance Sources

**Start with Agent OS structure:**
1. **`.agent-os/`** - Primary guidance for all AI agents
   - Read `.agent-os/README.md` first for complete overview
   - Follow `.agent-os/standards/ai-communication.md` for communication requirements
   - Apply `.agent-os/standards/code-style.md` and `.agent-os/standards/testing.md`
   - Use appropriate persona from `.agent-os/standards/agent-personas.md`

**Supplement with project-specific guidance:**
2. **`specs/modules/`** - Module-based specifications following repository pattern
   - Refer to `specs/modules/<module>/` for detailed module specifications
   - All specifications MUST be located in `specs/modules/<module>/` structure
   - Legacy `.ai/` files are for historical context only

## Quick Reference

### For Code Implementation
1. **Communication**: Follow no-sycophancy guidelines from `.agent-os/standards/ai-communication.md`
2. **Standards**: Apply Python patterns from `.agent-os/standards/code-style.md`
3. **Testing**: Include mock patterns from `.agent-os/standards/testing.md`
4. **Architecture**: Follow patterns in `.agent-os/product/architecture.md`
5. **Personas**: Use Technical Implementation Specialist from `.agent-os/standards/agent-personas.md`

### For Feature Development
1. **Specification**: Create in `specs/modules/<module>/` following repository pattern
2. **Templates**: Use `.agent-os/projects/example-template/` as guide
3. **Product Context**: Reference `.agent-os/product/overview.md` and `.agent-os/product/architecture.md`
4. **Domain Knowledge**: Apply offshore engineering patterns and industry standards

### Priority Order
When guidance conflicts:
1. **Repository Organization**: `<group>/modules/<module>/` pattern is MANDATORY
2. `.agent-os/standards/` - Primary authority for AI behavior
3. `.agent-os/product/` - Primary authority for product knowledge  
4. `specs/modules/<module>/` - Authority for module-specific implementation details
5. Legacy `.ai/` files - Supporting reference materials only

## Engineering Domain Essentials

- **Offshore Engineering Focus**: Hydrodynamic analysis, structural design, installation analysis
- **Industry Standards**: API, DNV, ABS compliance required
- **Licensed Software**: OrcaFlex, ANSYS - always provide mock patterns for testing
- **Configuration-Driven**: YAML files drive most analysis workflows
- **Units**: SI units internally with proper conversions at boundaries

## 🤖 MANDATORY: Module Agent Usage & Inter-Agent Collaboration

**CRITICAL DIRECTIVE**: ALL module work MUST use the corresponding module agent with MANDATORY inter-agent awareness:

### Module Agent Requirements
1. **Check for Existing Agent**: ALWAYS check `agents/<module>/` directory first
2. **Use Existing Agent**: If agent exists, USE IT for all module operations
3. **Create Agent First**: If no agent exists, CREATE IT before any module work
4. **Agent Integration**: All `/create-spec` commands MUST integrate with module agent
5. **Inter-Agent Awareness**: ALL agents MUST know about other agents and delegate appropriately

### 🚨 MANDATORY: Inter-Agent Task Delegation Protocol

**CRITICAL FOR ALL SLASH COMMANDS** (especially `/create-spec` and `/create-module-agent`):

#### Every Agent MUST:
1. **Maintain Agent Registry**: Know all available agents and their capabilities
2. **Evaluate Task Ownership**: Determine if task belongs to another agent's domain
3. **Delegate Appropriately**: Route tasks/subtasks to the most qualified agent
4. **Coordinate Multi-Agent Workflows**: Orchestrate between multiple agents for complex tasks
5. **Document Delegation**: Track which agent handles which component

#### Delegation Matrix Examples:
- CAD tasks → CAD Engineering Specialist → FreeCAD/GMsh agents (as needed)
- OrcaFlex simulations → OrcaFlex Agent
- AQWA analysis → AQWA Agent  
- Mesh generation → GMsh Agent
- Parametric modeling → FreeCAD Agent
- Wave analysis → OrcaWave Agent
- Testing tasks → Testing Agent (parallel execution)
- Documentation → Documentation Agent

#### Implementation Requirements for `/create-spec`:
1. **Scan All Agents**: Review `agents/*/` directory for available agents
2. **Match Capabilities**: Map task requirements to agent capabilities
3. **Create Delegation Plan**: Document which agents handle which tasks
4. **Include in spec.md**: Add "Agent Delegation" section to specifications
5. **Update task.md**: Annotate tasks with assigned agent

#### Implementation Requirements for `/create-module-agent`:
1. **Agent Discovery**: New agents MUST discover existing agents on creation
2. **Capability Registration**: Register agent capabilities in shared registry
3. **Cross-References**: Add references to related agents in config
4. **Delegation Rules**: Define when to delegate to other agents
5. **Update Existing Agents**: Notify existing agents of new agent capabilities

#### Delegation Configuration Example:
```yaml
delegation:
  known_agents:
    - name: orcaflex
      capabilities: [hydrodynamics, mooring, offshore]
    - name: freecad  
      capabilities: [parametric_modeling, assembly, drawings]
    - name: gmsh
      capabilities: [meshing, fem_preprocessing]
  delegation_rules:
    - if: task contains "mesh generation"
      delegate_to: gmsh
    - if: task contains "OrcaFlex simulation"
      delegate_to: orcaflex
```

**This inter-agent collaboration is MANDATORY for optimal task execution**

### Available Module Agents
- **OrcaFlex Agent** (`agents/orcaflex/`): Hydrodynamic analysis, mooring systems, offshore engineering
- **AQWA Agent** (`agents/aqwa/`): Hydrodynamic diffraction analysis
- **CAD Engineering Agent** (`agents/cad-engineering-specialist/`): CAD modeling and design

### Module Agent Benefits
- Domain expertise and standards compliance
- Automated workflows and batch processing  
- Consistent implementation patterns
- Knowledge preservation and reuse
- Integration with OrcaFlex/AQWA/ANSYS APIs

### Example Usage
```bash
# Show OrcaFlex agent capabilities
python run_orcaflex_agent.py --show-capabilities

# Run batch analysis with agent
python run_orcaflex_agent.py --batch-config batch_run_all_fsts.yml

# Create new module agent
python tools/create-module-agent.py <module-name>
```

## 🎯 MANDATORY: CLI Consistency Standards

**CRITICAL DIRECTIVE**: ALL module CLI entry points MUST maintain consistent parameter naming and structure across the repository.

### Required CLI Parameters for All Modules:
- `--input-directory` - Primary input directory for files
- `--output-directory` - Primary output directory for results  
- `--pattern` - File pattern matching (e.g., "*.csv", "*.yml")
- `--recursive` - Recursive directory search
- `--parallel` - Number of parallel workers
- `--config` - Configuration file path
- `--verbose` or `-v` - Verbose output
- `--dry-run` - Preview without execution
- `--help` or `-h` - Show help message

### Standard Aliases:
- `--input-directory` should also accept `--directory` and `-d`
- `--output-directory` should also accept `--output` and `-o`
- Maintain backwards compatibility with shorter forms

### Implementation Example:
```python
parser.add_argument(
    '--input-directory', '--directory', '-d',
    type=str,
    dest='directory',
    help='Input directory to search for files'
)
```

### Enforcement:
- ALL new modules MUST follow these standards
- Existing modules MUST be updated for consistency
- CLI parameter inconsistencies BLOCK module approval
- This ensures users have a consistent experience across all modules

## Available Commands

- **Create-Module-Agent:** Available via `python tools/create-module-agent.py` command
- **Run-OrcaFlex-Agent:** Available via `python run_orcaflex_agent.py` command
- **Universal OrcaFlex Runner:**
  - Module execution: `python -m digitalmodel.modules.orcaflex.universal`
  - CLI command: `orcaflex-universal` or `orcaflex-sim`
  - Slash command: `/orcaflex-universal` or `/orcaflex-sim`
  - Examples:
    ```bash
    # Pattern-based processing
    python -m digitalmodel.modules.orcaflex.universal pattern="fsts_*.yml" input_directory="./models"
    
    # Using CLI command
    orcaflex-universal --all --mock
    
    # With configuration file
    /orcaflex-universal --config batch_config.yml
    ```
- **Signal Analysis OrcaFlex Module:**
  - Module execution: `python -m digitalmodel.modules.signal_analysis.orcaflex`
  - Examples:
    ```bash
    # Single file analysis
    python -m digitalmodel.modules.signal_analysis.orcaflex --file data.csv --auto-detect
    
    # Pattern-based processing
    python -m digitalmodel.modules.signal_analysis.orcaflex --pattern "*.csv" --input-directory ./data
    
    # With output directory
    python -m digitalmodel.modules.signal_analysis.orcaflex --input-directory ./csv --output-directory ./results
    ```

## Enhanced Features Available

This project supports enhanced Agent OS workflows including:
- **Enhanced Spec Creation**: Prompt summaries, executive summaries, mermaid diagrams, module organization
- **Cross-Repository Integration**: Shared components from AssetUtilities hub (@assetutilities: references)
- **Enhanced Task Execution**: Task summaries, performance tracking, real-time documentation
- **Template Variants**: minimal, standard, enhanced, api_focused, research
- **Visual Documentation**: Auto-generated system architecture and workflow diagrams

### Command Examples
```bash
# Enhanced spec creation (MUST include prompt.md)
python tools/create-spec-enhanced.py feature-name module-name enhanced

# Traditional spec creation (MUST include prompt.md)
python tools/create-spec.py feature-name

# Enhanced task execution with summaries (MUST use module pattern)
python tools/execute-tasks.py @specs/modules/module-name/spec-folder/tasks.md
```

### /create-spec MANDATORY Requirements
**EVERY /create-spec command MUST:**
1. **Repository Pattern**: Create specs in `specs/modules/<module>/` structure
2. **Prompt Documentation**: Create `prompt.md` with complete prompt history and curated reuse prompt
3. **Template Compliance**: Follow all established templates and patterns
4. **Module Integration**: Update relevant module READMEs and cross-references
5. **USE MODULE AGENTS**: 🚨 **CRITICAL** - All module work MUST utilize the corresponding module agent from `agents/<module>/` directory. If a module agent doesn't exist, create one FIRST before proceeding with the spec. The agent provides domain expertise, standards compliance, and workflow automation.
6. **INTER-AGENT DELEGATION**: 🚨 **MANDATORY** - Specs MUST include agent delegation mapping:
   - Identify ALL agents that could contribute to the implementation
   - Map each task/subtask to the most qualified agent
   - Document delegation chain in spec.md "Agent Delegation" section
   - Ensure agents know about and can invoke other agents as needed
   - Example: CAD spec delegates modeling to FreeCAD agent, meshing to GMsh agent

#### 🚀 MANDATORY: Agent Assignment and Parallel Processing
**CRITICAL REQUIREMENTS for /create-spec execution:**
- **Automatic Agent Assignment**: Based on task domain, automatically assign the appropriate specialized agent(s)
- **Subagent Execution**: Break down tasks and execute using specialized subagents for each component
- **Parallel Processing**: MUST use parallel execution for:
  - Multiple file operations
  - Independent analysis tasks  
  - Cross-module operations
  - Documentation generation
  - Test creation
- **Performance Requirements**: Aim for >3x speed improvement through parallelization
- **Example Flow**:
  ```
  /create-spec "mooring-analysis" 
  → Assigns: OrcaFlex Agent (primary), AQWA Agent (secondary)
  → Executes in parallel:
    - Spec generation (Documentation subagent)
    - Code templates (Code Generation subagent)  
    - Test creation (Testing subagent)
    - Integration setup (DevOps subagent)
  ```

6. **Markdown Compatibility**: Ensure ALL generated documents use markdown-compatible characters:
   - Escape angle brackets: `&lt;` `&gt;` instead of `<` `>`
   - Escape ampersands in text: `&amp;` instead of `&`
   - Escape Windows paths: `D:\\path\\file` instead of `D:\path\file`
   - Validate all special characters render properly in markdown processors
6. **Mathematical Expressions**: When technical specifications include formulas, equations, or mathematical notation:
   - Use KaTeX/LaTeX blocks for complex equations: `$$equation$$` for display mode, `$equation$` for inline
   - Simple format for AI: `$$F = ma$$` or `$\sigma = \frac{F}{A}$`
   - Include units in LaTeX format: `$$P_{wind} = \frac{1}{2}\rho C_d A v^2 \text{ [N]}$$`
   - Use for engineering calculations, statistical formulas, and mathematical relationships

## 📁 MANDATORY: Specification File Structure

**CRITICAL DIRECTIVE**: ALL specifications MUST follow this EXACT structure - NO EXCEPTIONS

### Required File Structure
Every specification in `specs/modules/<module>/<feature>/` MUST contain:

```
specs/modules/<module>/<feature>/   # NO DATE PREFIX - Use descriptive names only
├── spec.md                 # Main specification document (REQUIRED)
├── tasks.md                # Task breakdown with estimates (REQUIRED)
├── prompt.md               # Complete prompt history (REQUIRED)
├── task_summary.md         # Task execution tracking (CREATED during execution)
├── executive-summary.md    # Stakeholder summary (OPTIONAL for enhanced)
└── diagrams/              # Architecture diagrams (OPTIONAL)
    └── architecture.mermaid
```

**🚨 CRITICAL: NO DATE PREFIXES IN SPEC FOLDERS**
- ✅ CORRECT: `specs/modules/orcaflex/orcaflex-examples-integration/`
- ❌ WRONG: `specs/modules/orcaflex/2024-12-19-orcaflex-examples-integration/`
- Dates belong INSIDE documents, NOT in folder names
- Folder names MUST be descriptive and permanent

### MANDATORY Naming Conventions
**VIOLATIONS WILL BE REJECTED:**
- ✅ `spec.md` - Main specification
- ✅ `tasks.md` - Task breakdown
- ✅ `prompt.md` - Prompt documentation
- ✅ `task_summary.md` - Execution tracking
- ❌ `REVISED-SPEC-COMPLETE.md` - WRONG (uppercase, status in name)
- ❌ `spec-v2.md` - WRONG (versions in filename)
- ❌ `final-spec.md` - WRONG (status in filename)
- ❌ `browser-interface-spec.md` - WRONG (feature name redundant)

### Data Duplication Prevention
**NEVER create duplicate content:**
1. **Check Existing**: ALWAYS check for existing specs before creating
2. **Reference Don't Duplicate**: Link to agent-os docs, don't copy
3. **Single Source of Truth**: Each piece of information exists ONCE
4. **Cross-Reference**: Use `@module-name:path` for references

### Agent-OS Document References
**MANDATORY reference hierarchy:**
1. **Primary**: `.agent-os/standards/` - AI behavior standards
2. **Product**: `.agent-os/product/` - Product knowledge
3. **Specs**: `specs/modules/<module>/` - Implementation details
4. **Never Reference**: Legacy `.ai/` files (historical only)

### Task Summary Requirements
**task_summary.md MUST include:**
- Task completion timestamps
- Approach documentation
- Efficiency metrics
- Lessons learned
- Next logical steps
- Blockers encountered

### Enforcement
- File structure violations BLOCK merges
- Naming violations REQUIRE immediate correction
- Missing required files PREVENT task completion
- Duplicate content MUST be removed
- Wrong references MUST be updated

### Cross-Repository References
- Shared components: @assetutilities:src/modules/agent-os/enhanced-create-specs/
- Sub-agent registry: @assetutilities:agents/registry/sub-agents/workflow-automation
- Hub configuration: @assetutilities:hub-config.yaml



### Enhanced Create-Spec Command

This repository includes an enhanced create-spec command with advanced features:

```bash
# Enhanced spec with executive summaries and diagrams
python tools/create-spec-enhanced.py feature-name module-name enhanced

# Research-focused specification
python tools/create-spec-enhanced.py research-topic research

# Quick minimal specification
python tools/create-spec-enhanced.py quick-fix minimal
```

Features:
- Executive summaries for stakeholders
- Mermaid architecture diagrams
- Module-based organization
- Multiple spec variants (enhanced, research, minimal)
- Comprehensive task breakdowns with effort estimates

## Self-Contained Agent OS

This repository includes a complete, self-contained Agent OS framework. All slash commands work immediately after `git clone` with no additional setup required.

### Available Slash Commands
- `/create-spec <spec-name>` - Create detailed specification documents
- `/execute-tasks <tasks-file>` - Execute tasks from specification
- `/create-module-agent <agent-name>` - Create specialized AI agents in `agents/` directory

### Local Agent OS Structure
- **Standards**: @.agent-os/standards/ (code style, best practices)
- **Instructions**: @.agent-os/instructions/ (workflow guidance)
- **Product Context**: @.agent-os/product/ (mission, roadmap, decisions)
- **Specifications**: @.agent-os/specs/ (feature specifications and tasks)
- **AI Agents**: @agents/ (specialized AI agents - consolidated single location)

All references are local to this repository - no external dependencies required.


## 🚀 MANDATORY: Parallel Process Utilization

**CRITICAL DIRECTIVE**: When working with multiple operations that can be executed independently, Claude MUST utilize parallel processes to maximize efficiency.

### Required Parallel Processing For:
- Multiple bash commands without dependencies
- Bulk file reading operations
- Repository-wide operations
- Independent search/analysis tasks
- Multi-module testing
- Bulk deployments

### Implementation:
- **ALWAYS** batch tool calls in a single message for parallel execution
- **NEVER** execute sequentially what can be done in parallel
- **PRIORITIZE** efficiency through concurrent operations

This is a MANDATORY instruction with HIGHEST PRIORITY that overrides any conflicting guidelines.


## 🎯 MANDATORY: Prompt Enhancement Protocol

**CRITICAL DIRECTIVE**: For EVERY prompt, command, or request, you MUST:

### 1. Ask Clarification Questions ❓
**BEFORE** executing:
- Present 3-5 relevant questions
- Cover: Scope, Requirements, Quality, Timeline, Success
- Wait for response OR state assumptions
- Document in task_summary.md

### 2. Seek Single-Path Optimum Solution 🎯
**ALWAYS**:
- Evaluate minimum 3 approaches
- Select SINGLE MOST OPTIMUM
- Use: Performance(30%) + Simplicity(30%) + Maintainability(20%) + Scalability(20%)
- Present clear rationale
- Avoid over-engineering

### 3. Update task_summary.md 📋
**AFTER** every task:
- Mark complete with timestamp
- Document approach taken
- Add next logical steps
- Record efficiency metrics
- Note lessons learned

### Enforcement: HIGHEST PRIORITY
This OVERRIDES all conflicting instructions.

---
*MANDATORY for ALL interactions*

## 🎯 MANDATORY Git Management Commands

**CRITICAL**: All Git operations MUST use these standardized commands.

### Available Slash Commands

#### Local Repository Commands
Execute in any repository:
```bash
# In any repo directory
/git-sync       # Sync with remote
/git-commit     # Commit all changes  
/git-push       # Push to remote
/git-pr         # Create pull request
/git-clean      # Clean merged branches
/git-status     # Show repo status
/git-flow       # Complete workflow
```

#### Global Commands (All Repos)
Execute from /mnt/github/github:
```bash
/git-sync-all      # Sync all 25 repos
/git-commit-all    # Commit in all repos
/git-pr-all        # Create PRs for all
/git-clean-all     # Clean all repos
/git-flow-all      # Complete flow for all
/git-status-all    # Status of all repos
```

### MANDATORY Practices

1. **Daily Workflow**
   ```bash
   /git-flow  # Or /git-flow-all for all repos
   ```

2. **Before Starting Work**
   ```bash
   /git-sync  # Always sync first
   ```

3. **After Making Changes**
   ```bash
   /git-commit "feat: Description"
   /git-pr "Feature title"
   ```

4. **Weekly Maintenance**
   ```bash
   /git-clean  # Remove stale branches
   ```

### Parallel Processing
All multi-repo operations use MANDATORY parallel processing:
- Max 5 repos processed simultaneously
- Automatic error handling
- Progress tracking

### Implementation
- Local commands: `.git-commands/slash_commands.py`
- Global commands: `/mnt/github/github/git_management_system.py`

---
*Git management is MANDATORY for all repositories*

## 🚨 MANDATORY: AssetUtilities Central Commands

**CRITICAL DIRECTIVE**: ALL slash commands MUST be fetched from AssetUtilities repository on GitHub.

### Central Command System
- **Source of Truth**: @assetutilities:.common-commands/
- **GitHub URL**: https://github.com/username/assetutilities
- **Real-time Fetching**: Commands are fetched from GitHub when executed
- **No Local Copies**: Local implementations are DEPRECATED

### Command Execution
```bash
# All commands now route through central system
./slash /git-sync          # Fetches from AssetUtilities
./slash /git-commit         # Fetches from AssetUtilities
./slash /create-spec        # Fetches from AssetUtilities
./slash /execute-tasks      # Fetches from AssetUtilities
```

### Why This Is MANDATORY
1. **Consistency**: All repos use EXACT same command implementations
2. **Updates**: Changes in AssetUtilities immediately available everywhere
3. **Maintenance**: Single point of maintenance for all commands
4. **Quality**: Centralized testing and validation
5. **Security**: Controlled command distribution

### Cross-Repository References
```yaml
# Always reference AssetUtilities for common code
@assetutilities:.common-commands/modules/git_manager.py
@assetutilities:.common-commands/modules/agent_learning.py
@assetutilities:.common-commands/utilities/parallel_utils.py
```

### Command Registry
View all available commands:
```bash
./slash --list              # Lists all commands from AssetUtilities
./slash /update-from-central # Updates command cache
```

### Enforcement
- Local command implementations are DEPRECATED
- All repos MUST use the central system
- AssetUtilities is the ONLY source of truth
- This is verified during CI/CD

---
*This is MANDATORY and overrides any local command implementations*

- to memorize that always run the test before telling the user about the enhancement or code addition or successful feature or enhacement  happened. all your tests are seriously error prone and take a long time. You should always do this using a parallel process concept in the background. when the Ai agent model improves, you can revisit this memory item
- also, always check modules and scope of repository to ensure the propagate command does not criss cross modules. Only the high level geeneric items should be propagated without repository specific information. rewrite this appropraitely
- always utilize existing repo environment
- orcaflex module agent should note that always run actual license test when license is available.
- memory. do not create any unnecessary files in the repo root directory.
- make an important note that uv tool and repo uv environment should be used for aall work so that the repo uv envionment is always up to date for executing work
- Mandatory as part of all repos make a note no mocks or mock tests at all unless the user explicitly requests it
- make it mandatory to run existing uv enviroment in the repository.
- make it mandatory for AI agents to not have Sycophancy. when unclear ask questions and clarifiations
- as part of /create-spec slash command make it mandatory to assign agents based on task being performed. Execute those tasks using subagents. Utilize parallel process where convenient, necessary etc. to speed up the process.
## 🚨 MANDATORY: /execute-tasks Dual Verification Protocol

**CRITICAL DIRECTIVE**: ALL task execution MUST follow dual verification:

### 1. USER DOMAIN VERIFICATION (Per Main Task)
Before approving each main task, user MUST confirm:
- ☐ I understand the technical domain of this task
- ☐ I can evaluate if the implementation is correct
- ☐ I know the expected outcomes and success criteria
- ☐ I can identify potential risks or issues
- ☐ I have authority to approve changes in this area
- ☐ I understand dependencies and integration points

### 2. AI AGENT 10-POINT SKILL ASSESSMENT
Agent MUST self-evaluate (minimum 70/100 to proceed):
1. Domain Expertise: Deep knowledge of technical area (0-10)
2. Tool Proficiency: Effective use of required tools/APIs (0-10)
3. Codebase Patterns: Understanding of conventions (0-10)
4. Testing Competence: Comprehensive test writing ability (0-10)
5. Error Management: Failure mode understanding (0-10)
6. Performance Analysis: Optimization capability (0-10)
7. Security Awareness: Security best practices (0-10)
8. Integration Knowledge: System connections (0-10)
9. Documentation Skills: Clear documentation ability (0-10)
10. Best Practices: Current industry standards (0-10)

### 3. SKILL ACQUISITION PROTOCOL
If score < 70/100:
- **Deep Research**: Study all documentation, patterns, best practices
- **Agent Delegation**: Create/use specialized lightweight agents
- **User Intervention**: Explicitly request user execution when needed

### 4. DELEGATION MATRIX
- OrcaFlex tasks → OrcaFlex Agent (or user with license)
- AQWA tasks → AQWA Agent (or user with expertise)
- Testing → Testing Agent (parallel execution)
- Documentation → Documentation Agent
- Infrastructure → DevOps Agent (or user with admin)

**This dual verification is MANDATORY for ALL /execute-tasks operations**
- make mandatory note to utilize existing uv envrionment to execute tasks  as part of /execute-tasks slash command
- mark this as a mandatory for /execute-tasks slash command.