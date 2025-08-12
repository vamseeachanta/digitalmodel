# CLAUDE.md

## 🚀 MANDATORY: Slash Command Ecosystem Integration

**CRITICAL DIRECTIVE**: For EVERY improvement, automation, or repetitive task:

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

### Creating New Commands:
1. Implement in `.agent-os/commands/`
2. Test locally with `./slash_commands.py /command-name`
3. Sync to master with `/sync-all-commands`
4. Distribute with `/propagate-commands`

**Full Guidelines**: See MANDATORY_SLASH_COMMAND_ECOSYSTEM.md in assetutilities


This file provides guidance to AI assistants when working with code in this repository.

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

## Available Commands

- **Create-Module-Agent:** Available via `python tools/create-module-agent.py` command

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
5. **Markdown Compatibility**: Ensure ALL generated documents use markdown-compatible characters:
   - Escape angle brackets: `&lt;` `&gt;` instead of `<` `>`
   - Escape ampersands in text: `&amp;` instead of `&`
   - Escape Windows paths: `D:\\path\\file` instead of `D:\path\file`
   - Validate all special characters render properly in markdown processors
6. **Mathematical Expressions**: When technical specifications include formulas, equations, or mathematical notation:
   - Use KaTeX/LaTeX blocks for complex equations: `$$equation$$` for display mode, `$equation$` for inline
   - Simple format for AI: `$$F = ma$$` or `$\sigma = \frac{F}{A}$`
   - Include units in LaTeX format: `$$P_{wind} = \frac{1}{2}\rho C_d A v^2 \text{ [N]}$$`
   - Use for engineering calculations, statistical formulas, and mathematical relationships

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
- `/create-module-agent <agent-name>` - Create specialized AI agents

### Local Agent OS Structure
- **Standards**: @.agent-os/standards/ (code style, best practices)
- **Instructions**: @.agent-os/instructions/ (workflow guidance)
- **Product Context**: @.agent-os/product/ (mission, roadmap, decisions)
- **Specifications**: @.agent-os/specs/ (feature specifications and tasks)

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
