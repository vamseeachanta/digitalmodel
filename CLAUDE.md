# Claude Code Configuration - SPARC Development Environment

## Plan Mode Convention (MANDATORY)
When using plan mode, save plan files to: `specs/modules/<module>/`
- `<module>` = relevant module name (e.g., orcaflex, orcawave, mooring, fatigue, structural)
- Example: `specs/modules/orcaflex/riser-analysis-plan.md`
- Example: `specs/modules/hydrodynamics/rao-import-plan.md`
- Example: `specs/modules/fatigue/sn-curve-plan.md`

**Templates**: Use `specs/templates/plan-template.md` (full) or `plan-template-minimal.md`

**Required Metadata**:
- `title`, `description`, `version`, `module` (core identification)
- `session.id`, `session.agent` (AI agent tracking for continuity)
- `status`, `progress`, `created`, `updated` (status tracking)
- `review` section with iteration tracking (cross-review process)

**Cross-Review Requirement (MANDATORY)**:
- Minimum **3 review iterations** with OpenAI Codex AND Google Gemini
- Both reviewers must approve before plan proceeds to implementation
- Document all feedback and changes in the Review Iteration Log
- Update `review.current_iteration` and `review.ready_for_next_step` status

---

## 🚨 CROSS-REVIEW POLICY (MANDATORY)

**ALL work performed by Claude Code or Google Gemini MUST be reviewed by OpenAI Codex.**

### Core Rule
Before presenting ANY work to the user:
1. **Commit changes** immediately after task completion
2. **Submit for Codex review** via post-commit hook
3. **Implement feedback** from Codex (maximum 3 iterations)
4. **Present to user** only after Codex approval OR 3 iterations complete

### Quick Reference
- Max iterations: 3
- Review scope: All code commits by Claude/Gemini
- Reviewer: OpenAI Codex
- Full policy: See workspace-hub @docs/modules/ai/CROSS_REVIEW_POLICY.md

---

## Interactive Engagement (MANDATORY)

**YOU MUST ask clarifying questions BEFORE starting any task.**

### Required Question Pattern

Before implementing ANY request, you MUST:
1. **Understand the requirement** - Ask about goals, constraints, and context
2. **Clarify ambiguities** - Identify and ask about unclear aspects
3. **Propose approach** - Describe your planned implementation strategy
4. **Wait for confirmation** - Get explicit approval before proceeding
5. **Ask follow-ups** - Continue dialogue as implementation reveals questions

### When to Ask Questions (ALWAYS)

- **Scope clarification**: What exactly should be included/excluded?
- **Technical choices**: Which approach/technology/pattern to use?
- **Edge cases**: How should unusual scenarios be handled?
- **Integration points**: How does this connect to existing code?
- **Testing requirements**: What tests are needed? What coverage?
- **Performance expectations**: Are there specific performance requirements?
- **Error handling**: How should errors be handled?
- **User experience**: If UI/UX involved, what's the desired interaction?

### Question Format

Present questions as:
- **Numbered lists** for multiple questions
- **Options with trade-offs** when choices exist
- **Specific scenarios** for concrete examples
- **Wait for response** - Never assume answers

### Example Interaction

**User**: "Add user authentication"

**Assistant**: "I have some questions before implementing authentication:

1. **Authentication method**: Which approach?
   - Option A: JWT tokens (stateless, scales well)
   - Option B: Session-based (simpler, server state)
   - Option C: OAuth (third-party providers)

2. **User storage**: Where to store user data?
   - Existing database?
   - New user table schema needed?

3. **Password security**: Requirements?
   - Hashing algorithm (bcrypt, argon2)?
   - Password complexity rules?

4. **Session management**: How long should sessions last?

5. **Error handling**: What should happen on failed login?

Please let me know your preferences for these decisions."

### Never Assume

- **Never guess** at requirements
- **Never implement** without explicit approval
- **Never skip** the question phase
- **Always engage** in dialogue before coding

## Codex Skills Link

Codex should be able to read the Claude skills stored under `.claude/skills`.
Create a Windows junction so Codex sees them under `C:\Users\ansystech\.codex\skills\claude`:

```
New-Item -ItemType Junction -Path C:\Users\ansystech\.codex\skills\claude -Target D:\workspace-hub\digitalmodel\.claude\skills
```


## 🚨 CRITICAL: CONCURRENT EXECUTION & FILE MANAGEMENT

**ABSOLUTE RULES**:
1. ALL operations MUST be concurrent/parallel in a single message
2. **NEVER save working files, text/mds and tests to the root folder**
3. ALWAYS organize files in appropriate subdirectories
4. **USE CLAUDE CODE'S TASK TOOL** for spawning agents concurrently, not just MCP

### ⚡ GOLDEN RULE: "1 MESSAGE = ALL RELATED OPERATIONS"

**MANDATORY PATTERNS:**
- **TodoWrite**: ALWAYS batch ALL todos in ONE call (5-10+ todos minimum)
- **Task tool (Claude Code)**: ALWAYS spawn ALL agents in ONE message with full instructions
- **File operations**: ALWAYS batch ALL reads/writes/edits in ONE message
- **Bash commands**: ALWAYS batch ALL terminal operations in ONE message
- **Memory operations**: ALWAYS batch ALL memory store/retrieve in ONE message

### 🎯 CRITICAL: Claude Code Task Tool for Agent Execution

**Claude Code's Task tool is the PRIMARY way to spawn agents:**
```javascript
// ✅ CORRECT: Use Claude Code's Task tool for parallel agent execution
[Single Message]:
  Task("Research agent", "Analyze requirements and patterns...", "researcher")
  Task("Coder agent", "Implement core features...", "coder")
  Task("Tester agent", "Create comprehensive tests...", "tester")
  Task("Reviewer agent", "Review code quality...", "reviewer")
  Task("Architect agent", "Design system architecture...", "system-architect")
```

**MCP tools are ONLY for coordination setup:**
- `mcp__claude-flow__swarm_init` - Initialize coordination topology
- `mcp__claude-flow__agent_spawn` - Define agent types for coordination
- `mcp__claude-flow__task_orchestrate` - Orchestrate high-level workflows

### 📁 File Organization Rules

**NEVER save to root folder. Use these directories:**
- `/src` - Source code files
- `/tests` - Test files
- `/docs` - Documentation and markdown files
- `/config` - Configuration files
- `/scripts` - Utility scripts
- `/examples` - Example code

## Project Overview

This project uses SPARC (Specification, Pseudocode, Architecture, Refinement, Completion) methodology with Claude-Flow orchestration for systematic Test-Driven Development.

## SPARC Commands


### 🤖 AI Folder Organization Responsibility

**After basic structure is established, AI manages file organization:**

1. **Recognize Need**: When 5+ files accumulate without clear organization
2. **Propose Structure**: Suggest module/domain-driven subfolder organization
3. **Wait for Approval**: Never create folders without user confirmation
4. **Execute**: Create folders, move files, update imports, commit

**Rules:**
- **Module-driven naming**: Use domain/business names (marine_analysis/, authentication/, data_processing/)
- **Maximum depth**: 5 levels of nesting
- **Consistency**: Same structure patterns across all 26 repositories
- **Always propose first**: Describe structure and rationale, wait for "yes"

**Example proposal:**
```
"I notice 12 files related to stress analysis. I propose:

marine_analysis/
├── stress/              # Stress calculations (4 files)
├── buckling/            # Buckling analysis (4 files)
└── fatigue/             # Fatigue analysis (4 files)

Should I proceed with this organization?"
```

**See full standards**: `docs/FILE_ORGANIZATION_STANDARDS.md`

### Core Commands
- `npx claude-flow sparc modes` - List available modes
- `npx claude-flow sparc run <mode> "<task>"` - Execute specific mode
- `npx claude-flow sparc tdd "<feature>"` - Run complete TDD workflow
- `npx claude-flow sparc info <mode>` - Get mode details

### Batchtools Commands
- `npx claude-flow sparc batch <modes> "<task>"` - Parallel execution
- `npx claude-flow sparc pipeline "<task>"` - Full pipeline processing
- `npx claude-flow sparc concurrent <mode> "<tasks-file>"` - Multi-task processing

### Build Commands
- `npm run build` - Build project
- `npm run test` - Run tests
- `npm run lint` - Linting
- `npm run typecheck` - Type checking

## SPARC Workflow Phases

1. **Specification** - Requirements analysis (`sparc run spec-pseudocode`)
2. **Pseudocode** - Algorithm design (`sparc run spec-pseudocode`)
3. **Architecture** - System design (`sparc run architect`)
4. **Refinement** - TDD implementation (`sparc tdd`)
5. **Completion** - Integration (`sparc run integration`)

## Code Style & Best Practices

- **Modular Design**: Files under 500 lines
- **Environment Safety**: Never hardcode secrets
- **Test-First**: Write tests before implementation
- **Clean Architecture**: Separate concerns
- **Documentation**: Keep updated

## 🚀 Available Agents (54 Total)

### Core Development
`coder`, `reviewer`, `tester`, `planner`, `researcher`

### Swarm Coordination
`hierarchical-coordinator`, `mesh-coordinator`, `adaptive-coordinator`, `collective-intelligence-coordinator`, `swarm-memory-manager`

### Consensus & Distributed
`byzantine-coordinator`, `raft-manager`, `gossip-coordinator`, `consensus-builder`, `crdt-synchronizer`, `quorum-manager`, `security-manager`

### Performance & Optimization
`perf-analyzer`, `performance-benchmarker`, `task-orchestrator`, `memory-coordinator`, `smart-agent`

### GitHub & Repository
`github-modes`, `pr-manager`, `code-review-swarm`, `issue-tracker`, `release-manager`, `workflow-automation`, `project-board-sync`, `repo-architect`, `multi-repo-swarm`

### SPARC Methodology
`sparc-coord`, `sparc-coder`, `specification`, `pseudocode`, `architecture`, `refinement`

### Specialized Development
`backend-dev`, `mobile-dev`, `ml-developer`, `cicd-engineer`, `api-docs`, `system-architect`, `code-analyzer`, `base-template-generator`

### Testing & Validation
`tdd-london-swarm`, `production-validator`

### Migration & Planning
`migration-planner`, `swarm-init`

## 🎯 Tool Responsibility Matrix

### The Division of Labor

```
┌─────────────────────────────────────────────────────────────────────┐
│                    TOOL RESPONSIBILITY MATRIX                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ╔═══════════════════════════════════════════════════════════════╗ │
│  ║  MCP TOOLS (claude-flow)         │  CLAUDE CODE TASK TOOL     ║ │
│  ║  Strategy & Coordination          │  Execution & Implementation║ │
│  ╠═══════════════════════════════════╪═══════════════════════════╣ │
│  ║  What: Define HOW to coordinate   │  What: DO the actual work ║ │
│  ║  When: Setup phase (optional)     │  When: Always for agents  ║ │
│  ║  Output: Coordination patterns    │  Output: Code, files, tests║ │
│  ╚═══════════════════════════════════╧═══════════════════════════╝ │
└─────────────────────────────────────────────────────────────────────┘
```

### MCP Tools (Coordination Only)

| Category | Tool | Purpose | Output |
|----------|------|---------|--------|
| **Topology** | `swarm_init` | Define mesh/hierarchical/ring/star patterns | Coordination structure |
| **Agent Types** | `agent_spawn` | Register agent types for coordination | Agent definitions |
| **Planning** | `task_orchestrate` | High-level workflow orchestration | Execution strategy |
| **Memory** | `memory_usage` | Store/retrieve shared state | Context persistence |
| **Monitoring** | `swarm_status` | Track coordination health | Status metrics |
| **Neural** | `neural_train` | Train pattern recognition | Learning models |
| **GitHub** | `github_swarm` | Repository coordination | Integration config |

**MCP tools define the "HOW" but do NOT execute code or create files.**

### Claude Code Task Tool (Execution Always)

| Category | Capability | Tool | Output |
|----------|-----------|------|--------|
| **Agent Spawning** | Spawn real agents that execute | `Task(name, desc, type)` | Running agents |
| **File Ops** | Read/Write/Edit code | Read, Write, Edit | Files created |
| **Code Gen** | Generate implementations | Task tool → agents code | Source code |
| **Terminal** | Run commands | Bash | Command output |
| **Testing** | Execute tests | Bash (pytest/jest) | Test results |
| **Git** | Commit/push/branch | Bash (git) | Version control |
| **Packages** | Install dependencies | Bash (uv/npm) | Installed packages |
| **Task Mgmt** | Track todos | TodoWrite | Todo list |

**Task tool executes the "WHAT" - it creates, modifies, and runs everything.**

### Decision Tree: Which Tool to Use?

```
                        ┌─────────────────────┐
                        │  Need to do work?   │
                        └──────────┬──────────┘
                                   │
                   ┌───────────────┴────────────────┐
                   │                                │
                   ▼                                ▼
         ┌──────────────────┐            ┌──────────────────┐
         │ Complex multi-   │            │ Simple task or   │
         │ agent workflow?  │            │ single agent?    │
         └────────┬─────────┘            └────────┬─────────┘
                  │                               │
         ┌────────┴────────┐                      │
         │                 │                      │
         ▼                 ▼                      ▼
    ┌─────────┐      ┌─────────┐          ┌─────────────┐
    │   YES   │      │   NO    │          │ TASK TOOL   │
    └────┬────┘      └────┬────┘          │   ONLY      │
         │                │               └─────────────┘
         ▼                ▼
    ┌─────────┐      ┌─────────┐
    │ STEP 1: │      │ TASK    │
    │   MCP   │      │ TOOL    │
    │ (setup) │      │ ONLY    │
    └────┬────┘      └─────────┘
         │
         ▼
    ┌─────────┐
    │ STEP 2: │
    │  TASK   │
    │  TOOL   │
    │ (execute)│
    └─────────┘
```

### Visual Workflow Diagram

```
┌────────────────────────────────────────────────────────────────────────┐
│                     AGENT EXECUTION WORKFLOW                           │
└────────────────────────────────────────────────────────────────────────┘

PHASE 1: COORDINATION SETUP (Optional - only for complex multi-agent work)
┌─────────────────────────────────────────────────────────────────────┐
│  [Single Message]                                                   │
│                                                                     │
│  mcp__claude-flow__swarm_init                                       │
│    ↓                                                                │
│  Creates: mesh/hierarchical/ring/star topology                      │
│    ↓                                                                │
│  mcp__claude-flow__agent_spawn (researcher, coder, tester...)       │
│    ↓                                                                │
│  Defines: Agent types and coordination patterns                     │
│    ↓                                                                │
│  mcp__claude-flow__memory_usage                                     │
│    ↓                                                                │
│  Stores: Shared context and coordination rules                      │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
                        CREATES COORDINATION
                           INFRASTRUCTURE
                                  ↓
PHASE 2: AGENT EXECUTION (Required - where actual work happens)
┌─────────────────────────────────────────────────────────────────────┐
│  [Single Message - All Parallel]                                    │
│                                                                     │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Task("Agent 1", "Full instructions...", "researcher")        │  │
│  │   ├─ Runs: pre-task hook (coordination)                      │  │
│  │   ├─ Executes: Research analysis                             │  │
│  │   ├─ Stores: Findings in memory                              │  │
│  │   └─ Runs: post-task hook (sync)                             │  │
│  └──────────────────────────────────────────────────────────────┘  │
│         ║ (Parallel)                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Task("Agent 2", "Full instructions...", "coder")             │  │
│  │   ├─ Runs: pre-task hook (coordination)                      │  │
│  │   ├─ Reads: Memory for context                               │  │
│  │   ├─ Writes: Code files                                      │  │
│  │   └─ Runs: post-task hook (sync)                             │  │
│  └──────────────────────────────────────────────────────────────┘  │
│         ║ (Parallel)                                               │
│  ┌──────────────────────────────────────────────────────────────┐  │
│  │ Task("Agent 3", "Full instructions...", "tester")            │  │
│  │   ├─ Runs: pre-task hook (coordination)                      │  │
│  │   ├─ Reads: Code from agent 2                                │  │
│  │   ├─ Writes: Test files                                      │  │
│  │   └─ Runs: post-task hook (sync)                             │  │
│  └──────────────────────────────────────────────────────────────┘  │
│                                                                     │
│  + TodoWrite { todos: [8-10 todos] }  (batched)                    │
│  + Write/Read file operations         (batched)                    │
│  + Bash commands                      (batched)                    │
└─────────────────────────────────────────────────────────────────────┘
```

### Key Principle: Separation of Concerns

| Concern | MCP Tools | Task Tool |
|---------|-----------|-----------|
| **What gets done** | ❌ No | ✅ Yes |
| **How to coordinate** | ✅ Yes | ❌ No |
| **Create files** | ❌ Never | ✅ Always |
| **Run code** | ❌ Never | ✅ Always |
| **Define topology** | ✅ Yes | ❌ No |
| **Store context** | ✅ Yes | ✅ Yes (via hooks) |
| **Execute agents** | ❌ No | ✅ Yes |

**CRITICAL**: MCP tools are OPTIONAL. Task tool is REQUIRED for all agent work.

## 🚀 Quick Setup

```bash
# Add MCP servers (Claude Flow required, others optional)
claude mcp add claude-flow npx claude-flow@alpha mcp start
claude mcp add ruv-swarm npx ruv-swarm mcp start  # Optional: Enhanced coordination
claude mcp add flow-nexus npx flow-nexus@latest mcp start  # Optional: Cloud features
```

## MCP Tool Categories

### Coordination
`swarm_init`, `agent_spawn`, `task_orchestrate`

### Monitoring
`swarm_status`, `agent_list`, `agent_metrics`, `task_status`, `task_results`

### Memory & Neural
`memory_usage`, `neural_status`, `neural_train`, `neural_patterns`

### GitHub Integration
`github_swarm`, `repo_analyze`, `pr_enhance`, `issue_triage`, `code_review`

### System
`benchmark_run`, `features_detect`, `swarm_monitor`

### Flow-Nexus MCP Tools (Optional Advanced Features)
Flow-Nexus extends MCP capabilities with 70+ cloud-based orchestration tools:

**Key MCP Tool Categories:**
- **Swarm & Agents**: `swarm_init`, `swarm_scale`, `agent_spawn`, `task_orchestrate`
- **Sandboxes**: `sandbox_create`, `sandbox_execute`, `sandbox_upload` (cloud execution)
- **Templates**: `template_list`, `template_deploy` (pre-built project templates)
- **Neural AI**: `neural_train`, `neural_patterns`, `seraphina_chat` (AI assistant)
- **GitHub**: `github_repo_analyze`, `github_pr_manage` (repository management)
- **Real-time**: `execution_stream_subscribe`, `realtime_subscribe` (live monitoring)
- **Storage**: `storage_upload`, `storage_list` (cloud file management)

**Authentication Required:**
- Register: `mcp__flow-nexus__user_register` or `npx flow-nexus@latest register`
- Login: `mcp__flow-nexus__user_login` or `npx flow-nexus@latest login`
- Access 70+ specialized MCP tools for advanced orchestration

## 🚀 Agent Execution Flow with Claude Code

### The Correct Pattern:

1. **Optional**: Use MCP tools to set up coordination topology
2. **REQUIRED**: Use Claude Code's Task tool to spawn agents that do actual work
3. **REQUIRED**: Each agent runs hooks for coordination
4. **REQUIRED**: Batch all operations in single messages

### Example Full-Stack Development:

```javascript
// Single message with all agent spawning via Claude Code's Task tool
[Parallel Agent Execution]:
  Task("Backend Developer", "Build REST API with Express. Use hooks for coordination.", "backend-dev")
  Task("Frontend Developer", "Create React UI. Coordinate with backend via memory.", "coder")
  Task("Database Architect", "Design PostgreSQL schema. Store schema in memory.", "code-analyzer")
  Task("Test Engineer", "Write Jest tests. Check memory for API contracts.", "tester")
  Task("DevOps Engineer", "Setup Docker and CI/CD. Document in memory.", "cicd-engineer")
  Task("Security Auditor", "Review authentication. Report findings via hooks.", "reviewer")
  
  // All todos batched together
  TodoWrite { todos: [...8-10 todos...] }
  
  // All file operations together
  Write "backend/server.js"
  Write "frontend/App.jsx"
  Write "database/schema.sql"
```

## 📋 Agent Coordination Protocol

### Conditional Hook Execution Pattern

**Hooks are ONLY required when:**
- Multiple agents coordinate (3+ agents)
- Cross-session memory needed
- Complex workflow tracking required
- Neural pattern training active

**Hooks are OPTIONAL when:**
- Single agent working alone
- Simple tasks (<30 min)
- No coordination needed
- Ad-hoc exploratory work

### Hook Execution Lifecycle

**1️⃣ BEFORE Work (Conditional):**
```bash
# Only if: Multi-agent coordination OR session restoration needed
if [[ $AGENT_COUNT -ge 3 ]] || [[ -n $SESSION_ID ]]; then
  npx claude-flow@alpha hooks pre-task --description "[task]"
  npx claude-flow@alpha hooks session-restore --session-id "swarm-[id]"
fi
```

**2️⃣ DURING Work (Conditional):**
```bash
# Only if: File changes need tracking OR memory coordination active
if [[ $TRACK_CHANGES == "true" ]] || [[ $MEMORY_COORDINATION == "active" ]]; then
  npx claude-flow@alpha hooks post-edit --file "[file]" --memory-key "swarm/[agent]/[step]"
  npx claude-flow@alpha hooks notify --message "[what was done]"
fi
```

**3️⃣ AFTER Work (Conditional):**
```bash
# Only if: Session metrics needed OR multi-agent workflow
if [[ $EXPORT_METRICS == "true" ]] || [[ $AGENT_COUNT -ge 3 ]]; then
  npx claude-flow@alpha hooks post-task --task-id "[task]"
  npx claude-flow@alpha hooks session-end --export-metrics true
fi
```

### Simplified Pattern for Simple Tasks

```javascript
// ✅ SIMPLE: Single agent, no hooks needed
Task("Quick Fix", "Fix typo in README.md", "coder")
// Agent executes directly, no coordination overhead

// ✅ COMPLEX: Multiple agents, hooks enabled
Task("Backend Dev", "Build API. Run pre-task/post-task hooks.", "backend-dev")
Task("Frontend Dev", "Build UI. Run pre-task/post-task hooks.", "coder")
Task("Tester", "Write tests. Run pre-task/post-task hooks.", "tester")
// Hooks coordinate between agents
```

### Memory Retention Policies

**SHORT-TERM MEMORY (Session-scoped)**
- **Duration**: Current conversation only
- **Namespace**: `swarm/session/[id]`
- **Storage**: In-memory (Claude Code context)
- **Use cases**: Temporary coordination, immediate context sharing
- **Cleanup**: Auto-cleared on conversation end

```javascript
// Store short-term coordination data
mcp__claude-flow__memory_usage {
  action: "store",
  key: "swarm/session/current/api-contract",
  namespace: "coordination",
  value: JSON.stringify({
    endpoints: ["/api/users", "/api/posts"],
    timestamp: Date.now(),
    ttl: "session"  // Cleared after conversation
  })
}
```

**MEDIUM-TERM MEMORY (Cross-session)**
- **Duration**: 24-72 hours
- **Namespace**: `swarm/shared/[feature]`
- **Storage**: File-based (`.claude-flow/memory/`)
- **Use cases**: Multi-session features, ongoing work
- **Cleanup**: Auto-expire after 72 hours

```javascript
// Store medium-term feature context
mcp__claude-flow__memory_usage {
  action: "store",
  key: "swarm/shared/auth-implementation",
  namespace: "coordination",
  value: JSON.stringify({
    status: "in-progress",
    decisions: ["Using JWT", "bcrypt for hashing"],
    files: ["src/auth.service.ts", "tests/auth.test.ts"],
    timestamp: Date.now(),
    ttl: "72h"  // Expires in 3 days
  })
}
```

**LONG-TERM MEMORY (Persistent knowledge)**
- **Duration**: Indefinite (until explicitly removed)
- **Namespace**: `swarm/knowledge/[domain]`
- **Storage**: Git-tracked (`.claude-flow/knowledge/`)
- **Use cases**: Patterns, standards, architectural decisions
- **Cleanup**: Manual only

```javascript
// Store long-term architectural decisions
mcp__claude-flow__memory_usage {
  action: "store",
  key: "swarm/knowledge/project-patterns",
  namespace: "coordination",
  value: JSON.stringify({
    patterns: {
      authentication: "JWT with refresh tokens",
      database: "Repository pattern with TypeORM",
      testing: "Jest with 80% coverage minimum",
      api: "RESTful with OpenAPI specs"
    },
    last_updated: Date.now(),
    ttl: "indefinite"  // Never auto-expires
  })
}
```

### Memory Access Patterns

**1. Check Before Store (Avoid Duplicates)**
```javascript
// Step 1: Check if memory exists
mcp__claude-flow__memory_usage {
  action: "retrieve",
  key: "swarm/shared/dependencies",
  namespace: "coordination"
}

// Step 2: Only store if not found or needs update
if (memory_not_found || needs_update) {
  mcp__claude-flow__memory_usage {
    action: "store",
    key: "swarm/shared/dependencies",
    namespace: "coordination",
    value: JSON.stringify({...})
  }
}
```

**2. Batch Memory Operations**
```javascript
// ✅ CORRECT: Single message with all memory ops
[Single Message]:
  mcp__claude-flow__memory_usage { action: "retrieve", key: "swarm/shared/api" }
  mcp__claude-flow__memory_usage { action: "retrieve", key: "swarm/shared/db" }
  mcp__claude-flow__memory_usage { action: "store", key: "swarm/session/status", ... }

// ❌ WRONG: Multiple messages
Message 1: retrieve api
Message 2: retrieve db
Message 3: store status
```

**3. Memory Cleanup (Scheduled)**
```bash
# Automatic cleanup runs daily
npx claude-flow@alpha hooks memory-cleanup --dry-run  # Preview
npx claude-flow@alpha hooks memory-cleanup --execute  # Run

# Manual cleanup
npx claude-flow@alpha hooks memory-cleanup --namespace "swarm/session/*"
npx claude-flow@alpha hooks memory-cleanup --older-than "7d"
```

### Memory Namespace Hierarchy

```
swarm/
├── session/          # SHORT-TERM (auto-cleanup on conversation end)
│   └── [id]/
│       ├── status
│       ├── context
│       └── temp-data
│
├── shared/           # MEDIUM-TERM (expires 72h)
│   ├── [feature]/
│   │   ├── decisions
│   │   ├── files
│   │   └── dependencies
│   └── [module]/
│       └── state
│
└── knowledge/        # LONG-TERM (persistent)
    ├── patterns/
    ├── standards/
    ├── architecture/
    └── best-practices/
```

## 🎯 Concurrent Execution Examples

### ✅ CORRECT WORKFLOW: MCP Coordinates, Claude Code Executes

```javascript
// Step 1: MCP tools set up coordination (optional, for complex tasks)
[Single Message - Coordination Setup]:
  mcp__claude-flow__swarm_init { topology: "mesh", maxAgents: 6 }
  mcp__claude-flow__agent_spawn { type: "researcher" }
  mcp__claude-flow__agent_spawn { type: "coder" }
  mcp__claude-flow__agent_spawn { type: "tester" }

// Step 2: Claude Code Task tool spawns ACTUAL agents that do the work
[Single Message - Parallel Agent Execution]:
  // Claude Code's Task tool spawns real agents concurrently
  Task("Research agent", "Analyze API requirements and best practices. Check memory for prior decisions.", "researcher")
  Task("Coder agent", "Implement REST endpoints with authentication. Coordinate via hooks.", "coder")
  Task("Database agent", "Design and implement database schema. Store decisions in memory.", "code-analyzer")
  Task("Tester agent", "Create comprehensive test suite with 90% coverage.", "tester")
  Task("Reviewer agent", "Review code quality and security. Document findings.", "reviewer")
  
  // Batch ALL todos in ONE call
  TodoWrite { todos: [
    {id: "1", content: "Research API patterns", status: "in_progress", priority: "high"},
    {id: "2", content: "Design database schema", status: "in_progress", priority: "high"},
    {id: "3", content: "Implement authentication", status: "pending", priority: "high"},
    {id: "4", content: "Build REST endpoints", status: "pending", priority: "high"},
    {id: "5", content: "Write unit tests", status: "pending", priority: "medium"},
    {id: "6", content: "Integration tests", status: "pending", priority: "medium"},
    {id: "7", content: "API documentation", status: "pending", priority: "low"},
    {id: "8", content: "Performance optimization", status: "pending", priority: "low"}
  ]}
  
  // Parallel file operations
  Bash "mkdir -p app/{src,tests,docs,config}"
  Write "app/package.json"
  Write "app/src/server.js"
  Write "app/tests/server.test.js"
  Write "app/docs/API.md"
```

### ❌ WRONG (Multiple Messages):
```javascript
Message 1: mcp__claude-flow__swarm_init
Message 2: Task("agent 1")
Message 3: TodoWrite { todos: [single todo] }
Message 4: Write "file.js"
// This breaks parallel coordination!
```

## Performance Benefits

- **84.8% SWE-Bench solve rate**
- **32.3% token reduction**
- **2.8-4.4x speed improvement**
- **27+ neural models**

## Hooks Integration

### Pre-Operation
- Auto-assign agents by file type
- Validate commands for safety
- Prepare resources automatically
- Optimize topology by complexity
- Cache searches

### Post-Operation
- Auto-format code
- Train neural patterns
- Update memory
- Analyze performance
- Track token usage

### Session Management
- Generate summaries
- Persist state
- Track metrics
- Restore context
- Export workflows

## Advanced Features (v2.0.0)

- 🚀 Automatic Topology Selection
- ⚡ Parallel Execution (2.8-4.4x speed)
- 🧠 Neural Training
- 📊 Bottleneck Analysis
- 🤖 Smart Auto-Spawning
- 🛡️ Self-Healing Workflows
- 💾 Cross-Session Memory
- 🔗 GitHub Integration

## Integration Tips

1. Start with basic swarm init
2. Scale agents gradually
3. Use memory for context
4. Monitor progress regularly
5. Train patterns from success
6. Enable hooks automation
7. Use GitHub tools first

## Support

- Documentation: https://github.com/ruvnet/claude-flow
- Issues: https://github.com/ruvnet/claude-flow/issues
- Flow-Nexus Platform: https://flow-nexus.ruv.io (registration required for cloud features)

---

## 📖 Quick Reference Card

```
╔════════════════════════════════════════════════════════════════╗
║              TOOL USAGE QUICK REFERENCE                        ║
╠════════════════════════════════════════════════════════════════╣
║                                                                ║
║  QUESTION: Do I need to execute code/create files?             ║
║  ├─ YES → Use Claude Code Task Tool (ALWAYS)                   ║
║  └─ NO  → Just planning? Skip tools entirely                   ║
║                                                                ║
║  QUESTION: Do I have 3+ agents coordinating?                   ║
║  ├─ YES → Optional: Use MCP for topology setup                 ║
║  │         Required: Task tool for execution                   ║
║  └─ NO  → Task tool only, skip MCP                             ║
║                                                                ║
║  QUESTION: Should I use hooks?                                 ║
║  ├─ Multi-agent (3+) → YES                                     ║
║  ├─ Cross-session memory → YES                                 ║
║  ├─ Complex workflow → YES                                     ║
║  └─ Simple/single agent → NO                                   ║
║                                                                ║
║  BATCHING CHECKLIST:                                           ║
║  ☑ All Task() calls in ONE message                             ║
║  ☑ All file ops (Read/Write/Edit) in ONE message               ║
║  ☑ All Bash commands in ONE message                            ║
║  ☑ All TodoWrite todos in ONE call (8-10 min)                  ║
║  ☑ All memory operations in ONE message                        ║
║                                                                ║
╚════════════════════════════════════════════════════════════════╝

MEMORY TTL QUICK GUIDE:
┌──────────────┬─────────────┬───────────────────────┐
│ Type         │ Duration    │ Namespace             │
├──────────────┼─────────────┼───────────────────────┤
│ Short-term   │ Session     │ swarm/session/[id]    │
│ Medium-term  │ 24-72 hours │ swarm/shared/[name]   │
│ Long-term    │ Indefinite  │ swarm/knowledge/[...]  │
└──────────────┴─────────────┴───────────────────────┘

TOOL DECISION TREE:
Need work done? → Task Tool (Required)
  ├─ 3+ agents? → MCP setup (Optional) + Task Tool (Required)
  └─ 1-2 agents? → Task Tool only

Files created? → Task Tool (ALWAYS)
Coordination? → MCP Tools (OPTIONAL)
```

---

Remember: **Claude Flow coordinates, Claude Code creates!**

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
Never save working files, text/mds and tests to the root folder.

## 📊 HTML Reporting Requirements

**MANDATORY FOR ALL MODULES:**

1. **Interactive Plots Only** - All visualizations MUST be interactive (Plotly, Bokeh, Altair, D3.js)
   - ❌ NO static matplotlib PNG/SVG exports
   - ✅ Interactive plots with hover, zoom, pan, export

2. **HTML Reports Required** - Every module MUST generate HTML reports
   - Analysis reports with visualizations
   - Performance dashboards
   - Data quality reports

3. **CSV Data Import** - Data MUST be imported from CSV with relative paths
   - Use relative paths from report location
   - Store CSVs in `/data/raw/`, `/data/processed/`, or `/data/results/`

**Technology Selection:**
- **General analysis:** Plotly (`plotly-visualization-agent`)
- **Dashboards:** Bokeh (`bokeh-dashboard-agent`)
- **Statistical:** Altair (`altair-analysis-agent`)
- **Custom viz:** D3.js (`d3js-custom-viz-agent`)

**See full standards:** `docs/HTML_REPORTING_STANDARDS.md`

## Automated Model Selection

**Quick Start:**
```bash
./scripts/monitoring/suggest_model.sh <repo-name> "<task description>"
```

**Example:**
```bash
./scripts/monitoring/suggest_model.sh digitalmodel "Implement user authentication"
```

**Check usage:**
```bash
./scripts/monitoring/check_claude_usage.sh today
```

**Full documentation:** See workspace-hub docs/AI_MODEL_SELECTION_AUTOMATION.md
