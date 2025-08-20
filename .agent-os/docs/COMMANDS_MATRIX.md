# Slash Commands Matrix

## Primary Commands and Subcommands Overview

| Command | Subcommands | Description | Features |
|---------|-------------|-------------|----------|
| **`/git`** | | **Git Operations** | |
| | `status` | Show status of all repositories | • Multi-repo status<br>• Uncommitted changes detection<br>• Branch information |
| | `sync` | Sync all repos with origin | • Auto-stash changes<br>• Pull latest<br>• Restore stashed changes<br>• **Auto-updates this documentation** |
| | `trunk` | Switch to trunk-based development | • Convert to main branch<br>• Clean old branches<br>• Setup trunk workflow |
| | `commit [msg]` | Commit changes | • Smart commit messages<br>• Auto-staging<br>• Push to origin |
| | `clean` | Clean branches and references | • Remove merged branches<br>• Prune remote refs<br>• Clean stale data |
| | `propagate` | Propagate slash commands to all repos | • Copy all commands<br>• Copy AI resources<br>• Update all repositories<br>• **Ensures command consistency** |
| | | | |
| **`/spec`** | | **Specification Management** | |
| | `create [name] [module]` | Create new specification | • AI template selection<br>• Module organization<br>• Agent recommendations<br>• Auto task generation |
| | `list` | List all specifications | • Show all specs<br>• Module grouping<br>• Status indicators |
| | `tasks [name]` | Show tasks for a spec | • Task breakdown<br>• Completion status<br>• Dependencies |
| | `templates` | Show available AI templates | • Claude Code Templates<br>• AITmpl templates<br>• Custom templates |
| | | | |
| **`/task`** | | **Task Execution** | |
| | `execute [id]` | Execute specific task | • UV environment usage<br>• Module-aware<br>• Progress tracking |
| | `execute --all` | Execute all pending tasks | • Batch execution<br>• Dependency order<br>• Auto-testing |
| | `status` | Show task completion status | • Progress metrics<br>• Module breakdown<br>• Blocking issues |
| | `verify` | Verify AI-generated work | • Code quality check<br>• Test coverage<br>• Standards compliance |
| | | | |
| **`/test`** | | **Testing Operations** | |
| | `run [module]` | Run tests (all or specific) | • UV Python usage<br>• Module-level testing<br>• Parallel execution |
| | `fix` | Auto-fix test failures | • Intelligent fixes<br>• Pattern recognition<br>• Safe modifications |
| | `summary` | Generate test summaries | • Module summaries<br>• Coverage reports<br>• Failure analysis |
| | `coverage` | Show coverage report | • Line coverage<br>• Branch coverage<br>• Module breakdown |
| | | | |
| **`/project`** | | **Project Management** | |
| | `status` | Overall project status | • Cross-repo status<br>• Progress metrics<br>• Health indicators |
| | `setup` | Initialize project structure | • Agent OS setup<br>• Module creation<br>• Standards application |
| | `optimize` | Run optimization agents | • Performance agents<br>• Code quality<br>• Bundle size |
| | `docs` | Generate documentation | • Auto-documentation<br>• API docs<br>• README generation |
| | | | |
| **`/data`** | | **Data Operations** | |
| | `context [folder]` | Generate engineering data context | • 25+ file formats<br>• Web research<br>• Module assignment |
| | `analyze` | Analyze data files | • Statistical analysis<br>• Pattern detection<br>• Visualizations |
| | `pipeline` | Create ETL pipelines | • Pipeline design<br>• Data validation<br>• Error handling |
| | `optimize` | Optimize data operations | • Query optimization<br>• Index recommendations<br>• Performance tuning |
| | | | |
| **`/ai-agent`** | | **AI Agent Management** | |
| | `list [--category]` | List all available agents | • 48+ agents<br>• Category filtering<br>• Capability display |
| | `recommend [context]` | Get agent recommendations | • Context analysis<br>• Auto-selection<br>• Workflow suggestions |
| | `use [agent]` | Activate specific agent | • Agent activation<br>• Task integration<br>• Guided usage |
| | `info [agent]` | Show agent details | • Capabilities<br>• Integration points<br>• Usage examples |
| | `workflow [type]` | Show agent workflows | • Step-by-step<br>• Agent chaining<br>• Best practices |

## Quick Reference Matrix

```
┌─────────────┬────────┬────────┬────────┬────────┬─────────┬───────────┐
│   Command   │ Sub 1  │ Sub 2  │ Sub 3  │ Sub 4  │  Sub 5  │   Sub 6   │
├─────────────┼────────┼────────┼────────┼────────┼─────────┼───────────┤
│ /git        │ status │ sync   │ trunk  │ commit │ clean   │ propagate │
│ /spec       │ create │ list   │ tasks  │template│    -    │     -     │
│ /task       │execute │exec-all│ status │ verify │    -    │     -     │
│ /test       │ run    │ fix    │summary │coverage│    -    │     -     │
│ /project    │ status │ setup  │optimize│ docs   │    -    │     -     │
│ /data       │context │analyze │pipeline│optimize│    -    │     -     │
│ /ai-agent   │ list   │recommend│ use   │ info   │workflow │     -     │
└─────────────┴────────┴────────┴────────┴────────┴─────────┴───────────┘
```

## Utility Commands

| Command | Description | Subcommands |
|---------|-------------|-------------|
| **`/uv-env`** | UV Environment Manager | `info`, `ensure`, `sync`, `add`, `enhance` |
| **`/verify`** | Verify AI Work | Standalone command for spec verification |

## Summary Statistics

- **7 Primary Commands**
- **31 Total Subcommands** (git now has 6 subcommands)
- **2 Utility Commands**
- **All with UV Environment Support**
- **48+ AI Agents Available**

## Feature Support Matrix

| Feature | /git | /spec | /task | /test | /project | /data | /ai-agent |
|---------|:----:|:-----:|:-----:|:-----:|:--------:|:-----:|:---------:|
| UV Environment | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Multi-repo | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| AI Integration | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Module Aware | - | ✓ | ✓ | ✓ | ✓ | ✓ | - |
| Auto-fix | - | - | ✓ | ✓ | ✓ | - | - |
| Web Research | - | ✓ | - | - | - | ✓ | - |

## Command Flow

```
/spec create → /ai-agent recommend → /task execute → /test run → /verify → /git commit
```

## Most Common Workflows

### Daily Development
```bash
/git sync --all          # Start your day
/task status            # Check progress
/test run               # Run tests
/git commit "updates"   # Commit work
```

### Feature Development
```bash
/spec create feature-name module-name
/ai-agent recommend
/task execute 1.1
/test run module-name
/verify
/git commit "Add feature-name"
```

### Data Processing
```bash
/data context ./data-folder
/data analyze
/data pipeline
/test run data-module
```

## Resources

- **AI Templates**: https://github.com/davila7/claude-code-templates
- **AITmpl**: https://www.aitmpl.com/
- **Agent Catalog**: `.agent-os/resources/aitmpl_agents_catalog.yaml`
- **Commands Location**: `.agent-os/commands/`

---

*Auto-generated: 2025-08-18*
*Distribution: This document is automatically updated and distributed via `/git sync --all`*
*Total Commands: 7 primary + 2 utility = 9 unified commands (consolidated from 21+)*
