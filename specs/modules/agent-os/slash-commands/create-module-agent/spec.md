# Slash Command: /create-module-agent

> Spec: Create Module Agent Command
> Created: 2025-08-03
> Status: Active
> Version: 1.0.0

## Overview

The `/create-module-agent` slash command provides a streamlined interface for creating specialized AI agents within the Agent OS ecosystem. It automates the creation of module-specific agents with optimized context, documentation references, and integration with the enhanced create-specs workflow.

## Command Syntax

```
/create-module-agent <module_name> [options]
```

### Options
- `--type <agent_type>`: Specify agent type (default: general-purpose)
- `--repos <repo_list>`: Comma-separated list of repository sub-agents to include
- `--context-cache`: Enable context optimization and local caching (default: true)
- `--templates <template_list>`: Use predefined agent templates

## User Stories

### Developer Creating a New Module Agent

As a developer, I want to quickly create a specialized agent for my module, so that I can leverage AI capabilities specific to my domain without manual setup.

**Workflow:**
1. Execute `/create-module-agent finance-analytics`
2. System creates agent structure in `agents/finance-analytics/`
3. Agent references are automatically populated from repo and external sources
4. Context is optimized and cached locally for faster responses
5. Integration with enhanced create-specs workflow is established

### Team Lead Managing Multiple Agents

As a team lead, I want to create agents with specific repository access and documentation references, so that each agent has the appropriate context for its tasks.

**Workflow:**
1. Execute `/create-module-agent devops --repos assetutilities,pyproject-starter --type infrastructure`
2. System creates agent with cross-repository references
3. Documentation is automatically linked from specified repositories
4. Agent context includes infrastructure-specific patterns and best practices

## Spec Scope

1. **Agent Creation Workflow** - Automated creation of module-specific agents with proper folder structure
2. **Documentation Integration** - Automatic linking of repository and external documentation
3. **Context Optimization** - Local caching and optimization of agent context for performance
4. **Enhanced Workflow Integration** - Seamless integration with enhanced create-specs system
5. **Template System** - Support for predefined agent templates and configurations
6. **Workflow Refresh System** - Continuous spec updating and execution with learning capabilities

## Out of Scope

- Real-time agent communication protocols
- Cross-agent orchestration (handled by multi-agent system)
- Dynamic agent spawning during runtime
- Agent performance monitoring and analytics

## Expected Deliverable

1. Functional `/create-module-agent` slash command that creates complete agent structure
2. Generated agents with optimized context and documentation references
3. Integration with existing enhanced create-specs workflow for AI task execution
4. Workflow refresh system that continuously updates and improves specs based on execution

## Agent Structure

```
agents/
└── <module_name>/
    ├── agent.yaml                 # Agent configuration and metadata
    ├── context/
    │   ├── repository/            # Repository-specific context
    │   │   ├── internal.md        # Internal repo documentation
    │   │   └── references.yaml    # Repository references
    │   ├── external/              # External documentation
    │   │   ├── web_sources.yaml  # Web documentation links
    │   │   └── api_docs.md       # External API documentation
    │   └── optimized/             # Optimized and cached context
    │       ├── cache.json         # Cached context for faster loading
    │       └── embeddings.bin     # Vector embeddings for semantic search
    ├── templates/                 # Agent-specific templates
    │   ├── responses/             # Response templates
    │   └── prompts/               # Prompt templates
    └── workflows/                 # Integration workflows
        └── enhanced_specs.yaml    # Enhanced create-specs integration

```

## Technical Integration

### Repository Sub-Agents
The command will integrate with the 17 defined repository-specific sub-agents:
- aceengineer-website, aceengineercode, digitalmodel
- energy, rock-oil-field, saipem
- acma-projects, client_projects, investments
- teamresumes, assethold, assetutilities
- pyproject-starter, worldenergydata
- ai-native-traditional-eng, frontierdeepwater, OGManufacturing

### Context Optimization Strategy
1. **Initial Loading**: Gather documentation from specified sources
2. **Processing**: Extract relevant patterns, APIs, and domain knowledge
3. **Optimization**: Create embeddings and semantic indices
4. **Caching**: Store optimized context locally for fast retrieval
5. **Updates**: Periodic refresh based on repository changes

### Enhanced Create-Specs Integration
- Agents created via this command automatically integrate with:
  - Prompt evolution tracking
  - Executive summaries
  - Mermaid diagram generation
  - Task decomposition and tracking
  - Cross-repository referencing

## Spec Documentation

- Implementation Details: @specs/modules/agent-os/slash-commands/create-module-agent/sub-specs/implementation.md
- Agent Template System: @specs/modules/agent-os/slash-commands/create-module-agent/sub-specs/templates.md
- Context Optimization: @specs/modules/agent-os/slash-commands/create-module-agent/sub-specs/context-optimization.md
- Integration Guide: @specs/modules/agent-os/slash-commands/create-module-agent/sub-specs/integration.md
- Workflow Refresh System: @specs/modules/agent-os/slash-commands/create-module-agent/sub-specs/workflow-refresh.md