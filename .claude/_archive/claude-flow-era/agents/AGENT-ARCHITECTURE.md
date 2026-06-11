# Generic AI Agent Architecture

**Purpose:** Platform-agnostic agent architecture for any AI system (Claude, GPT, Gemini, LLaMA, etc.)
**Version:** 1.0
**Last Updated:** 2025-10-02

---

## Overview

This document defines a **generic, platform-agnostic architecture** for AI agents that can work across any AI system. The architecture uses standard file formats (YAML, JSON, Markdown) and follows common patterns that translate across different AI platforms.

---

## Core Principles

1. **Platform Agnostic**: Works with Claude Code, OpenAI Assistants, AutoGPT, LangChain, etc.
2. **Standard Formats**: YAML for config, JSON for data, Markdown for documentation
3. **Modular Design**: Agents are composable and reusable
4. **Clear Interfaces**: Well-defined inputs, outputs, and capabilities
5. **Version Controlled**: All agents tracked in git with history

---

## Directory Structure

```
agents/
├── AGENT-ARCHITECTURE.md           # This file - architecture guide
├── README.md                        # Agent library overview
├── index.yaml                       # Agent registry
│
├── specialized/                     # Domain-specific agents
│   ├── marine-engineering/
│   │   ├── excel-analyzer/
│   │   │   ├── agent.yaml          # Agent configuration
│   │   │   ├── prompts/            # Prompt templates
│   │   │   ├── tools.yaml          # Tool specifications
│   │   │   ├── examples/           # Usage examples
│   │   │   └── README.md           # Agent documentation
│   │   ├── mooring-analyst/
│   │   ├── wave-spectra-analyst/
│   │   └── README.md
│   ├── cad-engineering/
│   ├── aqwa/
│   ├── orcaflex/
│   └── freecad/
│
├── core/                            # Core development agents
│   ├── coder/
│   ├── reviewer/
│   ├── tester/
│   ├── planner/
│   └── researcher/
│
├── templates/                       # Agent templates
│   ├── base-agent.yaml
│   ├── specialized-agent.yaml
│   └── workflow-agent.yaml
│
└── agent-management/                # Agent orchestration
    ├── agent_registry.yaml
    ├── workflows/
    └── coordination/
```

---

## Agent Structure (Generic)

### Agent Configuration (`agent.yaml`)

```yaml
# Generic agent configuration - works with any AI platform
agent:
  # Metadata
  id: "marine-excel-analyzer-v1"
  name: "Marine Engineering Excel Analyzer"
  version: "1.0.0"
  created: "2025-10-02"
  author: "Engineering Team"

  # Classification
  type: "specialized"                # core | specialized | workflow | coordinator
  category: "marine-engineering"
  domain: "data-analysis"

  # Platform compatibility
  platforms:
    - claude-code                    # Claude Code (Anthropic)
    - openai-assistants             # OpenAI Assistants API
    - langchain                     # LangChain agents
    - autogen                       # Microsoft AutoGen
    - crewai                        # CrewAI framework
    - custom                        # Custom implementations

  # Core capabilities
  capabilities:
    - excel-analysis
    - formula-extraction
    - engineering-model-identification
    - documentation-generation
    - data-structure-extraction

  # Tool requirements (platform-agnostic names)
  tools:
    required:
      - file_read                   # Read files
      - file_write                  # Write files
      - code_execution              # Execute scripts
      - search                      # Search/grep functionality
    optional:
      - web_fetch                   # Web access (if needed)
      - database_query              # Database access

  # Trigger patterns (for automatic activation)
  triggers:
    keywords:
      - "analyze excel"
      - "marine engineering"
      - "extract formulas"
      - "excel analysis"
    file_patterns:
      - "*.xlsx"
      - "*.xlsm"
      - "*marine*.xls*"
    task_types:
      - "data-extraction"
      - "analysis"
      - "documentation"

  # Performance characteristics
  performance:
    typical_duration: "30-120 minutes"
    resource_usage: "medium"
    parallelizable: true
    caching: true

  # Dependencies
  dependencies:
    libraries:
      - openpyxl>=3.1.0
      - pandas>=2.0.0
      - numpy>=1.24.0
    agents:                         # Other agents this depends on
      - documentation-generator
      - code-formatter
    environment:
      python_version: ">=3.10"
      memory_min: "2GB"
```

### Tool Specifications (`tools.yaml`)

```yaml
# Generic tool specifications - maps to platform-specific tools
tools:
  file_read:
    description: "Read file contents"
    platforms:
      claude_code: "Read"
      openai: "code_interpreter.read_file"
      langchain: "FileReadTool"
      autogen: "read_file"
    parameters:
      - name: "file_path"
        type: "string"
        required: true
      - name: "encoding"
        type: "string"
        default: "utf-8"

  file_write:
    description: "Write content to file"
    platforms:
      claude_code: "Write"
      openai: "code_interpreter.write_file"
      langchain: "FileWriteTool"
      autogen: "write_file"
    parameters:
      - name: "file_path"
        type: "string"
        required: true
      - name: "content"
        type: "string"
        required: true

  code_execution:
    description: "Execute code/scripts"
    platforms:
      claude_code: "Bash"
      openai: "code_interpreter.execute"
      langchain: "PythonREPLTool"
      autogen: "execute_code"
    parameters:
      - name: "code"
        type: "string"
        required: true
      - name: "language"
        type: "string"
        default: "python"

  search:
    description: "Search/grep functionality"
    platforms:
      claude_code: "Grep"
      openai: "custom_search"
      langchain: "GrepTool"
      autogen: "search_files"
    parameters:
      - name: "pattern"
        type: "string"
        required: true
      - name: "path"
        type: "string"
        required: false
```

### Prompts (`prompts/system.md`)

```markdown
# System Prompt for {{agent.name}}

## Role
You are a {{agent.name}}, specialized in {{agent.category}}.

## Capabilities
{{#each agent.capabilities}}
- {{this}}
{{/each}}

## Instructions

### Primary Objective
{{agent.objective}}

### Workflow
1. {{step1}}
2. {{step2}}
3. {{step3}}

### Output Format
{{output_format}}

### Quality Standards
{{quality_standards}}

## Platform-Specific Adaptations

### For Claude Code
{{claude_specific_instructions}}

### For OpenAI
{{openai_specific_instructions}}

### For LangChain
{{langchain_specific_instructions}}

## Examples
{{examples}}
```

### Agent Documentation (`README.md`)

```markdown
# {{agent.name}}

**Version:** {{agent.version}}
**Type:** {{agent.type}}
**Category:** {{agent.category}}

## Purpose
{{agent.description}}

## Capabilities
{{agent.capabilities}}

## Supported Platforms
{{agent.platforms}}

## Usage

### Platform: Claude Code
\`\`\`bash
# Automatic trigger
User: "Analyze this Excel file: marine_data.xlsm"

# Manual invocation
/agent use "{{agent.id}}"
\`\`\`

### Platform: OpenAI Assistants
\`\`\`python
from openai import OpenAI
client = OpenAI()

assistant = client.beta.assistants.create(
    name="{{agent.name}}",
    instructions=open("prompts/system.md").read(),
    tools=[{"type": "code_interpreter"}],
    model="gpt-4-turbo-preview"
)
\`\`\`

### Platform: LangChain
\`\`\`python
from langchain.agents import initialize_agent, Tool
from langchain.llms import OpenAI

tools = load_tools_from_yaml("tools.yaml")
agent = initialize_agent(
    tools,
    OpenAI(),
    agent="zero-shot-react-description"
)
\`\`\`

## Examples
See `examples/` directory for usage examples.

## Dependencies
{{agent.dependencies}}

## Outputs
{{agent.outputs}}

## Performance
{{agent.performance}}
```

---

## Agent Registry (`index.yaml`)

```yaml
# Central registry of all agents
agents:
  specialized:
    marine-engineering:
      - id: marine-excel-analyzer-v1
        path: specialized/marine-engineering/excel-analyzer
        status: active
        platforms: [claude-code, openai, langchain]

      - id: mooring-analyst-v1
        path: specialized/marine-engineering/mooring-analyst
        status: planned
        platforms: [claude-code]

    cad-engineering:
      - id: freecad-specialist-v1
        path: specialized/cad-engineering/freecad
        status: active
        platforms: [claude-code]

  core:
    - id: coder-v1
      path: core/coder
      status: active
      platforms: [claude-code, openai, langchain]

    - id: reviewer-v1
      path: core/reviewer
      status: active
      platforms: [claude-code, openai]

# Workflows (agent combinations)
workflows:
  marine-excel-to-python:
    description: "Complete workflow: Excel analysis → Spec creation → Implementation"
    agents:
      - marine-excel-analyzer-v1
      - spec-generator-v1
      - python-coder-v1
      - validator-v1
    platforms: [claude-code]
```

---

## Platform Translation Layer

### Tool Mapping

| Generic Tool | Claude Code | OpenAI | LangChain | AutoGen |
|--------------|-------------|--------|-----------|---------|
| `file_read` | `Read` | `code_interpreter.read_file` | `FileReadTool` | `read_file` |
| `file_write` | `Write` | `code_interpreter.write_file` | `FileWriteTool` | `write_file` |
| `code_execution` | `Bash` | `code_interpreter.execute` | `PythonREPLTool` | `execute_code` |
| `search` | `Grep` | custom function | `GrepTool` | `search_files` |
| `web_fetch` | `WebFetch` | `browser` | `WebBaseTool` | `web_scraper` |

### Capability Translation

```yaml
# capabilities_mapping.yaml
capabilities:
  excel-analysis:
    claude_code:
      tools: [Read, Bash, Write]
      approach: "Execute Python script with openpyxl"

    openai:
      tools: [code_interpreter]
      approach: "Use code interpreter to analyze Excel"

    langchain:
      tools: [PythonREPLTool, FileReadTool]
      approach: "Chain tools for Excel processing"

  engineering-model-identification:
    all_platforms:
      approach: "Pattern matching in extracted data"
      requires: "Domain knowledge in system prompt"
```

---

## Workflow Coordination

### Simple Workflow

```yaml
# workflows/marine-excel-to-spec.yaml
workflow:
  id: "marine-excel-to-spec-v1"
  name: "Marine Excel to Specification"

  steps:
    - step: 1
      agent: "marine-excel-analyzer-v1"
      inputs:
        excel_file: "${input.excel_path}"
      outputs:
        analysis_report: "docs/excel_analysis.md"
        mapping_doc: "docs/feature_mapping.md"

    - step: 2
      agent: "spec-generator-v1"
      inputs:
        analysis: "${step1.analysis_report}"
        mapping: "${step1.mapping_doc}"
      outputs:
        specs: "specs/modules/marine-engineering/"

    - step: 3
      agent: "reviewer-v1"
      inputs:
        specs: "${step2.specs}"
      outputs:
        review: "docs/spec_review.md"

  parallel: false
  error_handling: "continue_on_error"
  timeout: "2 hours"
```

### Parallel Workflow

```yaml
# workflows/parallel-module-implementation.yaml
workflow:
  id: "parallel-implementation-v1"
  name: "Parallel Module Implementation"

  steps:
    - step: 1
      type: parallel
      agents:
        - id: "mooring-implementer"
          agent: "python-coder-v1"
          inputs:
            spec: "specs/mooring-analysis/"

        - id: "wave-implementer"
          agent: "python-coder-v1"
          inputs:
            spec: "specs/wave-spectra/"

        - id: "hydro-implementer"
          agent: "python-coder-v1"
          inputs:
            spec: "specs/hydrodynamic-coefficients/"

    - step: 2
      agent: "integration-tester-v1"
      inputs:
        modules: ["${step1.mooring}", "${step1.wave}", "${step1.hydro}"]
      outputs:
        test_results: "tests/integration_results.json"
```

---

## Best Practices

### 1. Agent Design

**DO:**
- ✅ Use clear, descriptive agent IDs
- ✅ Document all capabilities and limitations
- ✅ Provide usage examples for each platform
- ✅ Define explicit input/output contracts
- ✅ Version agents semantically (v1.0.0)

**DON'T:**
- ❌ Hard-code platform-specific tool names
- ❌ Mix multiple responsibilities in one agent
- ❌ Skip documentation
- ❌ Forget to specify dependencies

### 2. Tool Mapping

**DO:**
- ✅ Use generic tool names in agent config
- ✅ Maintain translation layer in `tools.yaml`
- ✅ Test tools on all target platforms
- ✅ Handle platform-specific limitations

**DON'T:**
- ❌ Assume all platforms have same tools
- ❌ Skip error handling for missing tools
- ❌ Hard-code tool parameters

### 3. Workflow Design

**DO:**
- ✅ Break complex workflows into steps
- ✅ Define clear data flow between steps
- ✅ Handle errors gracefully
- ✅ Make workflows resumable

**DON'T:**
- ❌ Create monolithic workflows
- ❌ Skip validation between steps
- ❌ Ignore timeout handling

---

## Platform-Specific Implementations

### Claude Code

```yaml
# claude-code implementation
platforms:
  claude_code:
    task_tool: true                 # Use Task tool for sub-agents
    tools:
      - Read
      - Write
      - Bash
      - Grep
      - Glob
    deployment: ".claude/agents/"
    format: "markdown"
```

### OpenAI Assistants

```yaml
# OpenAI implementation
platforms:
  openai:
    api_version: "v2"
    tools:
      - code_interpreter
      - retrieval
      - function_calling
    deployment: "via API"
    format: "json"
```

### LangChain

```yaml
# LangChain implementation
platforms:
  langchain:
    agent_type: "zero-shot-react-description"
    tools: [custom_tools]
    memory: "conversation_buffer"
    deployment: "python package"
    format: "python class"
```

---

## Migration Guide

### From Platform-Specific to Generic

1. **Extract Agent Configuration**
   ```yaml
   # From Claude-specific markdown
   # To generic agent.yaml
   ```

2. **Map Tools**
   ```yaml
   # Identify platform-specific tools
   # Create generic tool mappings in tools.yaml
   ```

3. **Generalize Prompts**
   ```markdown
   # Use template variables for platform adaptation
   # Maintain platform-specific sections when needed
   ```

4. **Test Across Platforms**
   ```bash
   # Validate on each target platform
   # Document platform-specific behaviors
   ```

---

## Future Enhancements

### Planned Features
- **Agent Marketplace**: Share/discover agents
- **Auto-translation**: Automatic platform conversion
- **Performance Monitoring**: Track agent effectiveness
- **Version Management**: Semantic versioning for agents
- **Testing Framework**: Automated agent testing
- **Deployment Automation**: CI/CD for agents

### Extensibility
- Support for new AI platforms (Gemini, LLaMA, etc.)
- Custom tool definitions
- Workflow orchestration enhancements
- Agent composition patterns
- Multi-agent collaboration protocols

---

## Examples

See `examples/` directory for:
- Platform-specific agent implementations
- Workflow examples
- Tool mapping examples
- Migration guides
- Best practices demos

---

**Architecture Status:** Active
**Maintainer:** Engineering Team
**Last Review:** 2025-10-02
