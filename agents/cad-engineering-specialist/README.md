# cad-engineering-specialist Agent v3.0

## Overview
This agent implements mandatory v3.0 principles:
1. **Phased Document Processing** - Based on mixed-documentation-agent specification
2. **Modular Agent Management** - Based on modular-agent-management specification
3. **Plus all v2.0 features** - RAG optimization, context engineering, memory management

## Specialization: general-purpose
General-purpose agent with broad capabilities

## Features

### Phased Document Processing (v3.0)
- **Phase 1: Discovery** - Document inventory and classification
- **Phase 2: Quality Assessment** - Quality scoring and prioritization
- **Phase 3: Extraction** - Knowledge extraction with source tracking
- **Phase 4: Synthesis** - Conflict resolution and consolidation
- **Phase 5: Validation** - Consistency checks and quality assurance
- **Phase 6: Integration** - Agent knowledge integration

### Modular Management (v3.0)
- **Specialization Level**: general-purpose
- **Context Optimization**: 16000 tokens
- **Refresh Priority**: low
- **Auto-Refresh**: Enabled (7-day interval)

### Context Engineering (v2.0)
- **Layered Architecture**: Domain, operational, episodic, semantic, module, submodule
- **Memory Management**: Short-term and long-term with pruning
- **RAG Optimization**: Advanced chunking with phased strategy
- **Duplicate Detection**: SHA256-based content hashing

## Structure
```
cad-engineering-specialist/
├── agent.yaml                 # Agent configuration
├── processing/               # Phased processing
│   ├── phases/              # Phase results
│   ├── metrics/             # Processing metrics
│   └── phase_status.yaml    # Current status
├── context/                 # Context management
│   ├── docs_registry.yaml   # Documentation registry
│   ├── chunk_index.json     # Chunk index
│   ├── module/              # Module-specific docs
│   ├── submodule/           # Submodule-specific docs
│   └── [other layers]/      # Context layers
├── refresh/                 # Refresh mechanisms
├── prompts/                 # Agent prompts
├── templates/               # Reusable templates
├── tools/                   # Custom tools
├── scratchpad/              # Temporary workspace
└── validation/              # Quarantine and validation
```

## Usage

### Process Documents (Phased Approach)
```bash
python create_module_agent.py cad-engineering-specialist --mode update \
  --process-docs "/path/to/docs" --phased --module cad-engineering-specialist
```

### Refresh Agent Knowledge
```bash
python create_module_agent.py cad-engineering-specialist --mode refresh
```

### Add Documentation
```bash
python create_module_agent.py cad-engineering-specialist --mode update \
  --add-doc ./docs/guide.md --category module --title "Module Guide"
```

### Check Agent Health
```bash
python create_module_agent.py cad-engineering-specialist --mode update --health-check
```

## Metrics
- **Specialization**: general-purpose
- **Context Size**: 16000 tokens
- **Refresh Priority**: low
- **Created**: 2025-08-14T10:02:45.668134

---
*Enhanced Agent v3.0 - Implementing mandatory phased processing and modular management*
