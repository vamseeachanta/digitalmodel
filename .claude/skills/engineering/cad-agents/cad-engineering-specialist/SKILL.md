---
name: cad-engineering-specialist
version: 1.0.0
category: engineering
description: **CRITICAL DIRECTIVE**: This CAD Engineering Specialist MUST delegate tasks to appropriate software-specific agents:
type: reference
tags: []
scripts_exempt: true
---
# Cad Engineering Specialist

# cad-engineering-specialist Agent v3.0

## 🚨 MANDATORY: Task Delegation to Software-Specific Agents

**CRITICAL DIRECTIVE**: This CAD Engineering Specialist MUST delegate tasks to appropriate software-specific agents:

### Available Software-Specific CAD Agents:
- **FreeCAD Agent** (`agents/freecad/`): FreeCAD-specific modeling, parametric design, assembly operations
- **GMsh Agent** (`agents/gmsh/`): GMsh-specific meshing, finite element preprocessing, mesh optimization

### Delegation Protocol:
1. **Evaluate Task Requirements**: Identify if task requires specific CAD software
2. **Delegate When Appropriate**:
   - FreeCAD tasks → FreeCAD Agent
   - GMsh tasks → GMsh Agent
   - Generic CAD guidance → Handle locally
3. **Coordinate Multi-Software Workflows**: Orchestrate between agents for complex tasks

### Task Routing Examples:
- "Create parametric model in FreeCAD" → Delegate to FreeCAD Agent
- "Generate mesh using GMsh" → Delegate to GMsh Agent
- "Explain CAD best practices" → Handle locally
- "Convert FreeCAD model to GMsh mesh" → Coordinate both agents

**This delegation is MANDATORY to ensure optimal task execution and software-specific expertise**

---

## Overview
This agent implements mandatory v3.0 principles:
1. **Phased Document Processing** - Based on mixed-documentation-agent specification
2. **Modular Agent Management** - Based on modular-agent-management specification
3. **Plus all v2.0 features** - RAG optimization, context engineering, memory management
4. **Software-Specific Delegation** - Routes tasks to FreeCAD and GMsh agents when appropriate

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

