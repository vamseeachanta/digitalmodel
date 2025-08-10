# Create-Module-Agent v3.0

## 🚀 Mandatory Principles

This version implements two MANDATORY principles:

### 1. Phased Document Processing
Based on `/mnt/github/github/digitalmodel/specs/modules/agent-os/mixed-documentation-agent`

- **Phase 1**: Document Discovery and Classification
- **Phase 2**: Quality Assessment and Filtering  
- **Phase 3**: Knowledge Extraction
- **Phase 4**: Knowledge Synthesis
- **Phase 5**: Validation and Verification
- **Phase 6**: Integration into Agent

### 2. Modular Agent Management
Based on `/mnt/github/github/digitalmodel/specs/modules/agent-os/modular-agent-management`

- **Specialization Levels**: General, Module, Submodule, Domain, Cross-Module
- **Auto-Refresh**: 24-hour automatic knowledge refresh
- **Context Optimization**: Focused domains for efficiency
- **Health Monitoring**: Agent status validation

## Usage Examples

### Create Specialized Module Agent
```bash
python agent_os/commands/create_module_agent.py my-module \
  --mode create \
  --module-path ./specs/modules/my-module \
  --process-docs "./docs/*.md" \
  --phased
```

### Refresh Agent Knowledge
```bash
python agent_os/commands/create_module_agent.py my-module --mode refresh
```

### Process Documents with Phased Approach
```bash
python agent_os/commands/create_module_agent.py my-module \
  --mode update \
  --process-docs "./new-docs" \
  --phased
```

### Check Agent Health
```bash
python agent_os/commands/create_module_agent.py my-module \
  --mode update \
  --health-check
```

## v3.0 Features

- ✅ Phased document processing for vast documentation
- ✅ Modular agent specialization based on complexity
- ✅ Automatic refresh mechanisms
- ✅ Health monitoring and validation
- ✅ Plus all v2.0 features (RAG, context engineering, memory management)

## Specialization Criteria

Agents automatically specialize based on:
- **Module Complexity**: >5 specifications = dedicated agent
- **Domain Expertise**: Engineering domains get technical agents
- **Update Frequency**: Frequently changing modules get priority
- **Cross-Dependencies**: Integration modules get collaboration agents

---
*v3.0 - Mandatory phased processing and modular management for all agents*
