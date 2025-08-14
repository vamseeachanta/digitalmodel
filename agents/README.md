# AI Agents Directory

This directory contains specialized AI agents for various engineering and technical domains.

## Available Specialist Agents

### 1. **CAD Engineering Specialist** (`cad-engineering-specialist.md`)
- **Expertise**: CAD software, file format conversions, technical drawings
- **Specializations**: 
  - PDF to CAD conversions
  - Open-source CAD tools (FreeCAD, LibreCAD, OpenSCAD)
  - Proprietary systems (AutoCAD, SolidWorks, CATIA)
  - File formats (DWG, DXF, STEP, IGES, STL)
- **Use Cases**: Converting technical drawings, CAD automation, format interoperability

### 2. **Cathodic Protection Engineer** (`cathodic-protection-engineer.md`)
- **Expertise**: Corrosion prevention, electrical engineering for oil & gas
- **Specializations**:
  - Cathodic protection system design (SACP/ICCP)
  - NACE/ISO/DNV standards compliance
  - Pipeline integrity and coating assessments
  - Marine and offshore structure protection
- **Use Cases**: CP system design, corrosion mitigation, compliance analysis

## Module-Based Agents

### Domain-Specific Agents
- **AQWA** (`aqwa/`): Hydrodynamic analysis specialist
- **OrcaFlex** (`orcaflex/`): Marine dynamics and flexible riser analysis
- **Web Test Module** (`web-test-module/`): Web resource testing and validation

## Agent Structure

Each agent follows a standardized structure:

```yaml
---
name: agent-name
description: When to use this agent
model: sonnet/opus/haiku
color: display color
---

[Agent personality and expertise definition]
```

## How Agents Are Used

1. **Automatic Selection**: Claude automatically selects the appropriate specialist agent based on the task context
2. **Task Tool Integration**: When using the Task tool, the relevant `subagent_type` is selected
3. **Knowledge Accumulation**: Agents learn from project-specific tasks and documentation

## Creating New Agents

To create a new specialist agent:

```bash
# Using the create-module-agent command
python agent_os/commands/create_module_agent.py <agent-name>

# Or using the slash command
/create-module-agent <agent-name>
```

## Agent Management

- **Registry**: `agent-management/agent_registry.yaml` - Central registry of all agents
- **Templates**: `templates/` - Agent templates and configurations
- **Module Agents**: `module-agents/` - Agents specific to code modules
- **Submodule Agents**: `submodule-agents/` - Agents for submodules

## Best Practices

1. **Agent Selection**: Choose agents based on domain expertise required
2. **Agent Combination**: Multiple agents can collaborate on complex tasks
3. **Knowledge Updates**: Agents accumulate knowledge from completed tasks
4. **Documentation**: Keep agent descriptions updated as capabilities expand

---

*For more information on Agent OS, see `.agent-os/README.md`*