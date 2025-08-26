# Generic MCP Server Template - Prompt Documentation

## Original User Request

Create a generic MCP server template spec to make Claude CLI interact with a custom GUI program with the following requirements:
- Template will be used to create custom program MCP by providing arguments for name, screenshot, etc.
- Research the best MCP template and use as starting point (FastMCP, TypeScript templates, etc.)
- Build MCP server in phased approach:
  - Utilize module agent, documentation and web research for custom program info/docs
  - Make refinements/improvements after getting access to custom program
  - Add basic tests (understanding menu, etc.)
  - Create documentation for module agent to use

## Research Findings

### MCP Framework Analysis
1. **FastMCP Python (jlowin)**: Production-ready framework with comprehensive features including deployment, auth, dynamic tool rewriting, and built-in testing. FastMCP 2.0 is the actively maintained version.

2. **FastMCP TypeScript (punkpeye)**: Supports latest MCP specifications with OAuth discovery endpoints and annotation support.

3. **Best Practices (2025)**:
   - Quick prototyping: FastAPI-MCP (Python) or EasyMCP (TypeScript)
   - Production-ready: FastMCP or MCP-Framework
   - Performance-critical: Foxy Contexts (Go) or Quarkus MCP (Java)

### GUI Automation Capabilities
1. **Desktop Automation**: RobotJS and PyAutoGUI for mouse/keyboard control
2. **Vision Integration**: GPT-4 Vision and Claude Vision for UI understanding
3. **Screen Capture**: Cross-platform screenshot capabilities
4. **Existing Solutions**: mcp-desktop-automation, screenshot_mcp_server, Peekaboo

## Design Decisions

### 1. Framework Selection
**Decision**: Use FastMCP Python as primary implementation
**Rationale**: 
- Most mature and feature-complete
- Excellent Python ecosystem integration
- Production-ready with deployment tools
- Strong community support

### 2. Architecture Pattern
**Decision**: Modular component architecture with agent delegation
**Rationale**:
- Enables specialized agents for different aspects
- Allows progressive enhancement
- Supports domain-specific customization
- Facilitates testing and maintenance

### 3. Vision Strategy
**Decision**: Multi-model support with fallback options
**Rationale**:
- GPT-4 Vision for primary analysis
- Claude Vision as alternative
- Local models for offline/secure environments
- Reduces vendor lock-in

### 4. Development Approach
**Decision**: Five-phase development with parallel streams
**Rationale**:
- Allows early testing and validation
- Enables parallel development
- Provides clear milestones
- Supports iterative refinement

## Implementation Strategy

### Phase 1: Foundation (Weeks 1-2)
- Setup FastMCP server skeleton
- Implement basic screen capture
- Create configuration system
- Add simple automation

### Phase 2: Vision Integration (Weeks 3-4)
- Integrate AI vision models
- Implement UI element detection
- Add OCR capabilities
- Create element mapping

### Phase 3: Agent Integration (Weeks 5-6)
- Implement agent discovery
- Create delegation protocol
- Add context management
- Enable inter-agent communication

### Phase 4: Customization (Weeks 7-8)
- Create program profiles
- Add program-specific tools
- Implement shortcuts
- Optimize performance

### Phase 5: Production (Weeks 9-10)
- Comprehensive testing
- Complete documentation
- Deployment automation
- Security hardening

## Key Technical Components

### MCP Protocol Implementation
```python
# Resources for state inspection
@mcp.resource("screenshot://current")
@mcp.resource("ui://elements")
@mcp.resource("state://application")

# Tools for interaction
@mcp.tool("navigate")
@mcp.tool("interact")
@mcp.tool("analyze")
@mcp.tool("execute_sequence")

# Prompts for common patterns
@mcp.prompt("understand_ui")
@mcp.prompt("find_element")
@mcp.prompt("verify_state")
```

### Configuration Schema
```yaml
program:
  name: "Custom Program"
  executable: "program.exe"
capture:
  mode: "window"
  format: "png"
vision:
  model: "gpt-4-vision"
automation:
  library: "pyautogui"
agents:
  - name: "domain-expert"
    capabilities: ["analysis"]
```

## Agent Delegation Strategy

### Primary Agents
1. **MCP Development Agent**: Server implementation
2. **GUI Automation Agent**: Screen interaction
3. **Vision Analysis Agent**: UI understanding
4. **Testing Agent**: Validation and verification
5. **Inter-Agent Coordinator**: Task delegation

### Supporting Agents
1. **DevOps Agent**: Infrastructure and deployment
2. **Documentation Agent**: User guides and API docs
3. **Security Agent**: Security auditing
4. **Performance Agent**: Optimization

## Success Metrics
1. New MCP server creation in < 1 hour
2. > 95% UI element identification accuracy
3. < 2 second response time for actions
4. Support for 90% of standard GUI patterns
5. > 80% code reuse across implementations

## Risks and Mitigations
1. **GUI changes**: Vision-based detection with fallbacks
2. **Performance**: Caching and batch processing
3. **Cross-platform**: Abstraction layers
4. **Security**: Sandboxing and validation

## Future Enhancements
1. Mobile application support
2. Web browser integration
3. Voice commands
4. Gesture recognition
5. Multi-monitor support
6. Recording/playback
7. Visual regression testing
8. Collaborative automation

## Curated Reuse Prompt

To create an MCP server for a specific GUI program using this template:

```
Create an MCP server for [PROGRAM_NAME] using the generic GUI template at specs/modules/mcp-server/generic-gui-template/. 

The program is a [PROGRAM_TYPE] application that [PROGRAM_DESCRIPTION].

Key requirements:
1. Use FastMCP Python framework
2. Implement screen capture for [PLATFORM]
3. Configure vision model for [UI_TYPE] interface
4. Create specific tools for:
   - [TOOL_1_DESCRIPTION]
   - [TOOL_2_DESCRIPTION]
   - [TOOL_3_DESCRIPTION]
5. Integrate with [AGENT_NAME] agent for domain expertise

Start with Phase 1 foundation setup, focusing on:
- Basic MCP server with health check
- Screenshot capture of main window
- Simple click and type actions
- YAML configuration for program profile

Reference the template structure and follow the phased approach defined in the specification.
```

## Template Usage Examples

### Example 1: CAD Software MCP
```bash
python create_mcp_server.py \
    --name "autocad" \
    --type "cad-software" \
    --screenshot "examples/autocad-main.png" \
    --config "profiles/cad-default.yml"
```

### Example 2: IDE Integration
```bash
python create_mcp_server.py \
    --name "vscode" \
    --type "ide" \
    --platform "cross-platform" \
    --vision-model "gpt-4-vision"
```

### Example 3: Office Suite
```bash
python create_mcp_server.py \
    --name "excel" \
    --type "spreadsheet" \
    --agents "data-analysis,reporting" \
    --shortcuts "config/excel-shortcuts.yml"
```

## Module Agent Documentation

### Agent Configuration
```yaml
# agents/mcp-server/agent.yaml
name: mcp-server
version: 1.0.0
capabilities:
  - gui_automation
  - vision_analysis
  - program_integration
  - template_generation
tools:
  - create_server
  - customize_profile
  - test_interaction
  - deploy_server
knowledge:
  - mcp_protocol
  - vision_apis
  - automation_libraries
  - gui_patterns
```

### Agent Usage
```python
# Using the MCP Server Agent
from agents.mcp_server import MCPServerAgent

agent = MCPServerAgent()
agent.create_server(
    program="my-app",
    template="enhanced",
    vision_model="claude-vision"
)
agent.test_interaction("click File menu")
agent.deploy_server(port=3000)
```

## References and Resources
1. [MCP Specification](https://modelcontextprotocol.io/)
2. [FastMCP Python Documentation](https://github.com/jlowin/fastmcp)
3. [FastMCP TypeScript Documentation](https://github.com/punkpeye/fastmcp)
4. [PyAutoGUI Documentation](https://pyautogui.readthedocs.io/)
5. [RobotJS Documentation](http://robotjs.io/)
6. [GPT-4 Vision API Guide](https://platform.openai.com/docs/guides/vision)
7. [Claude Vision Documentation](https://docs.anthropic.com/claude/docs/vision)
8. [MCP Desktop Automation Examples](https://github.com/tanob/mcp-desktop-automation)