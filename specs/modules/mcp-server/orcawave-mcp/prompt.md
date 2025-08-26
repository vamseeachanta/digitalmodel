# OrcaWave MCP Server - Prompt Documentation

## Original User Request

Create OrcaWave MCP specification using the generic GUI template principles from `specs/modules/mcp-server/generic-gui-template`, incorporating module documentation and module agent capabilities.

## Context and Requirements Analysis

### Key Requirements Identified
1. **MCP Server for OrcaWave**: Enable Claude CLI to interact with OrcaWave hydrodynamic software
2. **Template-Based Approach**: Build upon the generic GUI template framework
3. **Module Integration**: Leverage existing OrcaWave module documentation and agent
4. **Domain Specificity**: Incorporate marine engineering and hydrodynamics expertise

### Existing Assets Reviewed
1. **Generic GUI Template** (`specs/modules/mcp-server/generic-gui-template/`)
   - FastMCP 2.0.5+ framework
   - Vision-based GUI automation
   - Multi-tier caching
   - Security hardening patterns

2. **OrcaWave Agent** (`agents/orcawave/`)
   - Domain expertise in hydrodynamics
   - Batch processing workflows
   - Performance targets established

3. **OrcaWave Module** (`src/modules/orcawave/diffraction/`)
   - Existing Python automation
   - Template-based configuration
   - Parallel processing capabilities
   - OrcaFlex integration patterns

## Design Decisions

### 1. Hybrid Control Architecture
**Decision**: Implement dual control via COM API and vision-based GUI
**Rationale**:
- COM API provides programmatic efficiency (10x faster)
- Vision provides verification and fallback
- Addresses COM API limitations with GUI monitoring
- Ensures reliability through redundancy

### 2. Domain-Specific Intelligence
**Decision**: Deep integration of hydrodynamic physics knowledge
**Rationale**:
- Mesh optimization requires domain expertise
- Frequency selection needs wave mechanics understanding
- Validation requires physics-based checks
- Users expect intelligent guidance

### 3. Parallel Batch Processing
**Decision**: Support concurrent analysis of multiple vessels
**Rationale**:
- Existing module shows 3x speedup with parallelization
- Common use case for fleet analysis
- License pooling enables efficiency
- Critical for production throughput

### 4. OrcaFlex-First Integration
**Decision**: Prioritize seamless OrcaFlex workflow
**Rationale**:
- Primary downstream consumer of results
- Existing vessel type format established
- Enables end-to-end automation
- Validated integration patterns available

### 5. Progressive Enhancement Strategy
**Decision**: Start with COM API, add vision progressively
**Rationale**:
- COM API faster to implement
- Vision adds verification layer
- Allows early testing
- Reduces initial complexity

## Technical Architecture Decisions

### COM API Strategy
```python
# Primary control through COM
app = win32com.client.Dispatch("OrcaWave.Application")
model = app.GetModel()

# Async wrapper for long operations
async def run_analysis_async():
    handle = model.StartAnalysis()
    while not handle.IsComplete():
        await asyncio.sleep(1)
    return handle.GetResults()
```

### Vision Verification Pattern
```python
# Visual verification of COM operations
async def verify_mesh_visually(com_mesh_data):
    screenshot = await capture_3d_view()
    visual_mesh = await analyze_mesh_visually(screenshot)
    discrepancies = compare_mesh_data(com_mesh_data, visual_mesh)
    if discrepancies:
        await reconcile_differences(discrepancies)
```

### Caching Strategy
- **L1 (Memory)**: Model configuration, mesh data
- **L2 (Redis)**: Analysis results, validation outcomes
- **L3 (Disk)**: Large result files, benchmark data

### Error Recovery Approach
1. **COM Exceptions**: Retry with exponential backoff
2. **GUI Desyncs**: Re-synchronize via screenshot
3. **License Issues**: Queue and retry
4. **Convergence Failures**: Adjust parameters automatically

## Implementation Strategy

### Phase Progression
1. **Foundation First**: Establish COM API control
2. **Intelligence Layer**: Add domain-specific algorithms
3. **Vision Enhancement**: Implement visual verification
4. **Automation**: Build end-to-end workflows
5. **Production**: Harden and optimize

### Testing Strategy
- **Unit Tests**: COM API wrappers, algorithms
- **Integration Tests**: COM-GUI synchronization
- **Domain Tests**: Physics validation
- **Performance Tests**: Batch processing throughput
- **Chaos Tests**: License failures, convergence issues

### Monitoring Approach
- **Real-time Progress**: WebSocket streaming
- **Performance Metrics**: Analysis times, queue depths
- **Quality Metrics**: Mesh scores, convergence rates
- **Business Metrics**: Daily throughput, error rates

## Agent Collaboration Model

### OrcaWave Agent (Primary)
- Owns hydrodynamic expertise
- Orchestrates analysis workflows
- Makes physics-based decisions
- Validates results

### Supporting Agents
```yaml
delegation_matrix:
  geometry_import:
    primary: cad_agent
    secondary: orcawave_agent
    
  mesh_optimization:
    primary: orcawave_agent
    advisor: cad_agent
    
  results_validation:
    primary: testing_agent
    context: orcawave_agent
    
  orcaflex_export:
    primary: orcaflex_agent
    source: orcawave_agent
```

## Success Criteria

### Technical Success
- COM API connection reliable (>99.9% uptime)
- Vision verification accurate (>95% match)
- Parallel processing efficient (3x speedup)
- Error recovery automatic (95% success)

### Business Success
- Setup time reduced 85% (2 min vs 15 min)
- Analysis throughput 10x (50-100 per day)
- Error rate reduced 90%
- Training time halved

### Quality Success
- Mesh quality consistently >0.85
- Physics validation 100% pass
- Benchmark accuracy within 5%
- Results reproducible

## Risk Analysis

### Identified Risks
1. **COM API Version Changes**: Mitigated by version detection
2. **License Server Issues**: Mitigated by queue management
3. **Complex Geometries**: Mitigated by CAD agent assistance
4. **Convergence Problems**: Mitigated by parameter tuning AI

### Contingency Plans
1. **COM Failure**: Full GUI automation fallback
2. **Vision API Limits**: Local model deployment
3. **Performance Issues**: Cloud scaling option
4. **Integration Blocks**: Manual export paths

## Lessons from Existing Modules

### From Generic GUI Template
- Multi-tier caching essential for performance
- Security hardening required from start
- Vision fallback increases reliability
- Parallel processing needs careful orchestration

### From OrcaWave Module
- Template-based configuration works well
- Parallel validation provides 3x speedup
- Error recovery mechanisms critical
- Integration tests prevent regressions

### From OrcaWave Agent
- Domain expertise drives value
- Workflow automation key differentiator
- Performance targets guide optimization
- User feedback shapes priorities

## Curated Reuse Prompt

To implement the OrcaWave MCP server based on this specification:

```
Implement the OrcaWave MCP server following the specification in specs/modules/mcp-server/orcawave-mcp/.

Start with Phase 1: Foundation & COM API
1. Setup project structure using the generic template
2. Implement OrcaWave COM API wrapper with error handling
3. Create basic model manipulation tools
4. Test COM connection and basic operations

Key implementation notes:
- Use pywin32 for COM interop
- Implement async wrappers for long operations
- Follow the hybrid control pattern (COM primary, vision verification)
- Reuse caching patterns from generic template
- Integrate with existing OrcaWave agent for domain logic

Reference implementations:
- Generic template: specs/modules/mcp-server/generic-gui-template/starter_implementation.py
- OrcaWave module: src/modules/orcawave/diffraction/orchestrator.py
- COM patterns: Use win32com.client.Dispatch()

Testing approach:
- Start with simple vessel creation
- Progress to geometry import
- Validate with mesh analysis
- Confirm with small diffraction run
```

## Module Agent Instructions

### For OrcaWave Agent
```yaml
responsibilities:
  - Own all hydrodynamic calculations
  - Validate mesh quality metrics
  - Select optimal analysis parameters
  - Verify physics compliance
  
interfaces:
  mcp_server:
    - Provide domain expertise
    - Validate user inputs
    - Suggest optimizations
    - Review results
    
  other_agents:
    - Request CAD healing from cad_agent
    - Send results to orcaflex_agent
    - Coordinate with testing_agent
```

### For MCP Development Agent
```yaml
tasks:
  - Implement FastMCP server framework
  - Create COM API integration layer
  - Setup vision verification system
  - Build caching infrastructure
  
patterns:
  - Follow generic template structure
  - Implement all security middleware
  - Use established error handling
  - Maintain audit trails
```

## References and Resources

### Technical Documentation
1. [OrcaWave Python API](https://www.orcina.com/webhelp/OrcFxAPI/)
2. [COM Automation Guide](https://www.orcina.com/resources/automation/)
3. [Panel Method Theory](https://www.orcina.com/resources/theory/)
4. [MCP Protocol Spec](https://modelcontextprotocol.io/)

### Internal Resources
1. Generic GUI Template: `specs/modules/mcp-server/generic-gui-template/`
2. OrcaWave Module: `src/modules/orcawave/diffraction/`
3. OrcaWave Agent: `agents/orcawave/`
4. Integration Examples: `specs/modules/orcaflex/orcawave-results-integration/`

### Key Files to Review
1. `starter_implementation.py` - Template MCP server
2. `orchestrator.py` - Workflow automation example
3. `agent_config.json` - Agent capabilities
4. `base_diffraction_config.yml` - Configuration patterns

## Next Steps

1. **Immediate Actions**
   - Setup development environment
   - Test COM API connection
   - Create project structure
   - Implement basic operations

2. **Week 1 Goals**
   - Complete COM API wrapper
   - Basic MCP server running
   - Simple vessel creation working
   - Initial tests passing

3. **Week 2 Goals**
   - Geometry import functional
   - Mesh analysis implemented
   - Vision verification added
   - Batch structure ready

## Conclusion

This specification combines the robustness of the generic GUI template with the domain expertise of the OrcaWave module and agent. The hybrid control approach (COM + Vision) provides both efficiency and reliability. The phased implementation allows for early testing while building toward production-ready capabilities. Success depends on leveraging existing patterns while adding marine engineering intelligence.