# OrcaWave MCP Server Specification

## Overview
A specialized Model Context Protocol (MCP) server that enables Claude CLI to interact with OrcaWave hydrodynamic diffraction analysis software. This server provides AI-powered automation for wave-structure interaction modeling, mesh optimization, and hydrodynamic coefficient extraction, built upon the generic GUI template framework with domain-specific enhancements for marine engineering applications.

**Key Innovation**: Combines vision-based GUI automation with OrcaWave COM API for hybrid control, enabling both visual verification and programmatic execution of complex hydrodynamic analyses.

## Problem Statement
OrcaWave is a critical tool for offshore engineering hydrodynamic analysis, but its complex GUI and specialized workflows create significant barriers:
- **Steep Learning Curve**: Requires deep understanding of panel methods and wave mechanics
- **Manual Workflow**: Time-consuming setup for each vessel configuration
- **Error-Prone Process**: Manual mesh generation and parameter tuning
- **Limited Automation**: Native scripting requires extensive COM API knowledge
- **Integration Challenges**: Difficult to connect with OrcaFlex and other tools
- **Batch Processing**: No native support for parallel multi-vessel analysis

## Goals
1. **Enable AI-Assisted Analysis**: Claude can guide users through complex diffraction analyses
2. **Automate Workflows**: End-to-end automation from geometry import to results export
3. **Intelligent Mesh Optimization**: AI-driven mesh convergence and quality assessment
4. **Batch Processing**: Parallel execution of multiple vessel configurations
5. **Error Prevention**: Proactive validation and intelligent parameter suggestions
6. **Seamless Integration**: Direct export to OrcaFlex vessel types
7. **Knowledge Preservation**: Capture expert workflows in reusable templates
8. **Real-Time Monitoring**: Visual verification of analysis progress
9. **Compliance Automation**: Ensure API/DNV/ABS standards adherence

## Technical Architecture

### Hybrid Control Strategy
```
┌─────────────────────────────────────────────────────────┐
│                    Claude CLI                           │
└────────────────────┬────────────────────────────────────┘
                     │ MCP Protocol
┌────────────────────▼────────────────────────────────────┐
│               OrcaWave MCP Server                        │
├──────────────────────────────────────────────────────────┤
│  ┌──────────────────────────────────────────────────┐  │
│  │            Hybrid Control Layer                   │  │
│  │  ┌─────────────┐       ┌─────────────────────┐  │  │
│  │  │  COM API    │ ←───→ │  Vision-Based GUI   │  │  │
│  │  │  Controller │       │  Automation         │  │  │
│  │  └─────────────┘       └─────────────────────┘  │  │
│  └──────────────────────────────────────────────────┘  │
│                                                          │
│  ┌──────────────────────────────────────────────────┐  │
│  │         Domain-Specific Components                │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────────┐  │  │
│  │  │   Mesh   │  │  Wave    │  │ Hydrodynamic │  │  │
│  │  │ Analyzer │  │ Physics  │  │  Validator   │  │  │
│  │  └──────────┘  └──────────┘  └──────────────┘  │  │
│  └──────────────────────────────────────────────────┘  │
│                                                          │
│  ┌──────────────────────────────────────────────────┐  │
│  │           Integration Layer                       │  │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────────┐  │  │
│  │  │ OrcaFlex │  │   AQWA   │  │    Excel     │  │  │
│  │  │  Export  │  │ Benchmark│  │   Reporter   │  │  │
│  │  └──────────┘  └──────────┘  └──────────────┘  │  │
│  └──────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│                    OrcaWave Application                  │
└──────────────────────────────────────────────────────────┘
```

### Core Components

#### 1. OrcaWave COM API Controller
- **Direct API Access**: Programmatic control via COM interface
- **Object Model**: Full access to OrcaWave data structures
- **Batch Operations**: Efficient bulk parameter updates
- **Error Handling**: Structured exception management
- **Performance**: 10x faster than GUI for data operations

#### 2. Vision-Based GUI Monitor
- **Visual Verification**: Confirm GUI state matches API state
- **Progress Monitoring**: Track analysis convergence visually
- **Error Detection**: Identify GUI-specific issues and warnings
- **Mesh Visualization**: Capture and analyze mesh quality
- **Results Validation**: Visual comparison of plots and graphs

#### 3. Mesh Analysis Engine
- **Quality Metrics**: Aspect ratio, skewness, orthogonality
- **Convergence Testing**: Automated mesh refinement studies
- **Panel Optimization**: Intelligent panel distribution
- **Symmetry Detection**: Automatic symmetry plane identification
- **Waterline Refinement**: Enhanced discretization at free surface

#### 4. Wave Physics Module
- **Frequency Selection**: Optimal frequency range determination
- **Direction Optimization**: Intelligent wave heading selection
- **Deep/Shallow Water**: Automatic depth regime detection
- **Irregular Wave**: JONSWAP/PM spectrum configuration
- **QTF Calculation**: Full/Newman/Pressure QTF selection

#### 5. Hydrodynamic Validator
- **Reciprocity Check**: Validate radiation-diffraction relations
- **Energy Conservation**: Verify power balance
- **Asymptotic Behavior**: Check high/low frequency limits
- **Benchmark Comparison**: Validate against known solutions
- **Unit Consistency**: Ensure dimensional correctness

## MCP Protocol Implementation

### Resources
```python
@mcp.resource("orcawave://model/current")
async def get_current_model(ctx: Context) -> Dict:
    """Get current OrcaWave model configuration"""
    return {
        "vessel": await self.com_api.get_vessel_data(),
        "mesh": await self.com_api.get_mesh_statistics(),
        "environment": await self.com_api.get_environment(),
        "analysis_type": await self.com_api.get_analysis_type(),
        "frequency_range": await self.com_api.get_frequencies(),
        "directions": await self.com_api.get_directions()
    }

@mcp.resource("orcawave://mesh/quality")
async def get_mesh_quality(ctx: Context) -> Dict:
    """Analyze mesh quality metrics"""
    mesh_data = await self.com_api.get_mesh()
    return {
        "panel_count": mesh_data.panel_count,
        "quality_metrics": {
            "aspect_ratio": calculate_aspect_ratios(mesh_data),
            "skewness": calculate_skewness(mesh_data),
            "size_variation": calculate_size_variation(mesh_data)
        },
        "waterline_panels": count_waterline_panels(mesh_data),
        "symmetry": detect_symmetry(mesh_data),
        "recommendations": generate_mesh_recommendations(mesh_data)
    }

@mcp.resource("orcawave://results/hydrodynamics")
async def get_hydrodynamic_results(ctx: Context) -> Dict:
    """Extract hydrodynamic coefficients"""
    return {
        "added_mass": await self.com_api.get_added_mass(),
        "damping": await self.com_api.get_damping(),
        "excitation": await self.com_api.get_excitation_forces(),
        "raos": await self.com_api.get_raos(),
        "qtf": await self.com_api.get_qtf_data(),
        "mean_drift": await self.com_api.get_mean_drift_forces()
    }

@mcp.resource("orcawave://validation/status")
async def get_validation_status(ctx: Context) -> Dict:
    """Get validation and convergence status"""
    return {
        "convergence": await check_convergence(),
        "reciprocity": await validate_reciprocity(),
        "energy_conservation": await check_energy_balance(),
        "benchmark_comparison": await compare_with_benchmarks(),
        "warnings": await get_analysis_warnings(),
        "recommendations": await generate_recommendations()
    }
```

### Tools
```python
@mcp.tool("create_vessel")
async def create_vessel(
    name: str,
    vessel_type: str,
    dimensions: Dict,
    ctx: Context = None
) -> Dict:
    """Create new vessel with specifications"""
    vessel = await self.com_api.create_vessel(
        name=name,
        vessel_type=vessel_type,
        loa=dimensions["length"],
        beam=dimensions["beam"],
        draft=dimensions["draft"]
    )
    return {
        "success": True,
        "vessel_id": vessel.id,
        "mesh_recommendation": generate_mesh_params(dimensions)
    }

@mcp.tool("import_geometry")
async def import_geometry(
    file_path: str,
    file_type: str,
    auto_heal: bool = True,
    ctx: Context = None
) -> Dict:
    """Import vessel geometry from CAD file"""
    # Validate geometry file
    validation = await validate_geometry_file(file_path, file_type)
    if not validation["valid"]:
        return {"success": False, "errors": validation["errors"]}
    
    # Import via COM API
    result = await self.com_api.import_geometry(
        file_path,
        file_type,
        auto_heal=auto_heal
    )
    
    # Visual verification
    screenshot = await capture_3d_view()
    visual_check = await verify_geometry_visually(screenshot)
    
    return {
        "success": result.success,
        "panel_count": result.panel_count,
        "visual_verification": visual_check,
        "warnings": result.warnings
    }

@mcp.tool("optimize_mesh")
async def optimize_mesh(
    target_panels: Optional[int] = None,
    convergence_study: bool = False,
    ctx: Context = None
) -> Dict:
    """Optimize mesh for analysis accuracy"""
    current_mesh = await self.com_api.get_mesh()
    
    # AI-driven optimization
    optimization_plan = await self.mesh_analyzer.create_optimization_plan(
        current_mesh,
        target_panels=target_panels
    )
    
    if convergence_study:
        results = []
        for refinement in optimization_plan["refinements"]:
            await self.com_api.refine_mesh(refinement)
            await self.com_api.run_analysis()
            results.append(await get_key_results())
        
        convergence = analyze_convergence(results)
        return {
            "success": True,
            "optimal_mesh": convergence["optimal"],
            "convergence_plot": convergence["plot_data"]
        }
    else:
        await self.com_api.apply_mesh_settings(optimization_plan["optimal"])
        return {
            "success": True,
            "mesh_quality": await get_mesh_quality(ctx)
        }

@mcp.tool("run_analysis")
async def run_analysis(
    analysis_type: str = "diffraction",
    monitor_progress: bool = True,
    ctx: Context = None
) -> Dict:
    """Execute hydrodynamic analysis with monitoring"""
    # Configure analysis
    await self.com_api.set_analysis_type(analysis_type)
    
    # Start analysis
    analysis_handle = await self.com_api.start_analysis_async()
    
    # Monitor progress
    if monitor_progress:
        progress_data = []
        while not analysis_handle.is_complete():
            progress = await analysis_handle.get_progress()
            visual_state = await capture_progress_window()
            progress_data.append({
                "percentage": progress.percentage,
                "current_frequency": progress.current_frequency,
                "convergence": await check_convergence_visually(visual_state)
            })
            await asyncio.sleep(1)
    
    # Get results
    results = await analysis_handle.get_results()
    validation = await validate_results(results)
    
    return {
        "success": results.success,
        "computation_time": results.elapsed_time,
        "validation": validation,
        "key_results": extract_key_results(results)
    }

@mcp.tool("export_to_orcaflex")
async def export_to_orcaflex(
    vessel_name: str,
    include_qtf: bool = True,
    simplified: bool = False,
    ctx: Context = None
) -> Dict:
    """Export results as OrcaFlex vessel type"""
    # Get hydrodynamic data
    hydro_data = await get_hydrodynamic_results(ctx)
    
    # Convert to OrcaFlex format
    vessel_type = create_orcaflex_vessel_type(
        vessel_name,
        hydro_data,
        include_qtf=include_qtf,
        simplified=simplified
    )
    
    # Save vessel type file
    output_path = f"results/{vessel_name}_vessel_type.yml"
    await save_vessel_type(vessel_type, output_path)
    
    # Validate in OrcaFlex
    validation = await validate_in_orcaflex(output_path)
    
    return {
        "success": True,
        "output_file": output_path,
        "validation": validation,
        "statistics": {
            "frequencies": len(hydro_data["added_mass"]["frequencies"]),
            "directions": len(hydro_data["excitation"]["directions"]),
            "qtf_included": include_qtf
        }
    }

@mcp.tool("batch_analysis")
async def batch_analysis(
    vessel_configs: List[Dict],
    parallel: bool = True,
    ctx: Context = None
) -> Dict:
    """Run batch analysis for multiple vessels"""
    results = []
    
    if parallel:
        # Create worker pool
        tasks = []
        for config in vessel_configs:
            task = asyncio.create_task(
                run_single_vessel_analysis(config)
            )
            tasks.append(task)
        
        # Execute in parallel (max 3 concurrent)
        for batch in chunk_tasks(tasks, 3):
            batch_results = await asyncio.gather(*batch)
            results.extend(batch_results)
    else:
        for config in vessel_configs:
            result = await run_single_vessel_analysis(config)
            results.append(result)
    
    # Generate comparison report
    comparison = generate_comparison_report(results)
    
    return {
        "success": all(r["success"] for r in results),
        "vessels_analyzed": len(results),
        "total_time": sum(r["time"] for r in results),
        "comparison": comparison,
        "results": results
    }
```

### Prompts
```python
@mcp.prompt("analyze_mesh")
def analyze_mesh_prompt() -> str:
    """Prompt for mesh quality analysis"""
    return """Analyze the OrcaWave mesh and provide:
    1. Overall mesh quality assessment (poor/fair/good/excellent)
    2. Problem areas requiring refinement
    3. Panel distribution analysis
    4. Waterline discretization adequacy
    5. Symmetry utilization opportunities
    6. Specific recommendations for improvement
    Format as structured JSON with actionable insights."""

@mcp.prompt("suggest_frequencies")
def suggest_frequencies_prompt(vessel_data: Dict) -> str:
    """Generate optimal frequency range suggestion"""
    return f"""Based on vessel characteristics:
    - Length: {vessel_data['length']}m
    - Beam: {vessel_data['beam']}m
    - Draft: {vessel_data['draft']}m
    - Water depth: {vessel_data['water_depth']}m
    
    Suggest optimal frequency range for diffraction analysis:
    1. Minimum frequency (capture long waves)
    2. Maximum frequency (capture short waves)
    3. Number of frequencies for good resolution
    4. Critical frequencies to include
    5. Reasoning for selections"""

@mcp.prompt("validate_results")
def validate_results_prompt(results: Dict) -> str:
    """Validate hydrodynamic results"""
    return f"""Validate these hydrodynamic results:
    Added mass at ω=0: {results['added_mass_zero']}
    Damping peak: {results['damping_peak']}
    RAO at resonance: {results['rao_resonance']}
    
    Check for:
    1. Physical plausibility
    2. Expected trends with frequency
    3. Symmetry in appropriate DOFs
    4. Comparison with similar vessels
    5. Any anomalies requiring investigation"""
```

## Configuration Schema

```yaml
# config/orcawave-mcp.yml
server:
  name: "orcawave-mcp-server"
  version: "1.0.0"
  port: 3100
  
orcawave:
  installation_path: "C:/Program Files/Orcina/OrcaWave"
  version: "11.0"
  license_server: "localhost:5053"
  com_timeout: 30000  # milliseconds
  
api_mode:
  primary: "com"  # com or gui
  fallback: "gui"
  hybrid_validation: true
  
analysis:
  default_type: "diffraction"
  convergence_tolerance: 0.01
  max_iterations: 100
  parallel_frequencies: true
  
mesh:
  target_quality: 0.8  # 0-1 scale
  max_aspect_ratio: 3.0
  waterline_refinement: 2.0
  auto_optimize: true
  
integration:
  orcaflex:
    export_format: "vessel_type"
    include_qtf: true
    compact_format: false
  excel:
    template: "templates/hydro_report.xlsx"
    auto_charts: true
  aqwa:
    benchmark_database: "benchmarks/aqwa_cases.db"
    tolerance: 0.05
    
performance:
  cache_results: true
  cache_ttl: 3600
  max_parallel_analyses: 3
  gpu_acceleration: false
  
validation:
  reciprocity_check: true
  energy_check: true
  asymptotic_check: true
  benchmark_comparison: true
  
monitoring:
  screenshot_interval: 5  # seconds
  progress_tracking: true
  error_screenshots: true
  visual_validation: true
```

## Development Phases

### Phase 1: Foundation & COM API (Week 1-2)
1. Setup FastMCP server framework
2. Implement OrcaWave COM API wrapper
3. Create basic model manipulation tools
4. Establish error handling patterns
5. Implement configuration system

### Phase 2: Vision Integration (Week 3-4)
1. Implement screen capture for OrcaWave
2. Create 3D view analyzer
3. Add progress monitoring
4. Implement visual validation
5. Create mesh quality visualizer

### Phase 3: Domain Intelligence (Week 5-6)
1. Implement mesh optimization algorithms
2. Create frequency selection logic
3. Add convergence analysis
4. Implement validation checks
5. Create benchmark comparison

### Phase 4: Workflow Automation (Week 7-8)
1. Create end-to-end workflows
2. Implement batch processing
3. Add parallel execution
4. Create template system
5. Implement error recovery

### Phase 5: Integration & Testing (Week 9-10)
1. Complete OrcaFlex integration
2. Add Excel reporting
3. Implement AQWA benchmarking
4. Comprehensive testing
5. Performance optimization

## Success Metrics

### Performance Targets
- **Analysis Setup**: < 2 minutes (vs 15-30 minutes manual)
- **Mesh Optimization**: < 5 minutes automated convergence
- **Single Analysis**: 10-60 seconds (same as native)
- **Batch Processing**: 3x speedup with parallel execution
- **Results Export**: < 30 seconds to OrcaFlex format

### Quality Metrics
- **Mesh Quality Score**: > 0.85 for all analyses
- **Convergence Achievement**: 99% of analyses converge
- **Validation Pass Rate**: 100% reciprocity/energy checks
- **Benchmark Accuracy**: Within 5% of reference solutions
- **Error Recovery**: 95% automatic recovery from failures

### Business Impact
- **Time Savings**: 80% reduction in analysis setup time
- **Error Reduction**: 90% fewer manual errors
- **Knowledge Capture**: 100+ reusable workflow templates
- **Training Time**: 50% reduction for new users
- **Throughput**: 10x increase in daily analyses

## Risk Mitigation

### Technical Risks
1. **COM API Limitations**
   - Mitigation: Hybrid approach with GUI fallback
   - Visual verification of COM operations
   
2. **License Constraints**
   - Mitigation: Intelligent queue management
   - License pooling for batch operations
   
3. **Version Compatibility**
   - Mitigation: Multi-version support
   - Graceful degradation for older versions

### Operational Risks
1. **Complex Geometries**
   - Mitigation: Advanced mesh healing algorithms
   - Manual intervention protocols
   
2. **Convergence Issues**
   - Mitigation: Adaptive relaxation factors
   - Expert system for parameter tuning

## Agent Delegation

### Primary Agent
- **OrcaWave Agent**: Domain expertise and workflow orchestration

### Supporting Agents
- **CAD Agent**: Geometry preparation and healing
- **Testing Agent**: Validation and benchmarking
- **Documentation Agent**: Report generation
- **OrcaFlex Agent**: Integration and co-simulation

### Delegation Rules
```yaml
- pattern: "mesh.*optimization"
  delegate_to: orcawave_agent
  fallback: cad_agent
  
- pattern: "geometry.*import"
  delegate_to: cad_agent
  coordinate_with: orcawave_agent
  
- pattern: "validate.*results"
  delegate_to: testing_agent
  require: orcawave_agent_context
  
- pattern: "export.*orcaflex"
  delegate_to: orcaflex_agent
  source: orcawave_agent
```

## Future Enhancements

1. **Machine Learning Integration**
   - Predictive mesh optimization
   - Automatic parameter tuning
   - Anomaly detection in results

2. **Cloud Execution**
   - Distributed batch processing
   - Cloud-based solver farms
   - Real-time collaboration

3. **Advanced Visualization**
   - AR/VR result visualization
   - Interactive 3D manipulation
   - Real-time animation of wave fields

4. **Regulatory Compliance**
   - Automated compliance checking
   - Report generation for classification
   - Audit trail maintenance

## Dependencies
- FastMCP 2.0.5+
- OrcaWave 11.0+ with valid license
- Python 3.11+ with COM support
- pywin32 for Windows COM
- numpy, pandas, scipy for analysis
- Vision API access (GPT-4/Claude)

## References
- [OrcaWave Python API Documentation](https://www.orcina.com/webhelp/OrcFxAPI/)
- [Panel Method Theory](https://www.orcina.com/resources/theory/)
- [MCP Specification](https://modelcontextprotocol.io/)
- [Generic GUI Template](../generic-gui-template/spec.md)