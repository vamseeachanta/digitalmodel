# Specialized Knowledge Requirements for OrcaWave Diffraction Analysis

## Critical Specialized Areas Requiring User/Expert Input

### 1. 🔴 GEOMETRY GENERATION & MESH CREATION
**Status: REQUIRES SPECIALIZED EXPERTISE**

#### Required Knowledge:
- **WAMIT GDF Format Specification**
  - Quadrilateral panel definition (4 vertices per panel)
  - Panel ordering and normal conventions
  - Symmetry plane definitions
  - Control surface mesh requirements
  
#### Current Limitations:
- ❌ Cannot generate valid GDF files without WAMIT format documentation
- ❌ Unable to convert triangular meshes to quadrilateral panels correctly
- ❌ Missing understanding of panel normal orientation requirements
- ❌ Unclear on waterline intersection handling

#### User Action Required:
```
🚨 ATTENTION: Geometry generation requires specialized knowledge
- Provide WAMIT user manual or GDF format specification
- Supply working GDF examples with documentation
- Consider creating specialized Geometry Agent
- Alternative: Use only pre-validated GDF files
```

### 2. 🟡 HYDRODYNAMIC PARAMETERS
**Status: PARTIAL KNOWLEDGE - VERIFICATION NEEDED**

#### Areas Needing Verification:
- **Mass and Inertia Properties**
  - Centre of mass location
  - Moments of inertia tensor vs radii of gyration
  - Added mass considerations
  
- **Damping Coefficients**
  - External damping matrices
  - Roll damping targets
  - Viscous damping coefficients

#### User Input Needed:
- Vessel mass properties from stability booklet
- Experimental damping values if available
- Design load conditions

### 3. 🔴 MESH QUALITY CRITERIA
**Status: REQUIRES DOMAIN EXPERTISE**

#### Critical Parameters:
- **Panel Size vs Wavelength**
  - Minimum panels per wavelength (typically 5-7)
  - Maximum panel aspect ratio (typically < 4)
  - Panel size gradation requirements

- **Waterline Treatment**
  - Free surface panel refinement
  - Waterline gap tolerances
  - Interior free surface handling

#### User Action Required:
```
🚨 MESH QUALITY: Requires marine engineering expertise
- Define acceptable panel quality metrics
- Specify wavelength range of interest
- Provide industry-standard mesh criteria
```

### 4. 🟡 QTF CALCULATION SETTINGS
**Status: COMPUTATIONAL EXPERTISE NEEDED**

#### Complex Decisions:
- Full QTF vs Diagonal only
- Newman approximation applicability
- Frequency pair selection
- Computational resource allocation

#### Recommendation:
- Start with diagonal QTF only
- Consult user for full QTF requirements
- Consider computational time vs accuracy

### 5. 🔴 MORISON ELEMENTS
**Status: REQUIRES STRUCTURAL EXPERTISE**

#### Knowledge Gap:
- Drag coefficient selection (Cd values)
- Inertia coefficient selection (Cm values)
- Member categorization
- Marine growth considerations

#### User Input Essential:
```
🚨 MORISON COEFFICIENTS: Industry-specific values needed
- Provide drag coefficients for different members
- Specify which elements use Morison vs diffraction
- Include marine growth factors if applicable
```

## Escalation Protocol

### When to Immediately Escalate to User:

1. **Geometry Issues**
   ```python
   if error_contains("mesh", "panel", "gdf", "geometry"):
       alert_user("Geometry specialist needed - cannot proceed")
   ```

2. **Numerical Instabilities**
   ```python
   if warning_contains("singular", "ill-conditioned", "convergence"):
       alert_user("Numerical expert consultation required")
   ```

3. **Physical Property Uncertainties**
   ```python
   if missing_data in ["mass", "inertia", "COG", "damping"]:
       request_user_input("Vessel properties needed from naval architect")
   ```

## Recommended Specialist Agents to Create

### 1. **Geometry Agent**
- Expertise: WAMIT/AQWA/NEMOH mesh formats
- Capabilities: GDF generation, mesh quality checking
- Training data: WAMIT manual, example files

### 2. **Hydrodynamics Agent**
- Expertise: Marine hydrodynamics, wave theory
- Capabilities: Parameter selection, results validation
- Training data: OrcaWave manual, DNV guidelines

### 3. **Mesh Quality Agent**
- Expertise: Panel method requirements
- Capabilities: Mesh refinement, quality metrics
- Training data: Industry standards, convergence studies

## Current Workarounds

### Without Specialized Knowledge:
1. **Use Pre-validated Files Only**
   - Work with example GDF files from `docs/modules/orcawave/examples/`
   - Don't attempt geometry generation
   
2. **Conservative Parameter Selection**
   - Use OrcaWave defaults where possible
   - Start with simplified analyses
   
3. **Incremental Testing**
   - Test with small, simple geometries first
   - Validate each step before proceeding

## Action Items for User

### Immediate Needs:
- [ ] Provide WAMIT GDF format specification
- [ ] Supply vessel mass properties
- [ ] Define analysis frequency range
- [ ] Specify environmental conditions

### Future Enhancement:
- [ ] Create specialized geometry agent
- [ ] Develop mesh quality validator
- [ ] Build parameter recommendation system
- [ ] Establish validation benchmarks

## Communication Template

When encountering specialized requirements:

```
🚨 SPECIALIZED KNOWLEDGE REQUIRED 🚨

Area: [Geometry/Hydrodynamics/Mesh/Other]
Issue: [Specific problem description]
Impact: [Cannot proceed/Results may be inaccurate/Verification needed]

Required Expertise:
- [Specific knowledge domain]
- [Required documentation/tools]

Options:
1. Provide documentation/guidance
2. Create specialized agent
3. Use simplified approach
4. Defer this component

Recommended Action: [Your recommendation]
```

## Summary

This specification acknowledges critical knowledge gaps in:
1. **Geometry Generation** - Cannot create valid GDF files
2. **Mesh Quality** - Need domain expertise for criteria
3. **Hydrodynamic Parameters** - Require vessel-specific data
4. **Morison Elements** - Need empirical coefficients

**Key Principle**: When in doubt, escalate to user rather than guess. Maritime engineering requires domain expertise that cannot be approximated.