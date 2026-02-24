# AI Agent Escalation Guide - OrcaWave Diffraction Analysis

## 🚨 MANDATORY ESCALATION POINTS

### STOP AND ESCALATE IMMEDIATELY When:

#### 1. Geometry Work Requested
```python
if task_contains(["create GDF", "generate mesh", "convert STL to GDF", "fix geometry"]):
    STOP()
    escalate_to_user("""
    🚨 GEOMETRY SPECIALIST REQUIRED
    Task requires WAMIT GDF format knowledge
    I cannot create valid GDF files without documentation
    Options:
    1. Provide WAMIT manual
    2. Use existing GDF files only
    3. Create specialized Geometry Agent
    """)
```

#### 2. Mesh Quality Issues
```python
if error_contains(["panel aspect ratio", "mesh quality", "waterline gap"]):
    STOP()
    request_expert("""
    🚨 MESH QUALITY EXPERT NEEDED
    Issue: [specific mesh problem]
    Required: Marine engineering expertise for mesh criteria
    Cannot proceed without domain knowledge
    """)
```

#### 3. Physical Properties Missing
```python
if missing_any(["vessel mass", "center of gravity", "moments of inertia"]):
    STOP()
    request_from_user("""
    🚨 VESSEL DATA REQUIRED
    Missing: [specific properties]
    Source: Naval architect / Stability booklet
    Cannot use arbitrary values
    """)
```

#### 4. Hydrodynamic Parameters
```python
if asked_to_select(["damping coefficients", "added mass", "drag coefficients"]):
    STOP()
    alert_user("""
    🚨 HYDRODYNAMIC EXPERTISE REQUIRED
    Parameter: [specific coefficient]
    These require empirical data or expertise
    Cannot estimate without domain knowledge
    """)
```

## ✅ SAFE TO PROCEED When:

### Can Handle Independently:
1. **Using existing GDF files**
   - Files from `docs/modules/orcawave/examples/`
   - Pre-validated geometries
   
2. **Running analyses with provided configs**
   - Using complete YAML configurations
   - All parameters already specified
   
3. **Processing results**
   - Converting output formats
   - Creating visualizations
   - Generating reports

4. **File organization**
   - Moving files to appropriate directories
   - Creating folder structures
   - Managing outputs

## 🟡 PROCEED WITH CAUTION:

### Verify Before Proceeding:
1. **Parameter modifications**
   - Use OrcaWave defaults when possible
   - Document any assumptions
   - Flag for user review

2. **Frequency/heading ranges**
   - Start with limited ranges
   - Expand only if requested
   - Note computational implications

3. **Solver settings**
   - Use standard settings
   - Don't modify convergence criteria
   - Keep default tolerances

## 📋 Decision Tree

```
Task Requested
    │
    ├─> Involves Geometry Creation?
    │   └─> YES → 🚨 ESCALATE TO USER
    │
    ├─> Requires Domain Knowledge?
    │   └─> YES → 🚨 ESCALATE TO USER
    │
    ├─> Missing Critical Data?
    │   └─> YES → 🚨 REQUEST FROM USER
    │
    ├─> Using Existing Files?
    │   └─> YES → ✅ PROCEED
    │
    └─> Standard Operation?
        └─> YES → ✅ PROCEED
```

## 📝 Escalation Template

```markdown
🚨 SPECIALIZED KNOWLEDGE REQUIRED 🚨

**Task:** [What was requested]
**Issue:** [Why you cannot proceed]
**Domain:** [Geometry/Hydrodynamics/Mesh/Other]

**Knowledge Gap:**
- [Specific missing knowledge]
- [Required documentation]
- [Needed expertise]

**Impact:**
- Cannot proceed with: [specific tasks]
- Risk if guessing: [potential problems]

**Options for User:**
1. Provide [specific documentation/data]
2. Consult [type of expert]
3. Use simplified approach: [alternative]
4. Create specialized agent for [domain]

**Recommendation:** [Your suggested path forward]

**Current Workaround:**
[What you CAN do without this knowledge]
```

## 🔄 Continuous Learning Protocol

### When User Provides Information:
1. **Document it** in specifications
2. **Create examples** for future reference
3. **Update this guide** with new patterns
4. **Share with other agents** via specs

### When Errors Occur:
1. **Capture error message**
2. **Identify knowledge gap**
3. **Add to escalation triggers**
4. **Request missing information**

## 🎯 Key Principle

**When in doubt, ASK THE USER**

Marine engineering is a specialized field with:
- Safety-critical calculations
- Industry-specific standards
- Empirical coefficients
- Complex physics

**Never guess or approximate** when:
- Lives could be at risk
- Structural integrity is involved
- Regulatory compliance is required
- Validation data is needed

## Summary Actions

### ALWAYS ESCALATE:
- ❌ Geometry generation
- ❌ Mesh quality criteria  
- ❌ Vessel properties
- ❌ Empirical coefficients

### SAFE TO HANDLE:
- ✅ File management
- ✅ Using existing configs
- ✅ Result processing
- ✅ Report generation

### DOCUMENT EVERYTHING:
- 📝 Knowledge gaps found
- 📝 User guidance received
- 📝 Assumptions made
- 📝 Validation needed