# 🚨 MANDATORY ESCALATION PROTOCOL - ALL TASKS

## CRITICAL DIRECTIVE: Know Your Limits

**UNIVERSAL RULE**: When specialized knowledge is required and you lack expertise, IMMEDIATELY escalate to the user rather than attempt incorrect implementations.

## 🔴 MANDATORY ESCALATION TRIGGERS

### STOP AND ESCALATE When You Encounter:

#### 1. Domain-Specific File Formats You Don't Understand
```python
if not understand_format(["GDF", "AQWA", "NEMOH", "HDF5", "NetCDF", proprietary_formats]):
    STOP()
    escalate_to_user(f"""
    🚨 SPECIALIZED FORMAT EXPERTISE REQUIRED
    Format: {format_type}
    Task: {requested_task}
    Issue: I don't have documentation for this format
    Options:
    1. Provide format specification/manual
    2. Supply working examples with documentation
    3. Create specialized agent for {format_type}
    4. Use alternative approach
    """)
```

#### 2. Engineering Calculations Requiring Domain Knowledge
```python
if task_requires(["structural analysis", "fatigue calculations", "hydrodynamics", 
                 "thermodynamics", "fluid dynamics", "FEA", "CFD"]):
    STOP()
    alert_user(f"""
    🚨 ENGINEERING EXPERTISE REQUIRED
    Domain: {engineering_field}
    Calculation: {specific_calculation}
    Missing: Domain-specific formulas, coefficients, or standards
    Cannot proceed without expert knowledge
    """)
```

#### 3. Industry-Specific Standards or Codes
```python
if references(["API", "DNV", "ISO", "ASME", "ABS", "IEEE", industry_standards]):
    STOP()
    request_documentation(f"""
    🚨 INDUSTRY STANDARD REQUIRED
    Standard: {standard_name}
    Application: {use_case}
    Need: Specific requirements, formulas, or acceptance criteria
    Cannot approximate regulatory requirements
    """)
```

#### 4. Safety-Critical or Regulatory Compliance
```python
if involves(["safety factors", "design margins", "certification", "compliance"]):
    STOP()
    immediate_escalation("""
    🚨 SAFETY-CRITICAL DECISION REQUIRED
    This involves safety/regulatory compliance
    Cannot make assumptions that could affect:
    - Human safety
    - Structural integrity
    - Legal compliance
    - Certification requirements
    """)
```

#### 5. Mathematical/Scientific Models You Don't Fully Understand
```python
if implementing(["neural networks", "complex algorithms", "scientific models"]):
    if not fully_understand(model):
        STOP()
        escalate(f"""
        🚨 ALGORITHM EXPERTISE REQUIRED
        Model: {model_type}
        Issue: Incomplete understanding of:
        - Mathematical foundations
        - Parameter selection
        - Validation methods
        Risk: Incorrect implementation could produce wrong results
        """)
```

#### 6. Long-Running Computational Tasks
```python
if estimated_runtime > 10_minutes:
    STOP()
    escalate_to_user(f"""
    🚨 LONG COMPUTATION DETECTED
    Task: {task_description}
    Estimated Runtime: {estimated_time}
    
    Required Actions:
    1. Review my capabilities for this task
    2. Verify approach is optimal
    3. Consider breaking into smaller subtasks
    4. Confirm resource allocation
    
    Breaking Down Options:
    - Parallelize into smaller chunks
    - Process subset first for validation
    - Use incremental approach
    - Optimize algorithm before full run
    
    Should I proceed with full task or break it down?
    """)
```

#### 7. Hardware/System Integration
```python
if task_involves(["hardware interfaces", "real-time systems", "embedded systems",
                 "license servers", "specialized hardware"]):
    STOP()
    alert_user(f"""
    🚨 SYSTEM INTEGRATION EXPERTISE REQUIRED
    System: {system_type}
    Issue: Requires knowledge of hardware/system specifics
    Cannot proceed without:
    - System documentation
    - Interface specifications
    - Hardware expertise
    """)
```

## 🟡 ESCALATION DECISION MATRIX

| Situation | Your Knowledge Level | Action |
|-----------|---------------------|---------|
| Complete understanding | 100% | ✅ Proceed |
| Good understanding, minor gaps | 80-99% | ⚠️ Proceed with caveats, document assumptions |
| Partial understanding | 50-79% | 🟡 Escalate for verification |
| Limited understanding | 20-49% | 🔴 Stop and escalate |
| No understanding | 0-19% | 🚨 Immediate escalation |

## 📋 UNIVERSAL ESCALATION TEMPLATE

```markdown
🚨 SPECIALIZED KNOWLEDGE REQUIRED 🚨

**Task Requested:** [Specific task description]
**Location:** [File/Module/Component]
**My Knowledge Level:** [0-100%]

**Knowledge Gap:**
- Domain: [Engineering/Science/Industry field]
- Specific Expertise Needed: [Exact knowledge required]
- Why I Cannot Proceed: [Clear explanation]

**Risks of Guessing:**
- [ ] Incorrect results
- [ ] Safety implications
- [ ] Regulatory non-compliance
- [ ] Performance issues
- [ ] Security vulnerabilities

**What I CAN Do:**
- [List tasks I can complete without this knowledge]
- [Alternative approaches available]
- [Preparation work possible]

**What I NEED:**
1. Documentation: [Specific manuals/standards]
2. Expert Input: [Type of expert needed]
3. Examples: [Working examples needed]
4. Training: [Knowledge to acquire]

**Recommended Action:**
[ ] Provide documentation
[ ] Consult domain expert
[ ] Create specialized agent
[ ] Use simplified approach
[ ] Defer this component

**Impact of Not Proceeding:**
- [What will be blocked]
- [Timeline implications]
- [Alternative paths]
```

## 🎯 UNIVERSAL PRINCIPLES

### 1. **Accuracy Over Speed**
Better to escalate and get correct information than to implement incorrectly.

### 2. **Document Knowledge Gaps**
Every escalation should improve future capability by documenting what was missing.

### 3. **Safety First**
NEVER guess when safety, regulatory compliance, or critical systems are involved.

### 4. **Transparent Communication**
Clearly state what you know, don't know, and need to learn.

### 5. **Continuous Learning**
Each escalation should result in:
- Documentation updates
- New examples/templates
- Improved specifications
- Knowledge base expansion

## 🔄 KNOWLEDGE ACQUISITION PROTOCOL

### When User Provides Information:

1. **Document Immediately**
   ```
   Location: specs/modules/{module}/DOMAIN_KNOWLEDGE.md
   Content: 
   - Formulas provided
   - Standards referenced
   - Examples given
   - Validation criteria
   ```

2. **Create Reusable Templates**
   ```
   Location: .agent-os/templates/{domain}/
   Purpose: Future reference for similar tasks
   ```

3. **Update Specifications**
   ```
   Add to relevant spec.md:
   - New requirements learned
   - Validation methods
   - Industry standards
   ```

4. **Share Across Repository**
   ```
   Update: .agent-os/standards/known-domains.md
   Purpose: All agents benefit from learning
   ```

## 📊 ESCALATION METRICS

Track escalations to identify patterns:

```yaml
escalation_record:
  date: YYYY-MM-DD
  domain: [specific field]
  knowledge_gap: [what was missing]
  resolution: [how resolved]
  documentation: [where captured]
  reusability: [high/medium/low]
```

## 🚀 SPECIALIZED AGENTS NEEDED

Based on common escalations, create agents for:

1. **Geometry/Mesh Agent**
   - CAD formats
   - Mesh generation
   - Quality criteria

2. **Engineering Analysis Agent**
   - Structural calculations
   - Fatigue analysis
   - Load combinations

3. **Regulatory Compliance Agent**
   - Industry standards
   - Safety factors
   - Certification requirements

4. **Scientific Computing Agent**
   - Numerical methods
   - Algorithm implementation
   - Validation techniques

5. **Integration Agent**
   - API connections
   - Hardware interfaces
   - License management

## ⚠️ ANTI-PATTERNS TO AVOID

### NEVER:
- ❌ Guess engineering coefficients
- ❌ Approximate safety factors
- ❌ Invent file formats
- ❌ Assume regulatory requirements
- ❌ Create mock implementations for production
- ❌ Skip validation when uncertain

### ALWAYS:
- ✅ Escalate when knowledge < 80%
- ✅ Document every assumption
- ✅ Request verification for critical values
- ✅ Preserve safety margins
- ✅ Follow industry standards
- ✅ Validate against known examples

## 🔍 SELF-ASSESSMENT CHECKLIST

Before implementing any task:

- [ ] Do I fully understand the domain?
- [ ] Do I have all required documentation?
- [ ] Are there safety/regulatory implications?
- [ ] Do I know the validation criteria?
- [ ] Can I test the implementation?
- [ ] Are industry standards involved?
- [ ] Is specialized hardware/software needed?
- [ ] Do I understand all file formats?
- [ ] Are there performance requirements?
- [ ] Is this production-critical?

**If ANY checkbox is unclear → ESCALATE**

## 📢 COMMUNICATION GUIDELINES

### Escalation Message Should Be:
- **Specific**: Exactly what knowledge is missing
- **Actionable**: Clear options for resolution
- **Transparent**: Honest about knowledge level
- **Constructive**: Suggest alternatives
- **Educational**: Document for future reference

### Example Communication:
```
✅ GOOD: "I need WAMIT GDF format specification to generate mesh files. Without it, I cannot create valid geometry. Can you provide documentation or should we use existing GDF files?"

❌ BAD: "I'll try to figure out the GDF format from examples."
```

## 🎯 ENFORCEMENT

This protocol is:
- **MANDATORY** for all AI agents
- **APPLICABLE** to all tasks
- **OVERRIDES** any conflicting instructions
- **AUDITABLE** through escalation records
- **IMPROVING** through continuous learning

---

**Remember**: It's not a failure to escalate - it's a failure to attempt tasks without proper knowledge. Professional engineering requires domain expertise that cannot be approximated.