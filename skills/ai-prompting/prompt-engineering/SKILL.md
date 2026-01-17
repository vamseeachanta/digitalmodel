---
name: prompt-engineering
description: Comprehensive prompting techniques including chain-of-thought, few-shot, zero-shot, system prompts, persona design, and evaluation patterns
version: 1.0.0
author: workspace-hub
category: ai-prompting
type: skill
trigger: manual
auto_execute: false
capabilities:
  - chain_of_thought
  - few_shot_learning
  - zero_shot_prompting
  - system_prompt_design
  - persona_creation
  - structured_output
  - prompt_templates
  - evaluation_patterns
  - iterative_refinement
tools:
  - Read
  - Write
  - Bash
  - Grep
tags: [prompting, llm, chain-of-thought, few-shot, zero-shot, system-prompts, personas, evaluation]
platforms: [python, api]
related_skills:
  - langchain
  - dspy
---

# Prompt Engineering Skill

> Comprehensive prompting techniques for effective LLM interactions across any model or framework.

## Quick Start

```python
import openai

client = openai.OpenAI()

# Basic prompt
response = client.chat.completions.create(
    model="gpt-4",
    messages=[
        {"role": "system", "content": "You are an expert engineer."},
        {"role": "user", "content": "Explain mooring systems."}
    ]
)

print(response.choices[0].message.content)
```

## When to Use This Skill

**USE when:**
- Designing prompts from scratch for any use case
- Learning core principles applicable across all LLMs
- Need portable patterns not tied to specific frameworks
- Building simple LLM integrations without heavy dependencies
- Optimizing existing prompts for better results
- Creating reusable prompt templates for teams
- Debugging underperforming LLM applications
- Teaching prompt engineering to others

**DON'T USE when:**
- Need framework-specific features (use LangChain/DSPy)
- Require programmatic optimization (use DSPy)
- Building production RAG systems (use LangChain)
- Need conversation memory management (use frameworks)

## Prerequisites

```bash
# OpenAI
pip install openai>=1.0.0
export OPENAI_API_KEY="sk-..."

# Anthropic
pip install anthropic>=0.5.0
export ANTHROPIC_API_KEY="sk-ant-..."

# Azure OpenAI
pip install openai>=1.0.0
export AZURE_OPENAI_ENDPOINT="https://..."
export AZURE_OPENAI_KEY="..."

# Optional: For testing prompts
pip install pytest promptfoo
```

## Core Concepts

### Anatomy of a Prompt

```
[SYSTEM CONTEXT]     - Who is the AI, what are its capabilities/constraints
[TASK DESCRIPTION]   - What needs to be done
[INPUT DATA]         - The specific content to process
[OUTPUT FORMAT]      - How the response should be structured
[EXAMPLES]           - Optional: demonstrations of desired behavior
[CONSTRAINTS]        - Limitations, things to avoid
```

### The Prompt Engineering Loop

```
1. Define Goal     -> What do you want to achieve?
2. Draft Prompt    -> Initial attempt
3. Test            -> Run with diverse inputs
4. Analyze         -> Identify failures and patterns
5. Refine          -> Improve based on analysis
6. Repeat          -> Until quality meets requirements
```

## Core Capabilities

### 1. Zero-Shot Prompting

**Basic Zero-Shot:**
```python
def zero_shot_prompt(task: str, input_text: str) -> str:
    """
    Zero-shot prompting: Direct instruction without examples.
    Best for simple, well-defined tasks.
    """
    prompt = f"""
Task: {task}

Input: {input_text}

Output:
"""
    return prompt

# Usage
prompt = zero_shot_prompt(
    task="Classify this text as positive, negative, or neutral",
    input_text="The mooring analysis passed all safety requirements."
)
# Output: positive
```

**Zero-Shot with Role:**
```python
def zero_shot_with_role(role: str, task: str, input_text: str) -> str:
    """
    Zero-shot with explicit role definition.
    """
    system = f"You are a {role}. You provide expert analysis."

    user = f"""
{task}

{input_text}
"""
    return system, user

# Usage
system, user = zero_shot_with_role(
    role="senior offshore engineer with 20 years experience",
    task="Review this mooring design and identify any concerns:",
    input_text="8-line spread mooring in 150m water depth..."
)
```

**Zero-Shot Classification:**
```python
CLASSIFICATION_TEMPLATE = """
Classify the following engineering report into one of these categories:
- ANALYSIS: Technical analysis or simulation results
- INSPECTION: Field inspection or survey findings
- DESIGN: Design specifications or requirements
- INCIDENT: Incident reports or failure analysis
- MAINTENANCE: Maintenance records or procedures

Report:
{report_text}

Category:
"""

def classify_report(report_text: str) -> str:
    prompt = CLASSIFICATION_TEMPLATE.format(report_text=report_text)
    # Send to LLM
    return prompt
```

### 2. Few-Shot Prompting

**Basic Few-Shot:**
```python
def few_shot_prompt(
    task_description: str,
    examples: list,
    input_text: str
) -> str:
    """
    Few-shot prompting with examples.
    Generally 2-5 examples work best.
    """
    prompt = f"{task_description}\n\n"

    # Add examples
    for i, ex in enumerate(examples, 1):
        prompt += f"Example {i}:\n"
        prompt += f"Input: {ex['input']}\n"
        prompt += f"Output: {ex['output']}\n\n"

    # Add actual input
    prompt += f"Now process this:\n"
    prompt += f"Input: {input_text}\n"
    prompt += f"Output:"

    return prompt

# Usage
examples = [
    {
        "input": "Tension: 2500 kN, Limit: 2800 kN",
        "output": "PASS - Tension is 89% of limit, within acceptable range."
    },
    {
        "input": "Tension: 3100 kN, Limit: 2800 kN",
        "output": "FAIL - Tension exceeds limit by 11%. Redesign required."
    },
    {
        "input": "Tension: 2750 kN, Limit: 2800 kN",
        "output": "WARNING - Tension is 98% of limit, minimal margin."
    }
]

prompt = few_shot_prompt(
    task_description="Evaluate mooring line tension against limits.",
    examples=examples,
    input_text="Tension: 2200 kN, Limit: 2800 kN"
)
```

**Few-Shot with Diverse Examples:**
```python
def create_balanced_few_shot(examples_by_category: dict, input_text: str) -> str:
    """
    Create few-shot prompt with balanced examples across categories.
    """
    prompt = "Classify engineering documents into categories.\n\n"

    # Include one example from each category
    for category, examples in examples_by_category.items():
        ex = examples[0]  # Take first example from each
        prompt += f"Document: {ex['text']}\n"
        prompt += f"Category: {category}\n\n"

    prompt += f"Document: {input_text}\n"
    prompt += f"Category:"

    return prompt

# Usage
examples_by_category = {
    "ANALYSIS": [
        {"text": "FEA results show stress concentration at weld..."}
    ],
    "INSPECTION": [
        {"text": "Visual inspection revealed corrosion on flange..."}
    ],
    "DESIGN": [
        {"text": "The platform shall be designed for 100-year storm..."}
    ]
}

prompt = create_balanced_few_shot(
    examples_by_category,
    input_text="Fatigue analysis indicates 35-year service life..."
)
```

### 3. Chain-of-Thought Prompting

**Basic Chain-of-Thought:**
```python
COT_TEMPLATE = """
Solve this problem step by step.

Problem: {problem}

Let me think through this carefully:

Step 1: First, I'll identify the key information...
Step 2: Next, I'll determine the approach...
Step 3: Then, I'll perform the calculations...
Step 4: Finally, I'll verify and state the answer...

Solution:
"""

def chain_of_thought_prompt(problem: str) -> str:
    return COT_TEMPLATE.format(problem=problem)

# Usage
prompt = chain_of_thought_prompt(
    problem="""
    A mooring line has a breaking load of 5000 kN.
    The maximum tension is 2800 kN.
    What is the safety factor, and does it meet the API RP 2SK
    requirement of 1.67 for intact conditions?
    """
)
```

**Zero-Shot Chain-of-Thought:**
```python
def zero_shot_cot(question: str) -> str:
    """
    Zero-shot CoT: Simply append "Let's think step by step"
    Surprisingly effective for many reasoning tasks.
    """
    return f"{question}\n\nLet's think step by step."

# Usage
prompt = zero_shot_cot(
    "If a vessel offsets 50m from its mean position, and the "
    "mooring stiffness is 100 kN/m, what is the restoring force?"
)
```

**Structured Chain-of-Thought:**
```python
STRUCTURED_COT_TEMPLATE = """
Analyze this engineering problem using structured reasoning.

Problem: {problem}

## Understanding
What are the key facts and requirements?

## Approach
What method or formula will I use?

## Calculation
Show the step-by-step calculation.

## Verification
How can I verify this is correct?

## Answer
State the final answer clearly.
"""

def structured_cot(problem: str) -> str:
    return STRUCTURED_COT_TEMPLATE.format(problem=problem)
```

**Self-Consistency Chain-of-Thought:**
```python
import random

def self_consistency_cot(problem: str, num_paths: int = 5) -> str:
    """
    Generate multiple reasoning paths and take majority vote.
    Improves reliability for complex reasoning.
    """
    prompt = f"""
Solve this problem {num_paths} different ways, then determine the most likely correct answer.

Problem: {problem}

Approach 1:
[Solve using one method]

Approach 2:
[Solve using a different method]

... continue for all {num_paths} approaches ...

Consensus Answer:
[The answer that most approaches agree on]

Confidence:
[How many approaches agreed: X/{num_paths}]
"""
    return prompt
```

### 4. System Prompt Design

**Role-Based System Prompt:**
```python
def create_role_system_prompt(
    role: str,
    expertise: list,
    personality: str = "professional and helpful",
    constraints: list = None
) -> str:
    """
    Create a role-based system prompt.
    """
    prompt = f"""You are a {role}.

## Expertise
You have deep knowledge in:
{chr(10).join(f"- {e}" for e in expertise)}

## Communication Style
You are {personality}. You provide clear, accurate information and acknowledge uncertainty when appropriate.
"""

    if constraints:
        prompt += f"""
## Constraints
{chr(10).join(f"- {c}" for c in constraints)}
"""

    return prompt

# Usage
system_prompt = create_role_system_prompt(
    role="senior offshore structural engineer",
    expertise=[
        "Mooring system design and analysis",
        "Fatigue assessment per DNV standards",
        "API and ISO offshore codes",
        "Finite element analysis"
    ],
    personality="thorough, safety-conscious, and educational",
    constraints=[
        "Always cite relevant standards when applicable",
        "Recommend consulting specialists for critical decisions",
        "Flag any safety concerns prominently"
    ]
)
```

**Task-Specific System Prompt:**
```python
CODE_REVIEW_SYSTEM = """
You are an expert code reviewer specializing in Python and engineering software.

## Your Task
Review code for:
1. Correctness - Does it work as intended?
2. Safety - Are there potential bugs or edge cases?
3. Performance - Any inefficiencies?
4. Maintainability - Is it readable and well-structured?
5. Best Practices - Does it follow Python conventions?

## Response Format
For each issue found:
- **Location**: File and line number
- **Severity**: Critical / Major / Minor / Suggestion
- **Issue**: Description of the problem
- **Fix**: Suggested solution with code example

## Guidelines
- Be constructive, not critical
- Praise good patterns you see
- Focus on the most impactful issues first
- Explain WHY something is an issue
"""

DOCUMENT_ANALYSIS_SYSTEM = """
You are a technical document analyst specializing in engineering reports.

## Your Task
Analyze engineering documents to:
1. Extract key metrics and findings
2. Identify risks and concerns
3. Summarize conclusions
4. Note any missing information

## Response Format
Always structure your response as:
1. **Executive Summary** (2-3 sentences)
2. **Key Metrics** (bullet list with values and units)
3. **Findings** (numbered list)
4. **Risks** (with severity: High/Medium/Low)
5. **Recommendations** (actionable items)
6. **Information Gaps** (what's missing or unclear)

## Guidelines
- Use precise technical language
- Include units for all measurements
- Flag any values that seem unusual
- Note document quality and completeness
"""
```

### 5. Persona Design

**Expert Persona:**
```python
def create_expert_persona(
    name: str,
    title: str,
    experience_years: int,
    specializations: list,
    notable_work: list = None,
    communication_style: str = None
) -> str:
    """
    Create a detailed expert persona for the AI.
    """
    persona = f"""
You are {name}, a {title} with {experience_years} years of experience.

## Background
You specialize in:
{chr(10).join(f"- {s}" for s in specializations)}
"""

    if notable_work:
        persona += f"""
## Notable Experience
{chr(10).join(f"- {w}" for w in notable_work)}
"""

    if communication_style:
        persona += f"""
## Communication Style
{communication_style}
"""

    persona += """
## Approach
- Draw on your extensive experience when answering
- Reference specific projects or cases when relevant
- Admit when something is outside your expertise
- Provide practical, actionable advice
"""

    return persona

# Usage
persona = create_expert_persona(
    name="Dr. Sarah Chen",
    title="Principal Mooring Engineer",
    experience_years=25,
    specializations=[
        "Deepwater mooring systems",
        "FPSO turret design",
        "Mooring integrity management",
        "API RP 2SK development committee member"
    ],
    notable_work=[
        "Led mooring design for 10+ FPSOs globally",
        "Developed industry guidelines for polyester moorings",
        "Expert witness in mooring failure investigations"
    ],
    communication_style="Direct and practical, with emphasis on safety and reliability. Uses real-world examples to illustrate points."
)
```

**Adaptive Persona:**
```python
def create_adaptive_persona(
    expertise_level: str,  # beginner, intermediate, expert
    domain: str
) -> str:
    """
    Create persona that adapts to user's expertise level.
    """
    adaptations = {
        "beginner": {
            "language": "simple, avoiding jargon",
            "explanations": "detailed with background context",
            "examples": "basic, relatable analogies",
            "assumptions": "minimal prior knowledge"
        },
        "intermediate": {
            "language": "technical but with key term explanations",
            "explanations": "focused on application",
            "examples": "practical industry scenarios",
            "assumptions": "familiar with fundamentals"
        },
        "expert": {
            "language": "fully technical, industry standard",
            "explanations": "concise, focus on nuances",
            "examples": "complex edge cases",
            "assumptions": "deep domain knowledge"
        }
    }

    config = adaptations.get(expertise_level, adaptations["intermediate"])

    return f"""
You are a {domain} expert adapting your communication to a {expertise_level} audience.

## Communication Adaptation
- Use {config['language']} language
- Provide {config['explanations']} explanations
- Use {config['examples']} examples
- Assume {config['assumptions']}

## Guidelines
- Check understanding before proceeding to complex topics
- Offer to go deeper or simpler based on responses
- Be encouraging and supportive of learning
"""

# Usage
beginner_prompt = create_adaptive_persona("beginner", "mooring engineering")
expert_prompt = create_adaptive_persona("expert", "mooring engineering")
```

### 6. Structured Output

**JSON Output:**
```python
JSON_OUTPUT_TEMPLATE = """
Analyze the following engineering data and return your analysis as JSON.

Data:
{input_data}

Return a JSON object with this exact structure:
{{
    "summary": "Brief summary of findings",
    "metrics": [
        {{"name": "metric name", "value": 123.45, "unit": "unit", "status": "pass/fail/warning"}}
    ],
    "risks": [
        {{"description": "risk description", "severity": "high/medium/low", "mitigation": "suggested action"}}
    ],
    "recommendation": "Overall recommendation"
}}

JSON Response:
"""

def structured_json_prompt(input_data: str) -> str:
    return JSON_OUTPUT_TEMPLATE.format(input_data=input_data)
```

**Markdown Table Output:**
```python
TABLE_OUTPUT_TEMPLATE = """
Analyze the mooring line data and present results as a markdown table.

Data:
{input_data}

Create a table with columns:
| Line | Tension (kN) | Limit (kN) | Utilization (%) | Status |

Include a summary row at the bottom.

Response:
"""
```

**XML Output:**
```python
XML_OUTPUT_TEMPLATE = """
Extract information from this document and format as XML.

Document:
{document}

Return XML with this structure:
<analysis>
    <document_type>...</document_type>
    <date>YYYY-MM-DD</date>
    <findings>
        <finding severity="high|medium|low">
            <description>...</description>
            <location>...</location>
        </finding>
    </findings>
    <metrics>
        <metric name="..." value="..." unit="..."/>
    </metrics>
</analysis>

XML Response:
"""
```

### 7. Prompt Templates

**Reusable Template Class:**
```python
from string import Template
from typing import Dict, Any, Optional
import json

class PromptTemplate:
    """
    Reusable prompt template with validation and versioning.
    """

    def __init__(
        self,
        template: str,
        required_vars: list,
        optional_vars: list = None,
        version: str = "1.0.0",
        description: str = ""
    ):
        self.template = Template(template)
        self.required_vars = required_vars
        self.optional_vars = optional_vars or []
        self.version = version
        self.description = description

    def format(self, **kwargs) -> str:
        """Format template with provided variables."""
        # Validate required variables
        missing = set(self.required_vars) - set(kwargs.keys())
        if missing:
            raise ValueError(f"Missing required variables: {missing}")

        # Set defaults for optional variables
        for var in self.optional_vars:
            if var not in kwargs:
                kwargs[var] = ""

        return self.template.safe_substitute(**kwargs)

    def to_dict(self) -> Dict[str, Any]:
        """Export template as dictionary."""
        return {
            "template": self.template.template,
            "required_vars": self.required_vars,
            "optional_vars": self.optional_vars,
            "version": self.version,
            "description": self.description
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "PromptTemplate":
        """Create template from dictionary."""
        return cls(
            template=data["template"],
            required_vars=data["required_vars"],
            optional_vars=data.get("optional_vars", []),
            version=data.get("version", "1.0.0"),
            description=data.get("description", "")
        )

    def save(self, path: str):
        """Save template to file."""
        with open(path, 'w') as f:
            json.dump(self.to_dict(), f, indent=2)

    @classmethod
    def load(cls, path: str) -> "PromptTemplate":
        """Load template from file."""
        with open(path) as f:
            return cls.from_dict(json.load(f))

# Usage
analysis_template = PromptTemplate(
    template="""
Analyze this $document_type for $purpose.

Document:
$content

Focus on:
$focus_areas

Provide your analysis following this structure:
1. Summary
2. Key Findings
3. Recommendations

$additional_instructions
""",
    required_vars=["document_type", "purpose", "content", "focus_areas"],
    optional_vars=["additional_instructions"],
    version="1.0.0",
    description="General document analysis template"
)

prompt = analysis_template.format(
    document_type="mooring analysis report",
    purpose="safety review",
    content="[Report content here...]",
    focus_areas="safety factors, fatigue life, extreme conditions"
)
```

**Template Library:**
```python
class PromptLibrary:
    """
    Library of reusable prompt templates.
    """

    def __init__(self):
        self.templates: Dict[str, PromptTemplate] = {}

    def register(self, name: str, template: PromptTemplate):
        """Register a template."""
        self.templates[name] = template

    def get(self, name: str) -> PromptTemplate:
        """Get a template by name."""
        if name not in self.templates:
            raise KeyError(f"Template '{name}' not found")
        return self.templates[name]

    def list_templates(self) -> list:
        """List all available templates."""
        return [
            {"name": name, "description": t.description, "version": t.version}
            for name, t in self.templates.items()
        ]

    def save_library(self, path: str):
        """Save entire library to file."""
        data = {name: t.to_dict() for name, t in self.templates.items()}
        with open(path, 'w') as f:
            json.dump(data, f, indent=2)

    @classmethod
    def load_library(cls, path: str) -> "PromptLibrary":
        """Load library from file."""
        library = cls()
        with open(path) as f:
            data = json.load(f)
        for name, template_data in data.items():
            library.register(name, PromptTemplate.from_dict(template_data))
        return library

# Create standard library
library = PromptLibrary()

library.register("summarize", PromptTemplate(
    template="Summarize the following in $length:\n\n$text\n\nSummary:",
    required_vars=["length", "text"],
    description="Text summarization"
))

library.register("classify", PromptTemplate(
    template="Classify this text into one of: $categories\n\nText: $text\n\nCategory:",
    required_vars=["categories", "text"],
    description="Text classification"
))

library.register("extract", PromptTemplate(
    template="Extract $entity from this text:\n\n$text\n\nExtracted:",
    required_vars=["entity", "text"],
    description="Entity extraction"
))

# Usage
summarize = library.get("summarize")
prompt = summarize.format(length="2 sentences", text="[Long document...]")
```

### 8. Evaluation and Iteration

**Prompt Testing Framework:**
```python
from dataclasses import dataclass
from typing import Callable, List
import json

@dataclass
class TestCase:
    """Single test case for prompt evaluation."""
    input_data: dict
    expected_output: str = None
    expected_contains: list = None
    expected_not_contains: list = None
    metadata: dict = None

@dataclass
class TestResult:
    """Result of a single test."""
    test_case: TestCase
    actual_output: str
    passed: bool
    score: float
    details: str

class PromptEvaluator:
    """
    Evaluate prompts against test cases.
    """

    def __init__(self, prompt_template: PromptTemplate, llm_caller: Callable):
        self.template = prompt_template
        self.llm_caller = llm_caller

    def run_test(self, test_case: TestCase) -> TestResult:
        """Run a single test case."""
        # Format prompt
        prompt = self.template.format(**test_case.input_data)

        # Get LLM response
        output = self.llm_caller(prompt)

        # Evaluate
        passed = True
        score = 1.0
        details = []

        # Check exact match
        if test_case.expected_output:
            if output.strip() != test_case.expected_output.strip():
                passed = False
                score -= 0.5
                details.append(f"Expected exact: {test_case.expected_output}")

        # Check contains
        if test_case.expected_contains:
            for term in test_case.expected_contains:
                if term.lower() not in output.lower():
                    passed = False
                    score -= 0.2
                    details.append(f"Missing: {term}")

        # Check not contains
        if test_case.expected_not_contains:
            for term in test_case.expected_not_contains:
                if term.lower() in output.lower():
                    passed = False
                    score -= 0.3
                    details.append(f"Should not contain: {term}")

        return TestResult(
            test_case=test_case,
            actual_output=output,
            passed=passed,
            score=max(0, score),
            details="; ".join(details) if details else "All checks passed"
        )

    def run_suite(self, test_cases: List[TestCase]) -> dict:
        """Run all test cases and return summary."""
        results = [self.run_test(tc) for tc in test_cases]

        passed = sum(1 for r in results if r.passed)
        avg_score = sum(r.score for r in results) / len(results)

        return {
            "total": len(results),
            "passed": passed,
            "failed": len(results) - passed,
            "pass_rate": passed / len(results),
            "avg_score": avg_score,
            "results": results
        }

# Usage
test_cases = [
    TestCase(
        input_data={"text": "Tension: 2500 kN, Limit: 2800 kN"},
        expected_contains=["pass", "within"],
        expected_not_contains=["fail", "exceed"]
    ),
    TestCase(
        input_data={"text": "Tension: 3100 kN, Limit: 2800 kN"},
        expected_contains=["fail", "exceed"],
        expected_not_contains=["pass", "within"]
    )
]

evaluator = PromptEvaluator(tension_check_template, llm_call)
results = evaluator.run_suite(test_cases)

print(f"Pass Rate: {results['pass_rate']:.1%}")
print(f"Average Score: {results['avg_score']:.2f}")
```

**A/B Testing Prompts:**
```python
from typing import Dict, List
import random

class PromptABTest:
    """
    A/B test different prompt variants.
    """

    def __init__(self, variants: Dict[str, PromptTemplate], metric_fn: Callable):
        self.variants = variants
        self.metric_fn = metric_fn
        self.results: Dict[str, List[float]] = {name: [] for name in variants}

    def run_test(self, test_cases: List[TestCase], n_runs: int = 1) -> dict:
        """Run A/B test across variants."""

        for _ in range(n_runs):
            for test_case in test_cases:
                for name, template in self.variants.items():
                    prompt = template.format(**test_case.input_data)
                    output = llm_call(prompt)
                    score = self.metric_fn(test_case, output)
                    self.results[name].append(score)

        # Calculate statistics
        summary = {}
        for name, scores in self.results.items():
            summary[name] = {
                "mean": sum(scores) / len(scores),
                "min": min(scores),
                "max": max(scores),
                "n_samples": len(scores)
            }

        # Determine winner
        best = max(summary.items(), key=lambda x: x[1]["mean"])
        summary["winner"] = best[0]

        return summary

# Usage
variants = {
    "concise": PromptTemplate("Briefly analyze: $text", ["text"]),
    "detailed": PromptTemplate("Provide detailed analysis of: $text", ["text"]),
    "structured": PromptTemplate("Analyze with sections:\n$text", ["text"])
}

ab_test = PromptABTest(variants, quality_metric)
results = ab_test.run_test(test_cases, n_runs=3)

print(f"Winner: {results['winner']}")
for name, stats in results.items():
    if name != "winner":
        print(f"{name}: mean={stats['mean']:.2f}")
```

## Complete Examples

### Example 1: Multi-Stage Document Processor

```python
from typing import Dict, List, Optional
from dataclasses import dataclass

@dataclass
class ProcessingResult:
    summary: str
    key_points: List[str]
    metrics: List[Dict]
    risks: List[Dict]
    recommendations: List[str]
    confidence: float

class DocumentProcessor:
    """
    Multi-stage document processing with specialized prompts.
    """

    def __init__(self, llm_caller: Callable):
        self.llm = llm_caller

        # Stage 1: Extract key information
        self.extract_prompt = """
Extract key information from this engineering document.

Document:
{document}

Return as JSON:
{{
    "document_type": "type of document",
    "date": "document date if present",
    "subject": "main subject",
    "key_points": ["point 1", "point 2", ...],
    "metrics": [
        {{"name": "metric name", "value": "value", "unit": "unit"}}
    ]
}}

JSON:
"""

        # Stage 2: Analyze for risks
        self.risk_prompt = """
Based on this document summary and metrics, identify potential risks.

Summary: {summary}
Metrics: {metrics}

For each risk, provide:
- Description
- Severity (high/medium/low)
- Likelihood (high/medium/low)
- Mitigation suggestion

Return as JSON list:
[
    {{
        "description": "...",
        "severity": "...",
        "likelihood": "...",
        "mitigation": "..."
    }}
]

Risks:
"""

        # Stage 3: Generate recommendations
        self.recommend_prompt = """
Based on this analysis, provide prioritized recommendations.

Key Points: {key_points}
Risks: {risks}

Provide 3-5 actionable recommendations, prioritized by importance.
Each recommendation should be specific and actionable.

Recommendations:
1.
"""

    def process(self, document: str) -> ProcessingResult:
        """Process document through all stages."""

        # Stage 1: Extract
        extract_result = self.llm(
            self.extract_prompt.format(document=document)
        )
        extracted = self._parse_json(extract_result)

        # Stage 2: Analyze risks
        risk_result = self.llm(
            self.risk_prompt.format(
                summary=extracted.get("subject", ""),
                metrics=str(extracted.get("metrics", []))
            )
        )
        risks = self._parse_json(risk_result)

        # Stage 3: Recommendations
        recommend_result = self.llm(
            self.recommend_prompt.format(
                key_points="\n".join(extracted.get("key_points", [])),
                risks=str(risks)
            )
        )
        recommendations = self._parse_recommendations(recommend_result)

        # Calculate confidence based on extraction quality
        confidence = self._calculate_confidence(extracted, risks)

        return ProcessingResult(
            summary=extracted.get("subject", ""),
            key_points=extracted.get("key_points", []),
            metrics=extracted.get("metrics", []),
            risks=risks if isinstance(risks, list) else [],
            recommendations=recommendations,
            confidence=confidence
        )

    def _parse_json(self, text: str) -> dict:
        """Parse JSON from LLM response."""
        import json
        try:
            # Find JSON in response
            start = text.find('{')
            end = text.rfind('}') + 1
            if start >= 0 and end > start:
                return json.loads(text[start:end])
            start = text.find('[')
            end = text.rfind(']') + 1
            if start >= 0 and end > start:
                return json.loads(text[start:end])
        except json.JSONDecodeError:
            pass
        return {}

    def _parse_recommendations(self, text: str) -> List[str]:
        """Parse numbered recommendations."""
        lines = text.strip().split('\n')
        recommendations = []
        for line in lines:
            line = line.strip()
            if line and line[0].isdigit():
                # Remove number prefix
                rec = line.lstrip('0123456789.').strip()
                if rec:
                    recommendations.append(rec)
        return recommendations

    def _calculate_confidence(self, extracted: dict, risks: list) -> float:
        """Calculate confidence score."""
        score = 0.5  # Base

        if extracted.get("key_points"):
            score += 0.15
        if extracted.get("metrics"):
            score += 0.15
        if risks and len(risks) > 0:
            score += 0.2

        return min(score, 1.0)

# Usage
processor = DocumentProcessor(llm_caller=call_openai)

result = processor.process("""
MOORING ANALYSIS REPORT
Project: Platform Alpha
Date: 2026-01-15

Executive Summary:
Analysis of 8-line spread mooring in 120m water depth.

Key Results:
- Maximum tension: 2,450 kN (Line 3, 100-year storm)
- Safety factor: 1.74 (minimum requirement: 1.67)
- Fatigue life: 38 years (design life: 25 years)

Observations:
- Line 3 shows highest utilization at fairlead
- Chain wear at 7% after 5 years operation
- Polyester sections in good condition
""")

print(f"Summary: {result.summary}")
print(f"Confidence: {result.confidence:.0%}")
print(f"\nKey Points:")
for point in result.key_points:
    print(f"  - {point}")
print(f"\nRisks:")
for risk in result.risks:
    print(f"  - [{risk.get('severity', 'unknown')}] {risk.get('description', '')}")
print(f"\nRecommendations:")
for i, rec in enumerate(result.recommendations, 1):
    print(f"  {i}. {rec}")
```

### Example 2: Interactive Prompt Builder

```python
class InteractivePromptBuilder:
    """
    Build prompts interactively with guided configuration.
    """

    def __init__(self):
        self.components = {
            "role": None,
            "context": None,
            "task": None,
            "format": None,
            "examples": [],
            "constraints": []
        }

    def set_role(self, role: str, expertise: List[str] = None) -> "InteractivePromptBuilder":
        """Set the AI's role."""
        role_text = f"You are a {role}."
        if expertise:
            role_text += f" You have expertise in: {', '.join(expertise)}."
        self.components["role"] = role_text
        return self

    def set_context(self, context: str) -> "InteractivePromptBuilder":
        """Set background context."""
        self.components["context"] = f"Context: {context}"
        return self

    def set_task(self, task: str, details: str = None) -> "InteractivePromptBuilder":
        """Set the main task."""
        task_text = f"Task: {task}"
        if details:
            task_text += f"\n\nDetails: {details}"
        self.components["task"] = task_text
        return self

    def set_format(self, format_type: str, structure: str = None) -> "InteractivePromptBuilder":
        """Set output format."""
        format_text = f"Output Format: {format_type}"
        if structure:
            format_text += f"\n\n{structure}"
        self.components["format"] = format_text
        return self

    def add_example(self, input_text: str, output_text: str) -> "InteractivePromptBuilder":
        """Add a few-shot example."""
        self.components["examples"].append({
            "input": input_text,
            "output": output_text
        })
        return self

    def add_constraint(self, constraint: str) -> "InteractivePromptBuilder":
        """Add a constraint."""
        self.components["constraints"].append(constraint)
        return self

    def build(self) -> tuple:
        """Build the final system and user prompts."""
        system_parts = []
        user_parts = []

        # System prompt components
        if self.components["role"]:
            system_parts.append(self.components["role"])

        if self.components["constraints"]:
            system_parts.append("Constraints:")
            for c in self.components["constraints"]:
                system_parts.append(f"- {c}")

        # User prompt components
        if self.components["context"]:
            user_parts.append(self.components["context"])

        if self.components["task"]:
            user_parts.append(self.components["task"])

        if self.components["examples"]:
            user_parts.append("\nExamples:")
            for i, ex in enumerate(self.components["examples"], 1):
                user_parts.append(f"\nExample {i}:")
                user_parts.append(f"Input: {ex['input']}")
                user_parts.append(f"Output: {ex['output']}")

        if self.components["format"]:
            user_parts.append(f"\n{self.components['format']}")

        return "\n".join(system_parts), "\n".join(user_parts)

    def preview(self) -> str:
        """Preview the complete prompt."""
        system, user = self.build()
        return f"=== SYSTEM ===\n{system}\n\n=== USER ===\n{user}"

# Usage
builder = InteractivePromptBuilder()

system, user = (
    builder
    .set_role("senior offshore engineer", ["mooring design", "structural analysis"])
    .set_context("Reviewing mooring analysis report for regulatory submission")
    .set_task(
        "Review the mooring analysis and identify any issues",
        "Focus on safety factors, fatigue life, and extreme conditions"
    )
    .add_example(
        "SF = 1.65, Limit = 1.67",
        "ISSUE: Safety factor 1.65 is below minimum requirement of 1.67"
    )
    .add_example(
        "Fatigue life = 80 years, Design life = 25 years",
        "OK: Fatigue life exceeds 3x design life requirement"
    )
    .add_constraint("Always cite the relevant standard for each finding")
    .add_constraint("Use severity levels: Critical, Major, Minor")
    .set_format("Structured markdown", """
## Summary
[Overall assessment]

## Findings
| ID | Finding | Severity | Standard |
|----|---------|----------|----------|
| 1  | ...     | ...      | ...      |

## Recommendations
1. ...
""")
    .build()
)

print(builder.preview())
```

## Integration Patterns

### OpenAI Integration

```python
import openai

def create_openai_caller(model: str = "gpt-4", temperature: float = 0.7):
    """Create OpenAI API caller."""
    client = openai.OpenAI()

    def call(prompt: str, system: str = None) -> str:
        messages = []
        if system:
            messages.append({"role": "system", "content": system})
        messages.append({"role": "user", "content": prompt})

        response = client.chat.completions.create(
            model=model,
            messages=messages,
            temperature=temperature
        )
        return response.choices[0].message.content

    return call

# Usage
llm = create_openai_caller(model="gpt-4", temperature=0.3)
response = llm("Explain mooring catenary equations", system="You are an engineer.")
```

### Anthropic Integration

```python
import anthropic

def create_anthropic_caller(model: str = "claude-3-opus-20240229"):
    """Create Anthropic API caller."""
    client = anthropic.Anthropic()

    def call(prompt: str, system: str = None) -> str:
        response = client.messages.create(
            model=model,
            max_tokens=4096,
            system=system or "",
            messages=[{"role": "user", "content": prompt}]
        )
        return response.content[0].text

    return call

# Usage
llm = create_anthropic_caller()
response = llm("Explain mooring design", system="You are an offshore engineer.")
```

## Best Practices

### 1. Be Specific and Clear

```python
# Bad
prompt = "Analyze this."

# Good
prompt = """
Analyze this mooring analysis report for safety compliance.

Check the following:
1. Safety factors meet API RP 2SK requirements (>=1.67 intact, >=1.25 damaged)
2. Fatigue life exceeds 3x design life
3. All load cases are covered

Report:
{report}

Provide findings in a table format.
"""
```

### 2. Use Consistent Formatting

```python
# Establish consistent patterns
STANDARD_SECTIONS = """
## Input
{input}

## Task
{task}

## Output Format
{format}

## Constraints
{constraints}
"""
```

### 3. Provide Context

```python
# Include relevant background
context = """
This is a mooring analysis for an FPSO in the Gulf of Mexico.
Water depth: 1500m
Mooring type: Polyester-chain hybrid
Design life: 25 years
Applicable standard: API RP 2SK 4th Edition
"""
```

### 4. Iterate Based on Failures

```python
# Keep track of failures
failures = []

def log_failure(prompt, expected, actual, notes):
    failures.append({
        "prompt": prompt,
        "expected": expected,
        "actual": actual,
        "notes": notes
    })

# Review and improve
def analyze_failures():
    # Find patterns in failures
    # Adjust prompts accordingly
    pass
```

## Troubleshooting

### Inconsistent Outputs

```python
# Lower temperature for consistency
response = client.chat.completions.create(
    model="gpt-4",
    messages=messages,
    temperature=0.1  # Lower = more consistent
)

# Or use seed for reproducibility
response = client.chat.completions.create(
    model="gpt-4",
    messages=messages,
    seed=42
)
```

### Outputs Too Long/Short

```python
# Control length explicitly
prompt = """
Provide a summary in exactly 3 sentences.
Do not exceed 100 words.
"""

# Or use max_tokens
response = client.chat.completions.create(
    model="gpt-4",
    messages=messages,
    max_tokens=150
)
```

### Wrong Format

```python
# Be very explicit about format
prompt = """
Return ONLY a JSON object. No explanation, no markdown.

{
    "key": "value"
}
"""

# Validate and retry
def get_json_response(prompt, max_retries=3):
    for attempt in range(max_retries):
        response = llm(prompt)
        try:
            return json.loads(response)
        except json.JSONDecodeError:
            prompt = f"Your response was not valid JSON. Try again.\n\n{prompt}"
    raise ValueError("Failed to get valid JSON")
```

## Resources

- **OpenAI Prompt Engineering Guide**: https://platform.openai.com/docs/guides/prompt-engineering
- **Anthropic Prompt Engineering**: https://docs.anthropic.com/claude/docs/prompt-engineering
- **Prompt Engineering Guide**: https://www.promptingguide.ai/
- **Learn Prompting**: https://learnprompting.org/

---

## Version History

- **1.0.0** (2026-01-17): Initial release with comprehensive prompting patterns
