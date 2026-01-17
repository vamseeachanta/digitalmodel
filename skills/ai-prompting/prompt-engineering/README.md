# Prompt Engineering

> Comprehensive prompting techniques for effective LLM interactions across any model or framework.

## Overview

Prompt engineering is the practice of designing effective prompts to get desired outputs from large language models. This skill covers universal patterns that work across all LLMs, from simple zero-shot prompts to complex multi-stage pipelines.

## Quick Start

```python
import openai

client = openai.OpenAI()

# Basic prompt with role and task
response = client.chat.completions.create(
    model="gpt-4",
    messages=[
        {"role": "system", "content": "You are an expert engineer."},
        {"role": "user", "content": "Explain mooring catenary equations."}
    ]
)
```

## Key Techniques

| Technique | Description | When to Use |
|-----------|-------------|-------------|
| Zero-Shot | Direct instruction, no examples | Simple, well-defined tasks |
| Few-Shot | Include examples | Classification, formatting |
| Chain-of-Thought | Step-by-step reasoning | Complex reasoning, math |
| System Prompts | Define AI role/behavior | All applications |
| Structured Output | JSON/XML formatting | Data extraction, APIs |

## When to Use

**USE when:**
- Designing prompts from scratch
- Need portable patterns across LLMs
- Building simple integrations
- Teaching/learning prompt engineering

**DON'T USE when:**
- Need programmatic optimization (use DSPy)
- Building complex RAG systems (use LangChain)
- Need conversation memory management

## Core Patterns

### Chain-of-Thought
```python
prompt = """
Solve this step by step.

Problem: {problem}

Let's think through this carefully:
1. First, identify the key information...
2. Next, determine the approach...
3. Finally, calculate the answer...

Solution:
"""
```

### Few-Shot
```python
prompt = """
Classify engineering documents.

Example 1:
Input: FEA results show stress at 450 MPa...
Output: ANALYSIS

Example 2:
Input: Visual inspection revealed corrosion...
Output: INSPECTION

Now classify:
Input: {new_document}
Output:
"""
```

### System Prompt Design
```python
system = """
You are a senior offshore engineer with 20 years experience.

Expertise:
- Mooring system design
- API and DNV standards
- Fatigue analysis

Guidelines:
- Always cite relevant standards
- Flag safety concerns prominently
- Acknowledge uncertainty
"""
```

### Structured Output
```python
prompt = """
Extract information as JSON:

{
    "summary": "brief summary",
    "metrics": [{"name": "...", "value": ..., "unit": "..."}],
    "risk_level": "high/medium/low"
}

Document:
{document}

JSON:
"""
```

## Prompt Anatomy

```
[SYSTEM CONTEXT]     - AI role and capabilities
[TASK DESCRIPTION]   - What to do
[INPUT DATA]         - Content to process
[OUTPUT FORMAT]      - Expected structure
[EXAMPLES]           - Demonstrations (optional)
[CONSTRAINTS]        - Limitations
```

## Best Practices

1. **Be Specific** - Clear instructions get better results
2. **Use Examples** - Show what you want, not just describe it
3. **Control Temperature** - Lower for consistency, higher for creativity
4. **Iterate** - Test, analyze failures, refine
5. **Version Control** - Track prompt changes

## Related Skills

- [langchain](../langchain/SKILL.md) - LLM application framework
- [dspy](../dspy/SKILL.md) - Programmatic prompt optimization

## Resources

- [OpenAI Guide](https://platform.openai.com/docs/guides/prompt-engineering)
- [Anthropic Guide](https://docs.anthropic.com/claude/docs/prompt-engineering)
- [Prompting Guide](https://www.promptingguide.ai/)

---

**Version**: 1.0.0
**Category**: ai-prompting
