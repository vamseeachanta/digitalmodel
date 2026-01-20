# Agenta

> Manage, evaluate, and deploy LLM prompts with confidence. Version control your prompts, run A/B tests, and measure quality with automated evaluation.

## Overview

Agenta is an open-source LLM prompt management and evaluation platform. It provides tools for versioning prompts, running experiments, evaluating quality, and deploying with confidence. Self-hosted deployment ensures data privacy.

## Quick Start

```bash
# Install SDK
pip install agenta

# Start self-hosted (Docker)
docker run -d -p 3000:3000 -p 8000:8000 ghcr.io/agenta-ai/agenta

# Initialize project
agenta init --app-name my-llm-app
```

```python
import agenta as ag

ag.init()

@ag.entrypoint
def generate(prompt: str) -> str:
    response = ag.llm.complete(
        prompt=prompt,
        model="gpt-4",
        temperature=0.3
    )
    return response.text

# Run with automatic tracking
result = generate("What is machine learning?")
print(result)
```

## Key Features

| Feature | Description |
|---------|-------------|
| Prompt Versioning | Track changes to prompts over time |
| A/B Testing | Compare prompt variants with traffic splitting |
| Evaluation | Automated metrics (exact match, LLM judge, custom) |
| Playground | Interactive experimentation interface |
| Model Comparison | Benchmark prompts across different models |
| Self-Hosted | Full control with Docker deployment |

## When to Use

**USE when:**
- Managing multiple prompt versions in production
- Need systematic A/B testing of variations
- Evaluating prompt quality automatically
- Collaborating on prompts across teams
- Requiring audit trails for changes
- Self-hosting for security compliance

**DON'T USE when:**
- Simple single-prompt applications
- No versioning or testing needs
- Already using another prompt management system
- Rapid prototyping without evaluation

## Common Patterns

### Versioned Prompt
```python
@ag.entrypoint
def summarize(text: str, style: str = "concise") -> str:
    prompt = f"Summarize in a {style} style: {text}"
    return ag.llm.complete(prompt=prompt).text
```

### A/B Test Setup
```python
from agenta import Agenta
client = Agenta()

# Create test between variants
variants = client.list_variants(app_name="my-app")
# Route traffic between control and treatment
```

### Evaluation Pipeline
```python
from agenta_eval import EvaluationPipeline

pipeline = EvaluationPipeline("my-app")
pipeline.add_metric(ContainsKeywords(["key", "terms"]))
pipeline.add_metric(LLMJudge(criteria="helpfulness"))

results = pipeline.evaluate_batch(test_cases)
```

### Model Comparison
```python
comparator = ModelComparator(models=["gpt-4", "gpt-3.5-turbo"])
results = comparator.run_comparison("Explain quantum computing")

for model, result in results.items():
    print(f"{model}: {result.latency:.2f}s, ${result.cost:.4f}")
```

## Related Skills

- [langchain](../langchain/SKILL.md) - LLM application framework
- [dspy](../dspy/SKILL.md) - Programmatic prompt optimization
- [prompt-engineering](../prompt-engineering/SKILL.md) - Manual prompting patterns

## Resources

- [Agenta Docs](https://docs.agenta.ai/)
- [GitHub Repository](https://github.com/Agenta-AI/agenta)
- [Self-Hosting Guide](https://docs.agenta.ai/self-hosting)
- [API Reference](https://docs.agenta.ai/api-reference)

---

**Version**: 1.0.0
**Category**: ai-prompting
