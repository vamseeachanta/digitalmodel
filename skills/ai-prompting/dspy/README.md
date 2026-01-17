# DSPy

> Compile prompts into self-improving pipelines with programmatic prompt engineering.

## Overview

DSPy is a framework for algorithmically optimizing LLM prompts and weights. Instead of manually crafting prompts, you define what you want (signatures), how to achieve it (modules), and let optimizers find the best prompts automatically.

## Quick Start

```bash
# Install
pip install dspy-ai

# Set API key
export OPENAI_API_KEY="your-key"
```

```python
import dspy

# Configure
lm = dspy.OpenAI(model="gpt-4")
dspy.settings.configure(lm=lm)

# Define signature
class QA(dspy.Signature):
    """Answer questions accurately."""
    question = dspy.InputField()
    answer = dspy.OutputField()

# Create module
qa = dspy.ChainOfThought(QA)

# Use
result = qa(question="What is mooring pretension?")
print(result.answer)
```

## Key Concepts

| Concept | Description |
|---------|-------------|
| Signatures | Define input/output specifications |
| Modules | Implement reasoning patterns (Predict, ChainOfThought, ReAct) |
| Optimizers | Automatically improve prompts with training data |
| Metrics | Evaluate and compare performance |

## When to Use

**USE when:**
- Optimizing prompts programmatically
- Building pipelines where prompt quality is critical
- Need reproducible, testable prompt engineering
- Complex multi-step reasoning tasks

**DON'T USE when:**
- Simple single-shot prompts
- Need exact control over prompt wording
- Prototyping where speed matters more than optimization

## Core Patterns

### Signatures
```python
class Analysis(dspy.Signature):
    """Analyze technical text."""
    text = dspy.InputField(desc="Technical document")
    summary = dspy.OutputField(desc="Key findings")
    risk_level = dspy.OutputField(desc="Risk: low, medium, high")
```

### Chain of Thought
```python
# Adds step-by-step reasoning
cot = dspy.ChainOfThought("question -> answer")
result = cot(question="Complex question...")
print(result.rationale)  # Shows reasoning
print(result.answer)
```

### Optimization
```python
from dspy.teleprompt import BootstrapFewShot

optimizer = BootstrapFewShot(
    metric=accuracy_metric,
    max_bootstrapped_demos=4
)

optimized = optimizer.compile(module, trainset=examples)
optimized.save("optimized_model.json")
```

### RAG
```python
class RAGModule(dspy.Module):
    def __init__(self):
        super().__init__()
        self.retrieve = dspy.Retrieve(k=5)
        self.generate = dspy.ChainOfThought("context, question -> answer")

    def forward(self, question):
        context = self.retrieve(question).passages
        return self.generate(context=context, question=question)
```

## Comparison with Manual Prompting

| Aspect | Manual Prompting | DSPy |
|--------|-----------------|------|
| Iteration | Trial and error | Systematic optimization |
| Reproducibility | Hard to version | Save/load modules |
| Evaluation | Ad-hoc testing | Metrics and benchmarks |
| Scaling | Rewrite prompts | Recompile with new data |

## Related Skills

- [langchain](../langchain/SKILL.md) - Application framework
- [prompt-engineering](../prompt-engineering/SKILL.md) - Core prompting patterns

## Resources

- [DSPy Docs](https://dspy-docs.vercel.app/)
- [DSPy GitHub](https://github.com/stanfordnlp/dspy)
- [DSPy Paper](https://arxiv.org/abs/2310.03714)

---

**Version**: 1.0.0
**Category**: ai-prompting
