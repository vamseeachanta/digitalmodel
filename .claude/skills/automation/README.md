# Automation Skills Library

> Workflow automation, CI/CD pipelines, and task orchestration patterns
> Version: 1.0.0 | Last Updated: 2026-01-17

## Overview

This library contains 5 production-ready automation skills for building workflows, pipelines, and orchestration systems. Each skill covers a specific automation platform with patterns for integration, error handling, and scaling. Skills follow the Anthropic Skills format with practical examples from real-world automation scenarios.

## Quick Start

```bash
# Browse available skills
ls skills/automation/

# Read a skill
cat skills/automation/n8n/SKILL.md

# Skills are documentation - implement patterns in your automation tools
```

## Available Skills

| Skill | Description | Key Features |
|-------|-------------|--------------|
| [n8n](./n8n/SKILL.md) | Self-hosted workflow automation | Visual workflows, 400+ integrations, webhooks |
| [activepieces](./activepieces/SKILL.md) | Open-source automation platform | Type-safe pieces, branching logic, loops |
| [airflow](./airflow/SKILL.md) | Workflow orchestration for data pipelines | DAGs, task dependencies, scheduling |
| [github-actions](./github-actions/SKILL.md) | CI/CD and repository automation | Matrix builds, reusable workflows, artifacts |
| [windmill](./windmill/SKILL.md) | Developer-first workflow engine | Scripts as workflows, TypeScript/Python, UI generation |

## Skill Categories

### Low-Code Visual Automation
- **n8n** - Drag-and-drop workflow builder with extensive integrations
- **activepieces** - Modern automation with type-safe custom pieces

### Data Pipeline Orchestration
- **airflow** - Enterprise-grade DAG-based workflow scheduling
- **windmill** - Scripts-first approach with auto-generated UIs

### CI/CD & Repository Automation
- **github-actions** - Native GitHub workflows for builds, tests, deployments

## Skill Selection Guide

### Choose n8n when:
- You need visual workflow design for non-developers
- Integrating 400+ services (Slack, Gmail, Notion, etc.)
- Building internal tool automations quickly
- Self-hosting is required for data sovereignty

### Choose activepieces when:
- Type safety and custom piece development are priorities
- Building modular, reusable automation components
- Need branching logic and loops in workflows
- Open-source with commercial-friendly licensing

### Choose airflow when:
- Orchestrating complex data pipelines
- Managing task dependencies and retries
- Scheduling recurring batch jobs
- Enterprise features (RBAC, audit logs) are required

### Choose github-actions when:
- Automating repository workflows (CI/CD, releases)
- Running tests across multiple OS/language matrices
- Building and publishing packages/containers
- Leveraging GitHub ecosystem (Issues, PRs, Releases)

### Choose windmill when:
- Developers prefer writing scripts over visual tools
- Need auto-generated UIs for script parameters
- TypeScript or Python are primary languages
- Combining workflow automation with internal tools

## Quick Examples

### n8n Webhook Workflow
```json
{
  "nodes": [
    {
      "name": "Webhook",
      "type": "n8n-nodes-base.webhook",
      "parameters": {
        "path": "incoming-data",
        "httpMethod": "POST"
      }
    },
    {
      "name": "Transform",
      "type": "n8n-nodes-base.set",
      "parameters": {
        "values": {
          "string": [
            { "name": "processed", "value": "={{ $json.data }}" }
          ]
        }
      }
    }
  ]
}
```

### Airflow DAG Definition
```python
from airflow import DAG
from airflow.operators.python import PythonOperator
from datetime import datetime, timedelta

default_args = {
    'owner': 'data-team',
    'retries': 3,
    'retry_delay': timedelta(minutes=5)
}

with DAG(
    'data_pipeline',
    default_args=default_args,
    schedule_interval='0 6 * * *',
    start_date=datetime(2026, 1, 1),
    catchup=False
) as dag:

    extract = PythonOperator(
        task_id='extract_data',
        python_callable=extract_from_source
    )

    transform = PythonOperator(
        task_id='transform_data',
        python_callable=apply_transformations
    )

    load = PythonOperator(
        task_id='load_data',
        python_callable=load_to_warehouse
    )

    extract >> transform >> load
```

### GitHub Actions Workflow
```yaml
name: CI Pipeline

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        python-version: ['3.10', '3.11', '3.12']

    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: ${{ matrix.python-version }}

      - name: Install dependencies
        run: pip install -r requirements.txt

      - name: Run tests
        run: pytest --cov=src tests/
```

### Windmill Script Workflow
```typescript
// windmill script with auto-generated UI
export async function main(
  database_url: string,
  table_name: string,
  limit: number = 100
): Promise<Record<string, any>[]> {
  const client = new Client(database_url);

  const result = await client.query(
    `SELECT * FROM ${table_name} LIMIT $1`,
    [limit]
  );

  return result.rows;
}
```

## Integration Patterns

### Webhook-to-Workflow Pattern
```
External Service --> Webhook --> Automation Platform --> Actions
     |                              |
     +-- Retry logic               +-- Error notifications
     +-- Signature validation      +-- Audit logging
```

### Scheduled Pipeline Pattern
```
Scheduler --> Check Dependencies --> Execute Tasks --> Post-processing
    |                 |                    |                |
    +-- Cron/interval +-- Data freshness  +-- Retries      +-- Alerts
    +-- Timezone      +-- Resource locks  +-- Timeouts     +-- Metrics
```

### Event-Driven Automation
```
Event Source --> Message Queue --> Worker --> Action
    |                |                |           |
    +-- Git push    +-- Buffering    +-- Scale   +-- Notifications
    +-- File change +-- Ordering     +-- Isolate +-- Data updates
```

## Common Patterns Across Skills

### Error Handling
```python
# Retry with exponential backoff
def with_retry(func, max_attempts=3, base_delay=1):
    for attempt in range(max_attempts):
        try:
            return func()
        except Exception as e:
            if attempt == max_attempts - 1:
                raise
            delay = base_delay * (2 ** attempt)
            time.sleep(delay)
```

### Secret Management
```yaml
# Use environment variables or secret stores
env:
  API_KEY: ${{ secrets.API_KEY }}
  DATABASE_URL: ${{ secrets.DATABASE_URL }}
```

### Idempotency
```python
# Ensure operations can be safely retried
def process_with_idempotency(record_id, data):
    existing = get_by_id(record_id)
    if existing and existing.checksum == compute_checksum(data):
        return existing  # Already processed
    return upsert(record_id, data)
```

## Integration with Workspace-Hub

These skills power automation across the workspace-hub ecosystem:

```
workspace-hub/
├── .github/
│   └── workflows/           # Uses: github-actions
│       ├── ci.yml
│       ├── release.yml
│       └── sync.yml
├── automation/
│   ├── n8n/                 # Uses: n8n
│   │   └── workflows/
│   ├── airflow/             # Uses: airflow
│   │   └── dags/
│   └── windmill/            # Uses: windmill
│       └── scripts/
└── config/
    └── automation.yaml      # Shared automation config
```

## Best Practices

### 1. Version Control Workflows
```bash
# Store workflow definitions in git
git add automation/workflows/
git commit -m "feat: Add data sync workflow"
```

### 2. Environment Separation
```yaml
# Use environment-specific configurations
production:
  schedule: "0 */6 * * *"
  parallelism: 10
development:
  schedule: null  # Manual trigger only
  parallelism: 2
```

### 3. Monitoring and Alerting
```python
# Add observability to all workflows
def with_monitoring(workflow_name):
    start_time = time.time()
    try:
        result = execute_workflow()
        metrics.record_success(workflow_name, time.time() - start_time)
        return result
    except Exception as e:
        metrics.record_failure(workflow_name, str(e))
        alerts.send(f"Workflow {workflow_name} failed: {e}")
        raise
```

### 4. Documentation
```yaml
# Document workflow purpose and dependencies
# ABOUTME: Syncs customer data from CRM to data warehouse
# ABOUTME: Depends on: crm-api, warehouse-connection
# ABOUTME: Schedule: Every 6 hours
```

## Testing Automation

```python
# Test workflow logic in isolation
def test_transform_step():
    input_data = {"raw": "value"}
    expected = {"processed": "VALUE"}

    result = transform_step(input_data)

    assert result == expected

# Integration tests with mocked services
def test_workflow_end_to_end(mock_api):
    mock_api.return_value = {"status": "ok"}

    result = run_workflow("test-workflow")

    assert result.success
    assert mock_api.called
```

## Related Resources

- [n8n Documentation](https://docs.n8n.io/)
- [Apache Airflow Docs](https://airflow.apache.org/docs/)
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Windmill Documentation](https://www.windmill.dev/docs)
- [Activepieces Docs](https://www.activepieces.com/docs)

## Version History

- **1.0.0** (2026-01-17): Initial release with 5 automation skills

---

*These skills represent patterns refined across production automation systems handling thousands of workflow executions daily.*
