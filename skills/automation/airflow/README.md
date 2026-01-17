# Apache Airflow Skill

> **Quick Reference Guide**

## Overview

Python DAG workflow orchestration using Apache Airflow for data pipelines, ETL processes, and scheduled task automation.

**Version**: 1.0.0
**Category**: automation
**Platforms**: linux, macos, docker, kubernetes

## Quick Start

### 1. Docker Compose Setup (Recommended)

```bash
# Download official docker-compose
curl -LfO 'https://airflow.apache.org/docs/apache-airflow/2.8.1/docker-compose.yaml'

# Create directories
mkdir -p ./dags ./logs ./plugins ./config
echo -e "AIRFLOW_UID=$(id -u)" > .env

# Initialize and start
docker compose up airflow-init
docker compose up -d

# Access UI at http://localhost:8080 (airflow/airflow)
```

### 2. Basic DAG

```python
# dags/my_pipeline.py
from datetime import datetime, timedelta
from airflow import DAG
from airflow.operators.python import PythonOperator

default_args = {
    'owner': 'data-team',
    'retries': 3,
    'retry_delay': timedelta(minutes=5),
}

with DAG(
    dag_id='my_pipeline',
    default_args=default_args,
    schedule_interval='0 6 * * *',  # Daily at 6 AM
    start_date=datetime(2026, 1, 1),
    catchup=False,
) as dag:

    def extract(**context):
        return {'records': 1000}

    def transform(**context):
        data = context['ti'].xcom_pull(task_ids='extract')
        return {'processed': data['records']}

    extract_task = PythonOperator(
        task_id='extract',
        python_callable=extract,
    )

    transform_task = PythonOperator(
        task_id='transform',
        python_callable=transform,
    )

    extract_task >> transform_task
```

## Key Capabilities

- **DAG Authoring**: Define workflows as Python code
- **Operators**: PythonOperator, BashOperator, SQL, HTTP, etc.
- **Sensors**: Wait for files, APIs, external DAGs
- **Hooks**: Connect to databases, cloud services, APIs
- **XComs**: Inter-task communication
- **Variables/Connections**: Configuration management
- **Scheduling**: Cron-based, data-interval aware
- **Task Groups**: Logical task organization

## Common Patterns

### Task Dependencies
```python
task1 >> task2 >> task3
task1 >> [task2, task3] >> task4
```

### Branching
```python
from airflow.operators.python import BranchPythonOperator

def choose_branch(**context):
    return 'path_a' if condition else 'path_b'

branch = BranchPythonOperator(
    task_id='branch',
    python_callable=choose_branch,
)
```

### Sensors
```python
from airflow.sensors.filesystem import FileSensor

wait_for_file = FileSensor(
    task_id='wait_for_file',
    filepath='/data/incoming/{{ ds }}/data.csv',
    poke_interval=60,
    timeout=3600,
)
```

### XCom Communication
```python
# Push (implicit via return)
def producer(**ctx):
    return {'key': 'value'}

# Pull
def consumer(**ctx):
    data = ctx['ti'].xcom_pull(task_ids='producer')
```

## Files

```
airflow/
├── SKILL.md    # Full documentation (900+ lines)
└── README.md   # This quick reference
```

## Installation Options

| Method | Command |
|--------|---------|
| pip | `pip install apache-airflow==2.8.1` |
| Docker | `docker compose up -d` |
| Helm | `helm install airflow apache-airflow/airflow` |

## CLI Commands

```bash
# List DAGs
airflow dags list

# Test task
airflow tasks test my_dag my_task 2026-01-15

# Trigger DAG
airflow dags trigger my_dag

# Check import errors
airflow dags list-import-errors
```

## Related Skills

- **yaml-configuration** - Configuration patterns
- **pandas-data-processing** - Data transformation
- **python-scientific-computing** - Numerical tasks

## Resources

- [Airflow Documentation](https://airflow.apache.org/docs/)
- [Airflow Best Practices](https://airflow.apache.org/docs/apache-airflow/stable/best-practices.html)
- [Helm Chart](https://airflow.apache.org/docs/helm-chart/stable/)

---

**See SKILL.md for complete documentation with 8+ comprehensive examples.**
