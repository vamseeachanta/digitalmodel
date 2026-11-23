# Minimal Subagent Structure with Rules

## Core Folder Structure

```
agents/
├── main_agent.py                # Main orchestrator agent
├── rules/
│   ├── base_rules.yaml          # Core rules all agents must follow
│   ├── communication_rules.yaml # Inter-agent communication rules
│   ├── orchestration_rules.yaml # Main agent orchestration rules
│   └── validation_rules.yaml    # Input/output validation rules
├── core/
│   ├── base_agent.py           # Base agent class with rule enforcement
│   ├── main_agent_base.py      # Base class for main orchestrator
│   ├── rule_engine.py          # Rule validation and enforcement
│   ├── message_bus.py          # Message passing system
│   ├── orchestrator.py         # Orchestration engine
│   └── task_manager.py         # Task distribution and tracking
├── subagents/
│   ├── [agent_name]/
│   │   ├── agent.py            # Agent implementation
│   │   ├── config.yaml         # Agent-specific config
│   │   └── rules.yaml          # Agent-specific rules (optional)
│   ├── processor/
│   │   ├── agent.py
│   │   ├── config.yaml
│   │   └── rules.yaml
│   └── notifier/
│       ├── agent.py
│       ├── config.yaml
│       └── rules.yaml
├── workflows/
│   ├── workflow_definitions.yaml # Define orchestration workflows
│   └── task_templates.yaml      # Reusable task templates
├── shared/
│   ├── interfaces.py           # Agent interfaces
│   ├── models.py              # Task, workflow, and message models
│   └── utils.py               # Common utilities
└── tests/
    ├── test_rules/             # Rule validation tests
    ├── test_orchestration/     # Orchestration tests
    └── test_agents/            # Agent-specific tests
```

## Rule Files

### rules/base_rules.yaml
```yaml
# Core rules that ALL subagents must follow
agent_rules:
  naming:
    class_name_pattern: "^[A-Z][a-zA-Z]*Agent$"  # Must end with 'Agent'
    file_name_pattern: "^[a-z_]+\\.py$"          # Lowercase with underscores
    
  structure:
    required_methods:
      - "initialize"
      - "process"
      - "shutdown"
      - "get_status"
    required_files:
      - "agent.py"
      - "config.yaml"
      
  behavior:
    max_processing_time: 30      # seconds
    max_memory_usage: 512        # MB
    required_logging: true
    error_handling: "graceful"   # must handle errors gracefully
    
  communication:
    must_use_message_bus: true
    no_direct_imports: true      # can't import other agents directly
    message_timeout: 10          # seconds
```

### rules/communication_rules.yaml
```yaml
# Rules for inter-agent communication
communication:
  message_format:
    required_fields:
      - "sender"
      - "recipient" 
      - "message_type"
      - "timestamp"
      - "payload"
    max_message_size: 1048576    # 1MB
    
  protocols:
    allowed:
      - "async_message"
      - "request_response"
    forbidden:
      - "direct_call"
      - "shared_memory"
      
  rate_limits:
    max_messages_per_second: 100
    max_concurrent_requests: 10
```

### rules/orchestration_rules.yaml
```yaml
# Rules for main agent orchestration
orchestration:
  main_agent:
    responsibilities:
      - "task_distribution"
      - "workflow_management" 
      - "subagent_coordination"
      - "result_aggregation"
    required_methods:
      - "create_workflow"
      - "distribute_tasks"
      - "monitor_progress"
      - "handle_failures"
      
  task_management:
    max_concurrent_tasks: 50
    task_timeout: 300            # 5 minutes
    retry_attempts: 3
    failure_escalation: true
    
  subagent_coordination:
    discovery_required: true     # Main agent must discover subagents
    health_monitoring: true      # Monitor subagent health
    load_balancing: "round_robin" # Task distribution strategy
    
  workflow:
    max_workflow_depth: 10       # Prevent infinite loops
    parallel_execution: true     # Allow parallel task execution
    dependency_validation: true  # Validate task dependencies
```

### rules/validation_rules.yaml
```yaml
# Input/output validation rules
validation:
  input:
    required_validation: true
    sanitization: "strict"
    type_checking: "enforced"
    
  output:
    format_consistency: true
    error_codes: "standardized"
    logging_required: true
    
  data:
    no_sensitive_data_logging: true
    encryption_required: false   # can be overridden per agent
    retention_days: 30
```

## Orchestration Components

### core/main_agent_base.py
```python
from abc import ABC, abstractmethod
from .base_agent import BaseAgent
from .orchestrator import Orchestrator
from .task_manager import TaskManager
from .message_bus import MessageBus
import asyncio
import yaml

class MainAgentBase(BaseAgent):
    def __init__(self, agent_name: str = "main_agent"):
        super().__init__(agent_name)
        self.orchestrator = Orchestrator(self.rules)
        self.task_manager = TaskManager(self.rules)
        self.message_bus = MessageBus()
        self.subagents = {}
        self.workflows = self._load_workflows()
        
    def _load_workflows(self):
        with open('workflows/workflow_definitions.yaml', 'r') as f:
            return yaml.safe_load(f)
            
    async def initialize(self) -> bool:
        """Initialize main agent and discover subagents"""
        # Initialize message bus
        await self.message_bus.initialize()
        
        # Discover and register subagents
        await self._discover_subagents()
        
        # Start orchestration engine
        await self.orchestrator.start()
        
        return True
        
    async def _discover_subagents(self):
        """Discover available subagents"""
        import os
        subagent_dirs = [d for d in os.listdir('subagents') 
                        if os.path.isdir(f'subagents/{d}')]
        
        for agent_dir in subagent_dirs:
            try:
                # Load subagent configuration
                with open(f'subagents/{agent_dir}/config.yaml', 'r') as f:
                    config = yaml.safe_load(f)
                    
                self.subagents[agent_dir] = {
                    'name': agent_dir,
                    'config': config,
                    'status': 'discovered',
                    'capabilities': config.get('capabilities', [])
                }
                print(f"Discovered subagent: {agent_dir}")
            except Exception as e:
                print(f"Failed to discover subagent {agent_dir}: {e}")
                
    async def create_workflow(self, workflow_name: str, input_data: dict) -> str:
        """Create and execute a workflow"""
        if workflow_name not in self.workflows:
            raise ValueError(f"Unknown workflow: {workflow_name}")
            
        workflow_def = self.workflows[workflow_name]
        workflow_id = await self.orchestrator.create_workflow(workflow_def, input_data)
        
        return workflow_id
        
    async def distribute_task(self, task_type: str, task_data: dict, target_agent: str = None) -> str:
        """Distribute a task to appropriate subagent"""
        if target_agent:
            # Direct assignment
            if target_agent not in self.subagents:
                raise ValueError(f"Unknown subagent: {target_agent}")
            selected_agent = target_agent
        else:
            # Auto-select based on capabilities
            selected_agent = self._select_agent_for_task(task_type)
            
        task_id = await self.task_manager.create_task(
            task_type=task_type,
            task_data=task_data,
            assigned_agent=selected_agent
        )
        
        # Send task to subagent
        await self.message_bus.send_message(
            recipient=selected_agent,
            message_type="task_assignment",
            payload={
                "task_id": task_id,
                "task_type": task_type,
                "data": task_data
            }
        )
        
        return task_id
        
    def _select_agent_for_task(self, task_type: str) -> str:
        """Select appropriate subagent for task based on capabilities"""
        suitable_agents = []
        for agent_name, agent_info in self.subagents.items():
            capabilities = agent_info.get('capabilities', [])
            if task_type in capabilities or 'general' in capabilities:
                suitable_agents.append(agent_name)
                
        if not suitable_agents:
            raise ValueError(f"No suitable agent found for task type: {task_type}")
            
        # Simple round-robin selection
        return suitable_agents[0]  # Can implement load balancing here
        
    async def monitor_progress(self) -> dict:
        """Monitor overall progress of tasks and workflows"""
        return {
            "active_workflows": await self.orchestrator.get_active_workflows(),
            "pending_tasks": await self.task_manager.get_pending_tasks(),
            "subagent_status": {name: info['status'] for name, info in self.subagents.items()}
        }
        
    async def handle_task_result(self, task_id: str, result: dict, from_agent: str):
        """Handle task completion from subagent"""
        await self.task_manager.complete_task(task_id, result)
        
        # Check if this completes any workflows
        await self.orchestrator.handle_task_completion(task_id, result)
        
    async def handle_failures(self, task_id: str, error: str, from_agent: str):
        """Handle task failures and retry logic"""
        await self.task_manager.handle_task_failure(task_id, error)
        
        # Implement retry logic based on rules
        retry_attempts = self.rules['orchestration']['task_management']['retry_attempts']
        if await self.task_manager.get_retry_count(task_id) < retry_attempts:
            # Retry the task
            task_data = await self.task_manager.get_task_data(task_id)
            await self.distribute_task(
                task_type=task_data['type'],
                task_data=task_data['data'],
                target_agent=from_agent
            )
```

### core/orchestrator.py
```python
import asyncio
import uuid
from typing import Dict, List, Any
from shared.models import Workflow, Task

class Orchestrator:
    def __init__(self, rules: dict):
        self.rules = rules
        self.active_workflows = {}
        self.workflow_templates = {}
        
    async def start(self):
        """Start the orchestration engine"""
        # Start background monitoring
        asyncio.create_task(self._monitor_workflows())
        
    async def create_workflow(self, workflow_def: dict, input_data: dict) -> str:
        """Create a new workflow instance"""
        workflow_id = str(uuid.uuid4())
        
        workflow = Workflow(
            id=workflow_id,
            definition=workflow_def,
            input_data=input_data,
            status="created"
        )
        
        self.active_workflows[workflow_id] = workflow
        
        # Start workflow execution
        asyncio.create_task(self._execute_workflow(workflow))
        
        return workflow_id
        
    async def _execute_workflow(self, workflow: Workflow):
        """Execute workflow steps"""
        try:
            workflow.status = "running"
            
            for step in workflow.definition['steps']:
                await self._execute_step(workflow, step)
                
            workflow.status = "completed"
            
        except Exception as e:
            workflow.status = "failed"
            workflow.error = str(e)
            
    async def _execute_step(self, workflow: Workflow, step: dict):
        """Execute a single workflow step"""
        step_type = step['type']
        
        if step_type == "task":
            # Create and assign task
            task_id = await self._create_task_from_step(workflow, step)
            workflow.active_tasks.append(task_id)
            
        elif step_type == "parallel":
            # Execute multiple steps in parallel
            tasks = []
            for parallel_step in step['steps']:
                task = asyncio.create_task(self._execute_step(workflow, parallel_step))
                tasks.append(task)
            await asyncio.gather(*tasks)
            
        elif step_type == "condition":
            # Conditional execution
            condition = step['condition']
            if self._evaluate_condition(condition, workflow.context):
                for conditional_step in step['then']:
                    await self._execute_step(workflow, conditional_step)
            else:
                for conditional_step in step.get('else', []):
                    await self._execute_step(workflow, conditional_step)
                    
    async def _monitor_workflows(self):
        """Monitor workflow progress and timeouts"""
        while True:
            for workflow_id, workflow in list(self.active_workflows.items()):
                if workflow.status == "completed" or workflow.status == "failed":
                    # Clean up completed workflows
                    del self.active_workflows[workflow_id]
                    
            await asyncio.sleep(10)  # Check every 10 seconds
            
    async def get_active_workflows(self) -> List[dict]:
        """Get status of all active workflows"""
        return [
            {
                "id": wf.id,
                "status": wf.status, 
                "active_tasks": len(wf.active_tasks)
            }
            for wf in self.active_workflows.values()
        ]
        
    async def handle_task_completion(self, task_id: str, result: dict):
        """Handle task completion and update workflows"""
        for workflow in self.active_workflows.values():
            if task_id in workflow.active_tasks:
                workflow.active_tasks.remove(task_id)
                workflow.context[f"task_{task_id}_result"] = result
```

### core/task_manager.py
```python
import uuid
import asyncio
from typing import Dict, List
from shared.models import Task

class TaskManager:
    def __init__(self, rules: dict):
        self.rules = rules
        self.active_tasks = {}
        self.completed_tasks = {}
        self.failed_tasks = {}
        
    async def create_task(self, task_type: str, task_data: dict, assigned_agent: str) -> str:
        """Create a new task"""
        task_id = str(uuid.uuid4())
        
        task = Task(
            id=task_id,
            type=task_type,
            data=task_data,
            assigned_agent=assigned_agent,
            status="created"
        )
        
        self.active_tasks[task_id] = task
        
        # Set timeout
        timeout = self.rules['orchestration']['task_management']['task_timeout']
        asyncio.create_task(self._handle_task_timeout(task_id, timeout))
        
        return task_id
        
    async def complete_task(self, task_id: str, result: dict):
        """Mark task as completed"""
        if task_id in self.active_tasks:
            task = self.active_tasks[task_id]
            task.status = "completed"
            task.result = result
            task.completed_at = asyncio.get_event_loop().time()
            
            self.completed_tasks[task_id] = task
            del self.active_tasks[task_id]
            
    async def handle_task_failure(self, task_id: str, error: str):
        """Handle task failure"""
        if task_id in self.active_tasks:
            task = self.active_tasks[task_id]
            task.status = "failed"
            task.error = error
            task.retry_count += 1
            
            max_retries = self.rules['orchestration']['task_management']['retry_attempts']
            if task.retry_count >= max_retries:
                self.failed_tasks[task_id] = task
                del self.active_tasks[task_id]
                
    async def _handle_task_timeout(self, task_id: str, timeout: int):
        """Handle task timeout"""
        await asyncio.sleep(timeout)
        
        if task_id in self.active_tasks:
            await self.handle_task_failure(task_id, "Task timeout")
            
    async def get_pending_tasks(self) -> List[dict]:
        """Get all pending tasks"""
        return [
            {
                "id": task.id,
                "type": task.type,
                "assigned_agent": task.assigned_agent,
                "status": task.status
            }
            for task in self.active_tasks.values()
        ]
```

## Workflow Configuration

### workflows/workflow_definitions.yaml
```yaml
# Define reusable workflows
workflows:
  data_processing_pipeline:
    description: "Complete data processing workflow"
    steps:
      - type: "task"
        name: "validate_input"
        agent_type: "validator"
        task_type: "validate_data"
        
      - type: "task" 
        name: "process_data"
        agent_type: "processor"
        task_type: "process_data"
        depends_on: ["validate_input"]
        
      - type: "parallel"
        steps:
          - type: "task"
            name: "generate_report"
            agent_type: "reporter"
            task_type: "generate_report"
            
          - type: "task"
            name: "send_notification"
            agent_type: "notifier"
            task_type: "send_notification"
            
  user_request_workflow:
    description: "Handle user requests"
    steps:
      - type: "task"
        name: "parse_request"
        agent_type: "parser"
        task_type: "parse_user_input"
        
      - type: "condition"
        condition: "request_type == 'data_query'"
        then:
          - type: "task"
            name: "query_data"
            agent_type: "data_agent"
            task_type: "query_database"
        else:
          - type: "task"
            name: "handle_general"
            agent_type: "general_agent"
            task_type: "handle_request"
```

## Enhanced Subagent Implementation

### Enhanced subagents/processor/agent.py
```python
from core.base_agent import BaseAgent
from core.message_bus import MessageBus
import asyncio
import logging

class ProcessorAgent(BaseAgent):
    def __init__(self):
        super().__init__('processor')
        self.message_bus = MessageBus()
        
    async def initialize(self) -> bool:
        await self.message_bus.initialize()
        await self.message_bus.register_agent(self.agent_name, self._handle_message)
        logging.info(f"Initialized {self.agent_name}")
        return True
        
    async def _handle_message(self, message: dict):
        """Handle incoming messages from main agent"""
        message_type = message.get('message_type')
        
        if message_type == "task_assignment":
            await self._handle_task_assignment(message)
        elif message_type == "health_check":
            await self._handle_health_check(message)
        else:
            logging.warning(f"Unknown message type: {message_type}")
            
    async def _handle_task_assignment(self, message: dict):
        """Handle task assignment from main agent"""
        payload = message['payload']
        task_id = payload['task_id']
        task_type = payload['task_type']
        data = payload['data']
        
        try:
            # Process the task
            result = await self.process({"type": task_type, "data": data})
            
            # Send result back to main agent
            await self.message_bus.send_message(
                recipient="main_agent",
                message_type="task_result",
                payload={
                    "task_id": task_id,
                    "result": result,
                    "from_agent": self.agent_name
                }
            )
            
        except Exception as e:
            # Send error back to main agent
            await self.message_bus.send_message(
                recipient="main_agent", 
                message_type="task_error",
                payload={
                    "task_id": task_id,
                    "error": str(e),
                    "from_agent": self.agent_name
                }
            )
            
    async def process(self, message: dict) -> dict:
        # Simulate processing
        await asyncio.sleep(1)
        return {"status": "processed", "data": message.get("data", {})}
        
    def get_status(self) -> dict:
        return {
            "name": self.agent_name,
            "status": "running",
            "capabilities": ["process_data", "validate_data"]
        }
```

### subagents/processor/config.yaml
```yaml
agent:
  name: "processor"
  version: "1.0.0"
  description: "Data processing agent"
  
capabilities:
  - "process_data"
  - "validate_data"
  - "transform_data"
  
runtime:
  max_concurrent_tasks: 5
  timeout_seconds: 15
```

## Main Agent Implementation

### main_agent.py
```python
from core.main_agent_base import MainAgentBase
import asyncio

class MainAgent(MainAgentBase):
    async def handle_user_request(self, request: dict) -> dict:
        """Handle incoming user requests"""
        request_type = request.get('type', 'general')
        
        if request_type == 'data_processing':
            # Create workflow for data processing
            workflow_id = await self.create_workflow(
                'data_processing_pipeline',
                request.get('data', {})
            )
            
            return {
                "status": "workflow_created",
                "workflow_id": workflow_id,
                "message": "Data processing workflow started"
            }
            
        elif request_type == 'simple_task':
            # Direct task assignment
            task_id = await self.distribute_task(
                task_type=request.get('task_type', 'process_data'),
                task_data=request.get('data', {}),
                target_agent=request.get('preferred_agent')
            )
            
            return {
                "status": "task_assigned", 
                "task_id": task_id,
                "message": "Task assigned to subagent"
            }
            
        else:
            return {
                "status": "error",
                "message": f"Unknown request type: {request_type}"
            }

# Usage example
async def main():
    main_agent = MainAgent()
    await main_agent.initialize()
    
    # Handle a user request
    result = await main_agent.handle_user_request({
        "type": "data_processing",
        "data": {"input": [1, 2, 3, 4, 5]}
    })
    
    print(f"Result: {result}")
    
    # Monitor progress
    progress = await main_agent.monitor_progress()
    print(f"Progress: {progress}")

if __name__ == "__main__":
    asyncio.run(main())
```

### core/base_agent.py
```python
from abc import ABC, abstractmethod
from .rule_engine import RuleEngine
import yaml
import logging

class BaseAgent(ABC):
    def __init__(self, agent_name: str):
        self.agent_name = agent_name
        self.rule_engine = RuleEngine()
        self.config = self._load_config()
        self.rules = self._load_rules()
        self._validate_against_rules()
        
    def _load_config(self):
        with open(f'subagents/{self.agent_name}/config.yaml', 'r') as f:
            return yaml.safe_load(f)
            
    def _load_rules(self):
        # Load base rules + agent-specific rules
        base_rules = self.rule_engine.load_base_rules()
        try:
            with open(f'subagents/{self.agent_name}/rules.yaml', 'r') as f:
                agent_rules = yaml.safe_load(f)
            return self.rule_engine.merge_rules(base_rules, agent_rules)
        except FileNotFoundError:
            return base_rules
            
    def _validate_against_rules(self):
        """Validate this agent against all applicable rules"""
        self.rule_engine.validate_agent(self, self.rules)
        
    @abstractmethod
    def initialize(self) -> bool:
        """Initialize the agent"""
        pass
        
    @abstractmethod
    def process(self, message: dict) -> dict:
        """Process incoming message"""
        pass
        
    @abstractmethod
    def shutdown(self) -> bool:
        """Shutdown the agent"""
        pass
        
    @abstractmethod
    def get_status(self) -> dict:
        """Get agent status"""
        pass
```

### core/rule_engine.py
```python
import yaml
import inspect
import logging
from typing import Dict, Any

class RuleEngine:
    def __init__(self):
        self.base_rules = self._load_base_rules()
        
    def _load_base_rules(self):
        rules = {}
        rule_files = [
            'rules/base_rules.yaml',
            'rules/communication_rules.yaml', 
            'rules/validation_rules.yaml'
        ]
        for file_path in rule_files:
            with open(file_path, 'r') as f:
                rules.update(yaml.safe_load(f))
        return rules
        
    def validate_agent(self, agent, rules):
        """Validate agent against rules"""
        self._validate_structure(agent, rules)
        self._validate_methods(agent, rules)
        self._validate_behavior(agent, rules)
        
    def _validate_structure(self, agent, rules):
        """Check required files and naming conventions"""
        # Validate class name
        class_name = agent.__class__.__name__
        pattern = rules['agent_rules']['naming']['class_name_pattern']
        if not self._matches_pattern(class_name, pattern):
            raise ValueError(f"Agent class name '{class_name}' doesn't match pattern '{pattern}'")
            
    def _validate_methods(self, agent, rules):
        """Check required methods exist"""
        required_methods = rules['agent_rules']['structure']['required_methods']
        agent_methods = [method for method in dir(agent) if not method.startswith('_')]
        
        for required_method in required_methods:
            if required_method not in agent_methods:
                raise ValueError(f"Agent missing required method: {required_method}")
                
    def _validate_behavior(self, agent, rules):
        """Validate behavioral rules"""
        # Add runtime behavior validation here
        pass
        
    def _matches_pattern(self, text, pattern):
        import re
        return bool(re.match(pattern, text))
        
    def load_base_rules(self):
        return self.base_rules
        
    def merge_rules(self, base_rules, agent_rules):
        """Merge base rules with agent-specific rules"""
        merged = base_rules.copy()
        if agent_rules:
            # Agent rules can override base rules
            self._deep_merge(merged, agent_rules)
        return merged
        
    def _deep_merge(self, base_dict, override_dict):
        """Deep merge two dictionaries"""
        for key, value in override_dict.items():
            if key in base_dict and isinstance(base_dict[key], dict) and isinstance(value, dict):
                self._deep_merge(base_dict[key], value)
            else:
                base_dict[key] = value
```

## Example Agent Implementation

### subagents/processor/agent.py
```python
from core.base_agent import BaseAgent
import logging

class ProcessorAgent(BaseAgent):
    def __init__(self):
        super().__init__('processor')
        
    def initialize(self) -> bool:
        logging.info(f"Initializing {self.agent_name}")
        # Setup resources
        return True
        
    def process(self, message: dict) -> dict:
        # Validate input according to rules
        self.rule_engine.validate_input(message, self.rules)
        
        # Process the message
        result = {"status": "processed", "data": message.get("data", {})}
        
        # Validate output according to rules  
        self.rule_engine.validate_output(result, self.rules)
        
        return result
        
    def shutdown(self) -> bool:
        logging.info(f"Shutting down {self.agent_name}")
        return True
        
    def get_status(self) -> dict:
        return {
            "name": self.agent_name,
            "status": "running",
            "rules_compliant": True
        }
```

### subagents/processor/config.yaml
```yaml
agent:
  name: "processor"
  version: "1.0.0"
  description: "Data processing agent"
  
runtime:
  max_concurrent_tasks: 5
  timeout_seconds: 15
  
logging:
  level: "INFO"
  file: "processor.log"
```

### subagents/processor/rules.yaml (optional)
```yaml
# Agent-specific rules that override or extend base rules
agent_rules:
  behavior:
    max_processing_time: 20    # Override: shorter than base rule of 30
    custom_validation: true    # Extend: add custom validation
    
  data:
    allowed_formats:
      - "json"
      - "csv" 
      - "xml"
```

## Usage Example

```python
# main.py
from subagents.processor.agent import ProcessorAgent

def main():
    # Create agent (automatically validates against rules)
    processor = ProcessorAgent()
    
    # Initialize
    if processor.initialize():
        # Process message
        message = {
            "sender": "user",
            "recipient": "processor", 
            "message_type": "data_process",
            "timestamp": "2025-01-01T10:00:00Z",
            "payload": {"data": [1, 2, 3]}
        }
        
        result = processor.process(message)
        print(f"Result: {result}")
        
        # Shutdown
        processor.shutdown()

if __name__ == "__main__":
    main()
```

## Key Benefits

1. **Minimal Structure** - Only essential folders and files
2. **Rule Enforcement** - Built-in validation at agent creation
3. **Flexibility** - Agents can override base rules when needed
4. **Consistency** - All agents follow the same pattern
5. **Easy to Extend** - Simple to add new rules or agents

## Getting Started Steps

1. Define your base rules in `rules/base_rules.yaml`
2. Create a new agent by copying the template structure
3. Implement the required methods in `agent.py`
4. Add agent-specific config in `config.yaml`
5. Optionally override rules in `rules.yaml`
6. The base agent class automatically validates compliance

This structure grows with your needs while maintaining simplicity and rule compliance from day one.