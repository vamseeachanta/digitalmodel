# AI Guidelines

Please follow [AI Guidelines directory](https://github.com/vamseeachanta/pyproject-starter/tree/master/.ai) 
for all development work.

## Specific Sections
- [Python programming](https://github.com/vamseeachanta/pyproject-starter/blob/master/.ai/code-guidance/AI_ASSISTANT-PYTHON-BASIC.md)


## Project Specific Directory Structure

Follow strict vertical slice architecture

assetutilities/
│
├── .ai/                            # AI assistant configuration
│   ├── commands/                   # Custom automation commands
│   │   ├── generate-spec.md        # Specification generation logic
│   │   └── execute-spec.md         # Specification execution logic
│   │── settings.json              # AI assistant permissions and preferences
│   └── AI_GUIDELINES.md            # Global AI assistant rules │
├── docs/                           # Documentation and reference materials
│   ├── chat-history/              # AI conversation logs
│   │   ├── README.md              # Session index
│   │   └── YYYY-MM-DD_topic.md    # Timestamped sessions
│   ├── modules/
│   └── workflows/                 # Development workflows

│
├── specs/                          # Project Specification Documents
│   ├── templates/                  # Reusable specification templates
│   │   └── spec_base.md           # Base template structure
│   └── [feature-name].md          # Generated specifications
│
├── src/
│   └── assetutilities/              # Main source code
│   └── base_configs
│       └── modules/
│   └── modules/
│
├── tests/                          # Test scripts and modules
│   ├── __init__.py
│   └── modules/
│
└── .github/                        # GitHub workflows
