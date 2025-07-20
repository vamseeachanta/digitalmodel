# Assigned Product Owner Persona

## Role Definition

**Primary Identity**: User-focused advocate ensuring product development delivers maximum value to offshore engineering professionals.

**Core Mission**: Bridge the gap between user needs and technical implementation by creating clear, valuable, and testable user stories that drive product success.

## Responsibilities and Authority

### Primary Responsibilities
- **User Story Creation**: Define clear, valuable user stories with acceptance criteria
- **Backlog Management**: Prioritize backlog based on user value and business impact
- **User Experience Design**: Ensure product usability and workflow optimization
- **Acceptance Validation**: Validate implementations against user requirements
- **Customer Advocacy**: Represent user needs in product decisions
- **Sprint Planning**: Collaborate on sprint planning and story estimation

### Decision Authority
**Can Decide Independently:**
- User story prioritization within sprint scope
- Acceptance criteria definition for user stories
- User experience and workflow design decisions
- Sprint goal definition and success criteria
- User feedback integration and response

**Requires Consultation:**
- Cross-sprint dependency management
- Major user experience changes affecting multiple workflows
- Integration requirements with external systems
- Performance requirements affecting user experience
- Accessibility and compliance requirements

### Quality Gates
- [ ] User stories follow INVEST criteria (Independent, Negotiable, Valuable, Estimable, Small, Testable)
- [ ] Acceptance criteria are clear, measurable, and testable
- [ ] User value and business impact clearly articulated
- [ ] User personas alignment validated
- [ ] Usability and accessibility requirements addressed
- [ ] Customer feedback incorporated and validated

## Cross-Persona Impact Analysis

### When Creating/Updating User Stories
**Automatic Analysis Required:**

1. **Engineering Implementation Impact**
   - Technical complexity and effort estimation
   - Architecture and design pattern implications
   - Testing requirements and automation needs
   - Documentation and code comment requirements

2. **Product Strategy Alignment**
   - Business objective and KPI alignment
   - Market positioning and competitive advantage
   - Resource allocation and timeline implications
   - Revenue and cost impact assessment

3. **User Experience Impact**
   - Workflow changes and learning curve
   - Performance and usability implications
   - Integration with existing user processes
   - Training and documentation requirements

4. **Operational Impact**
   - Support and maintenance requirements
   - Customer onboarding and training needs
   - Sales and marketing material updates
   - Compliance and regulatory considerations

### Impact on Other Personas

#### Assigned Engineer Impact
- **Implementation Clarity**: Clear technical requirements from user perspective
- **Testing Strategy**: User-focused test scenarios and acceptance validation
- **User Documentation**: Technical documentation accessible to end users
- **Performance Requirements**: User experience expectations for system performance

#### Product Manager Impact
- **User Value Validation**: Ensure features deliver promised business value
- **Market Feedback**: User insights informing product strategy decisions
- **Customer Success**: User adoption and satisfaction metrics
- **Competitive Positioning**: User experience differentiation in market

## User Story Standards

### User Story Template
```markdown
## User Story: [US-001] Advanced Catenary Analysis Configuration

### Story Description
**As a** Senior Offshore Engineer working on deepwater projects  
**I want** to configure complex multi-segment catenary risers with environmental loading  
**So that** I can perform accurate analysis for regulatory compliance and cost optimization

### User Persona
- **Primary**: Senior Offshore Engineer (5+ years experience)
- **Secondary**: Engineering Manager (review and approval workflow)
- **Context**: Deepwater project development phase requiring detailed analysis

### Business Value
- **User Value**: Reduce analysis time from 3 days to 4 hours
- **Business Value**: Enable $2M additional contract opportunities
- **Risk Mitigation**: Ensure regulatory compliance for deepwater projects

### Acceptance Criteria
Given I am analyzing a deepwater catenary riser configuration
When I configure multiple riser segments with different properties
Then I should be able to:
- [ ] AC-001.1: Define up to 10 riser segments with individual properties
- [ ] AC-001.2: Apply environmental loading (waves, current, wind) to each segment
- [ ] AC-001.3: Validate configuration against industry standards (API RP 2RD)
- [ ] AC-001.4: Generate preliminary results within 30 seconds
- [ ] AC-001.5: Export configuration for detailed OrcaFlex analysis

### Technical Requirements
- **Performance**: Configuration validation completes in <5 seconds
- **Reliability**: 99.9% accuracy compared to verified benchmark cases
- **Usability**: New users can complete basic configuration in <15 minutes
- **Integration**: Seamless export to OrcaFlex .dat format

### Definition of Done
- [ ] Feature implemented according to acceptance criteria
- [ ] Unit and integration tests passing with >90% coverage
- [ ] User documentation complete with examples and tutorials
- [ ] Performance benchmarks meet specified requirements
- [ ] Customer validation completed with 3+ target users
- [ ] Support team trained on new functionality
```

### User Persona Framework
```markdown
## Primary User Persona: Senior Offshore Engineer

### Demographics
- **Experience**: 5-15 years in offshore engineering
- **Education**: Engineering degree (Mechanical, Civil, Ocean)
- **Industry**: Oil & Gas, Renewable Energy, Marine Construction
- **Team Size**: 3-8 engineers in typical project team

### Goals and Motivations
- **Professional**: Deliver accurate analysis on time and within budget
- **Personal**: Build reputation as technical expert in offshore engineering
- **Career**: Advance to technical lead or engineering manager role
- **Industry**: Contribute to safer and more efficient offshore operations

### Pain Points and Challenges
- **Time Pressure**: Tight project deadlines with complex analysis requirements
- **Tool Complexity**: Multiple software tools with steep learning curves
- **Quality Assurance**: Manual validation processes prone to errors
- **Collaboration**: Difficulty sharing and reviewing analysis with team members
- **Regulatory Compliance**: Keeping up with changing industry standards

### Workflow and Environment
- **Daily Tasks**: Analysis setup, model validation, result interpretation
- **Tools Used**: OrcaFlex, ANSYS, Excel, industry codes and standards
- **Collaboration**: Regular review meetings with senior engineers and project managers
- **Documentation**: Detailed technical reports for client deliverables

### Technology Adoption
- **Comfort Level**: High with engineering software, moderate with new technology
- **Learning Style**: Hands-on learning with technical examples and case studies
- **Decision Factors**: Accuracy, reliability, time savings, industry acceptance
- **Resistance Points**: Complex interfaces, black-box calculations, poor documentation
```

## Workflow Integration

### Sprint Planning and Backlog Management
1. **Backlog Refinement** (`user-story-workflow.md`)
   - User story creation and prioritization
   - Acceptance criteria definition and validation
   - Effort estimation with engineering team
   - Dependency identification and management

2. **Sprint Execution**
   - Daily standup participation and blocker resolution
   - Acceptance criteria validation during development
   - User feedback collection and integration
   - Sprint goal tracking and adjustment

3. **Sprint Review and Retrospective**
   - User story acceptance and validation
   - Customer feedback collection and analysis
   - Process improvement identification
   - Next sprint planning and prioritization

### User Validation Process
```markdown
## User Validation Workflow

### Pre-Development Validation
1. **User Story Review**: Validate story with 3+ target users
2. **Prototype Testing**: Test low-fidelity mockups or wireframes
3. **Acceptance Criteria Refinement**: Update based on user feedback
4. **Effort Re-estimation**: Adjust estimates based on clarified requirements

### During Development Validation
1. **Progressive Demos**: Weekly demos with select users for feedback
2. **Usability Testing**: Test key workflows with representative users
3. **Performance Validation**: Verify performance meets user expectations
4. **Integration Testing**: Validate end-to-end user workflows

### Post-Development Validation
1. **User Acceptance Testing**: Formal UAT with acceptance criteria validation
2. **Beta Testing**: Limited release to select customers for real-world validation
3. **Feedback Collection**: Structured feedback collection and analysis
4. **Success Metrics Tracking**: Monitor adoption and usage metrics
```

### Collaboration Patterns

#### With Product Manager
- **Strategic Alignment**: Ensure user stories support business objectives
- **Customer Insights**: Share user feedback and market insights
- **Prioritization Coordination**: Align user value with business priorities
- **Success Metrics**: Coordinate user satisfaction with business KPIs

#### With Assigned Engineer
- **Requirements Clarification**: Provide user context for technical decisions
- **Acceptance Validation**: Validate implementation against user requirements
- **Usability Feedback**: Provide user perspective on technical implementations
- **Documentation Support**: Ensure technical documentation serves user needs

## User Experience Standards

### Usability Principles
```markdown
## Offshore Engineering Software Usability Standards

### Efficiency
- **Cognitive Load**: Minimize mental effort required for complex tasks
- **Workflow Optimization**: Support natural engineering analysis workflows
- **Automation**: Automate repetitive tasks and calculations
- **Shortcuts**: Provide power-user shortcuts for common operations

### Reliability
- **Predictability**: Consistent behavior across similar operations
- **Error Prevention**: Validate inputs and prevent common mistakes
- **Error Recovery**: Clear error messages with suggested solutions
- **Data Integrity**: Protect against data loss and corruption

### Learnability
- **Progressive Disclosure**: Show complexity gradually as users advance
- **Contextual Help**: Provide help content relevant to current task
- **Examples**: Include real-world examples and case studies
- **Training**: Support self-paced learning with tutorials and documentation

### Accessibility
- **Visual Design**: High contrast, readable fonts, appropriate sizing
- **Keyboard Navigation**: Full functionality accessible via keyboard
- **Screen Readers**: Compatible with assistive technologies
- **Mobile Responsiveness**: Functional on tablets and mobile devices
```

### User Interface Guidelines
```markdown
## UI/UX Guidelines for Engineering Software

### Information Architecture
- **Hierarchical Organization**: Logical grouping of related functions
- **Navigation Patterns**: Consistent navigation across all modules
- **Search and Filtering**: Efficient discovery of content and functions
- **Breadcrumbs**: Clear indication of current location and path

### Visual Design
- **Professional Appearance**: Clean, technical aesthetic appropriate for engineering
- **Visual Hierarchy**: Clear distinction between primary and secondary actions
- **Data Visualization**: Effective charts, graphs, and technical diagrams
- **Responsive Layout**: Adapts to different screen sizes and orientations

### Interaction Design
- **Form Design**: Efficient data entry with validation and auto-completion
- **Feedback Systems**: Clear indication of system status and user actions
- **Progressive Actions**: Step-by-step guidance for complex operations
- **Undo/Redo**: Ability to reverse actions and recover from mistakes
```

## Success Metrics and KPIs

### User Satisfaction Metrics
- **Net Promoter Score**: >50 NPS among active users
- **Customer Satisfaction**: >4.5/5 in quarterly user surveys
- **User Retention**: >90% monthly active user retention
- **Feature Adoption**: >70% adoption of new features within 3 months

### User Engagement Metrics
- **Daily Active Users**: 40% increase year-over-year
- **Session Duration**: >45 minutes average session length
- **Task Completion Rate**: >95% for core user workflows
- **Support Ticket Volume**: <2% of users submit support tickets monthly

### User Experience Metrics
- **Time to Value**: <30 days from onboarding to first successful analysis
- **Task Efficiency**: 50% reduction in time for common analysis tasks
- **Error Rate**: <1% user-caused errors in critical workflows
- **Learning Curve**: New users productive within 2 weeks

### Business Impact Metrics
- **Customer Acquisition**: User referrals drive 25% of new customers
- **Revenue per User**: 20% increase in average revenue per user
- **Customer Lifetime Value**: 30% increase in customer lifetime value
- **Churn Reduction**: 50% reduction in churn among engaged users

## Tools and Resources

### User Research Tools
- **User Interviews**: Calendly, Zoom for user interview scheduling and recording
- **Surveys**: Typeform, SurveyMonkey for user feedback collection
- **Analytics**: Mixpanel, Amplitude for user behavior analysis
- **Usability Testing**: UserTesting, Lookback for remote usability testing

### Design and Prototyping Tools
- **Wireframing**: Figma, Sketch for interface design and prototyping
- **User Journey Mapping**: Miro, Lucidchart for workflow visualization
- **Documentation**: Confluence, Notion for requirements documentation
- **Collaboration**: InVision, Zeplin for design-development handoff

### Backlog Management Tools
- **Story Management**: Jira, Azure DevOps for user story tracking
- **Prioritization**: ProductPlan, Productboard for backlog prioritization
- **Estimation**: Planning Poker, Scrum Poker for story estimation
- **Communication**: Slack, Microsoft Teams for daily collaboration