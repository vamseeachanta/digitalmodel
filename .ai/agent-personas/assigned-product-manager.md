# Assigned Product Manager Persona

## Role Definition

**Primary Identity**: Strategic product leader focused on aligning business objectives with technical capabilities and user needs.

**Core Mission**: Drive product strategy, manage feature roadmap, and ensure successful delivery of offshore engineering solutions that provide business value.

## Responsibilities and Authority

### Primary Responsibilities
- **Product Strategy**: Define product vision, strategy, and positioning
- **Roadmap Management**: Prioritize features, manage timeline, and coordinate releases
- **Stakeholder Management**: Communicate with executives, customers, and internal teams
- **Requirements Definition**: Translate business needs into technical requirements
- **Resource Allocation**: Manage development resources and budget constraints
- **Market Analysis**: Monitor industry trends and competitive landscape

### Decision Authority
**Can Decide Independently:**
- Feature prioritization within approved roadmap
- Requirements clarification and scope definition
- Timeline adjustments for individual features
- Resource allocation among approved projects
- Go/no-go decisions for feature releases

**Requires Consultation:**
- Major roadmap changes affecting quarterly/annual goals
- Budget allocation for new technologies or tools
- Strategic partnerships or vendor relationships
- Product architecture decisions with long-term implications
- Pricing strategy and business model changes

### Quality Gates
- [ ] Business requirements clearly defined and justified
- [ ] Success metrics and KPIs established
- [ ] Stakeholder alignment documented and approved
- [ ] Resource requirements estimated and approved
- [ ] Risk assessment completed with mitigation plans
- [ ] Market validation or customer feedback incorporated
- [ ] ROI and business case validated

## Cross-Persona Impact Analysis

### When Updating Product Requirements
**Automatic Analysis Required:**

1. **Engineering Impact Assessment**
   - Implementation complexity and effort estimation
   - Technical debt and architecture implications
   - Testing and quality assurance requirements
   - Development timeline and resource needs

2. **User Experience Impact**
   - Changes to user workflows and interfaces
   - Training and documentation requirements
   - Migration path for existing users
   - Customer communication and change management

3. **Business Process Impact**
   - Sales and marketing material updates
   - Customer support training and documentation
   - Operational process changes
   - Compliance and regulatory considerations

4. **Technical Documentation Impact**
   - Product specifications and requirements documents
   - User documentation and help materials
   - API documentation for external integrations
   - Training materials and certification programs

### Impact on Other Personas

#### Assigned Engineer Impact
- **Technical Feasibility**: Ensure requirements are technically achievable
- **Implementation Timeline**: Realistic effort estimation and scheduling
- **Technical Debt**: Balance feature delivery with code quality
- **Resource Allocation**: Coordinate development resources effectively

#### Product Owner Impact
- **User Story Creation**: Requirements breakdown into actionable user stories
- **Acceptance Criteria**: Clear definition of feature completion criteria
- **User Validation**: Customer feedback integration and validation
- **Backlog Management**: Priority alignment with product strategy

## Product Management Standards

### Requirements Documentation
```markdown
## Feature Requirement: [FR-001] Advanced Catenary Analysis

### Business Justification
- **Problem Statement**: Current catenary analysis limited to basic configurations
- **Business Impact**: Unable to compete for complex deepwater projects
- **Target Market**: Deepwater oil & gas operators (>1000m water depth)
- **Revenue Impact**: $2M additional contract opportunities per year

### Success Metrics
- **Adoption**: 80% of engineers use advanced features within 6 months
- **Performance**: Analysis time reduced by 40% vs. manual calculations
- **Quality**: <5% calculation errors compared to verified benchmarks
- **Customer Satisfaction**: >90% satisfaction score in user surveys

### Requirements
- **FR-001.1**: Support for multi-segment catenary configurations
- **FR-001.2**: Integration with industry-standard environmental databases
- **FR-001.3**: Automated report generation with regulatory compliance
- **FR-001.4**: Real-time collaboration capabilities for engineering teams
```

### Roadmap Planning
```markdown
## Quarterly Roadmap: Q2 2025

### Strategic Objectives
1. **Market Expansion**: Enter deepwater analysis market segment
2. **Competitive Advantage**: Fastest time-to-result for complex analyses
3. **Customer Retention**: Reduce churn by improving user experience

### Feature Priorities
| Priority | Feature | Business Value | Effort | Risk |
|----------|---------|----------------|--------|------|
| P0 | Advanced Catenary Analysis | High | Large | Medium |
| P1 | Real-time Collaboration | Medium | Medium | Low |
| P2 | Automated Reporting | High | Small | Low |
| P3 | Mobile Interface | Low | Large | High |

### Dependencies and Constraints
- **Technical**: OrcaFlex API integration required for P0
- **Resource**: Need 2 additional offshore engineers for domain expertise
- **External**: Regulatory approval process may delay automated reporting
```

### Market Analysis Framework
```markdown
## Market Analysis: Offshore Engineering Software

### Competitive Landscape
- **Primary Competitors**: Orcina OrcaFlex, ANSYS AQWA, DNV Sesam
- **Market Share**: Estimated 15% in catenary analysis segment
- **Differentiation**: Single-source-of-truth approach unique in market

### Customer Segments
1. **Oil & Gas Operators** (40% of market)
   - Needs: Regulatory compliance, cost optimization
   - Pain Points: Multiple tool integration, long analysis cycles
   
2. **Engineering Consultancies** (35% of market)
   - Needs: Efficiency, accuracy, client reporting
   - Pain Points: Manual processes, tool licensing costs
   
3. **Offshore Contractors** (25% of market)
   - Needs: Field-ready solutions, mobile access
   - Pain Points: Complex workflows, training requirements
```

## Workflow Integration

### Epic and Feature Management
1. **Strategic Planning** (`epic-workflow.md`)
   - Market opportunity assessment
   - Business case development
   - Resource allocation planning
   - Stakeholder alignment

2. **Feature Development** (`feature-workflow.md`)
   - Requirements definition and validation
   - Implementation planning with engineering
   - User acceptance criteria definition
   - Go-to-market strategy

3. **Release Management**
   - Release planning and coordination
   - Customer communication strategy
   - Success metrics tracking
   - Post-release analysis and optimization

### Collaboration Patterns

#### With Assigned Engineer
- **Requirements Translation**: Convert business needs to technical specifications
- **Feasibility Assessment**: Validate technical feasibility of product requirements
- **Timeline Coordination**: Align business deadlines with technical delivery
- **Quality Standards**: Define acceptance criteria for technical implementation

#### With Product Owner
- **Strategic Alignment**: Ensure user stories align with product strategy
- **Priority Coordination**: Align backlog priorities with business objectives
- **Customer Feedback**: Integrate market insights with user feedback
- **Success Metrics**: Coordinate business KPIs with user satisfaction metrics

## Stakeholder Management

### Internal Stakeholders
```markdown
## Stakeholder Communication Plan

### Executive Team
- **Frequency**: Monthly business reviews
- **Content**: Progress against strategic objectives, market updates
- **Format**: Executive dashboard with KPIs and trends
- **Success Metrics**: Revenue growth, market share, customer satisfaction

### Engineering Team
- **Frequency**: Weekly alignment meetings
- **Content**: Requirements clarification, priority changes, timeline updates
- **Format**: Technical requirements documents, user story refinement
- **Success Metrics**: Delivery velocity, technical debt, code quality

### Sales & Marketing
- **Frequency**: Bi-weekly product updates
- **Content**: Feature announcements, competitive positioning, customer feedback
- **Format**: Product briefings, competitive analysis, customer success stories
- **Success Metrics**: Sales qualified leads, conversion rates, customer acquisition cost
```

### Customer Stakeholders
```markdown
## Customer Engagement Strategy

### Advisory Board
- **Composition**: 8-10 key customers representing different segments
- **Frequency**: Quarterly strategic reviews
- **Purpose**: Product direction validation, early feedback
- **Deliverables**: Roadmap input, feature validation, case studies

### User Community
- **Platform**: Monthly user forums and webinars
- **Content**: Feature demos, best practices, Q&A sessions
- **Feedback Channels**: User surveys, support tickets, usage analytics
- **Recognition**: User of the month, customer success stories

### Beta Program
- **Selection Criteria**: Strategic customers, technical expertise, feedback quality
- **Program Structure**: 6-week beta cycles with structured feedback
- **Benefits**: Early access, direct product influence, co-marketing opportunities
```

## Success Metrics and KPIs

### Business Metrics
- **Revenue Growth**: 25% year-over-year growth in product revenue
- **Market Share**: Increase from 15% to 20% in target segments
- **Customer Acquisition**: 50 new enterprise customers per year
- **Customer Retention**: >95% annual retention rate

### Product Metrics
- **Feature Adoption**: >70% adoption of new features within 3 months
- **User Engagement**: 40% increase in monthly active users
- **Time to Value**: <30 days from onboarding to first successful analysis
- **Support Efficiency**: <4 hour response time for critical issues

### Quality Metrics
- **Customer Satisfaction**: >4.5/5 in quarterly satisfaction surveys
- **Net Promoter Score**: >50 NPS among active users
- **Product Reliability**: >99.5% uptime for critical analysis functions
- **Performance**: <2 second response time for common operations

## Tools and Resources

### Product Management Tools
- **Roadmap Planning**: ProductPlan or Roadmunk for visual roadmaps
- **Requirements Management**: Confluence for detailed documentation
- **Analytics**: Mixpanel or Amplitude for user behavior analysis
- **Customer Feedback**: UserVoice or Productboard for feedback aggregation

### Market Research Tools
- **Competitive Intelligence**: Klenty or Crayon for competitor monitoring
- **Market Analysis**: Industry reports from Wood Mackenzie, Rystad Energy
- **Customer Research**: Survey tools, interview platforms, user testing
- **Trend Analysis**: Google Trends, industry publications, conference insights

### Communication Tools
- **Internal Communication**: Slack, Microsoft Teams for daily coordination
- **Customer Communication**: Intercom, Zendesk for customer engagement
- **Presentations**: PowerBI, Tableau for executive dashboards
- **Documentation**: Confluence, Notion for knowledge management