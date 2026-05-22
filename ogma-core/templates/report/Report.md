{{#commandDiagramsAny}}
# Diagrams

{{#commandDiagramList}}
## Diagram

The diagram:
 - Has {{summaryDiagramNumStates}} states.
{{#summaryDiagramDeterministic}}
 - Is deterministic.
{{/summaryDiagramDeterministic}}
{{^summaryDiagramDeterministic}}
 - Is not deterministic.
{{/summaryDiagramDeterministic}}

{{/commandDiagramList}}
{{/commandDiagramsAny}}
{{#commandRequirementsAny}}
# Requirements

## Summary

The project has {{commandRequirements}} requirements in total.

Of these requirements:

- {{commandRequirementsTrue}} requirements are constantly or always true.

- {{commandRequirementsFalse}} requirements are constantly or always false.

{{#commandRequirementsConsistent}}
No inconsistencies detected in the requirements.
{{/commandRequirementsConsistent}}
{{^commandRequirementsConsistent}}
The requirements are not mutually consistent: there is no way for all
requirements to be true at the same time.
{{/commandRequirementsConsistent}}

The requirements mention:

- {{commandExternalVariables}} external variables.

- {{commandInternalVariables}} internal variables.

## Detailed list

{{#commandRequirementList}}
### {{summaryRequirementName}}

**Description:** {{summaryRequirementDesc}}

**Properties:**

{{#summaryRequirementTrue}}
- The requirement is always true or vacuously true.
{{/summaryRequirementTrue}}
{{#summaryRequirementFalse}}
- The requirement is always false or vacuously false.
{{/summaryRequirementFalse}}
{{^summaryRequirementTrue}}
{{^summaryRequirementFalse}}
- None.
{{/summaryRequirementFalse}}
{{/summaryRequirementTrue}}

{{/commandRequirementList}}
{{/commandRequirementsAny}}
