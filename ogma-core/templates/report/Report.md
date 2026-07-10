{{#commandDiagramsAny}}
# Diagrams

{{#commandDiagramsList}}
## Diagram

The diagram `{{summaryDiagramFile}}`:

 - Has {{summaryDiagramNumStates}} states.
{{#summaryDiagramDeterministic}}
 - Is deterministic.
{{/summaryDiagramDeterministic}}
{{^summaryDiagramDeterministic}}
 - Is not deterministic.
{{/summaryDiagramDeterministic}}

{{/commandDiagramsList}}
{{/commandDiagramsAny}}
{{#commandRequirementsAny}}
# Requirements

{{#commandRequirementList}}
## File `{{summaryRequirementsFile}}`

The file `{{summaryRequirementsFile}}` has {{summaryRequirements}} requirements
in total.

Of these requirements:

- {{summaryRequirementsTrue}} requirements are constantly or always true.

- {{summaryRequirementsFalse}} requirements are constantly or always false.

{{#summaryRequirementsConsistent}}
No inconsistencies detected in the requirements.
{{/summaryRequirementsConsistent}}
{{^summaryRequirementsConsistent}}
The requirements are not mutually consistent: there is no way for all
requirements to be true at the same time.
{{/summaryRequirementsConsistent}}

The requirements mention:

- {{summaryExternalVariables}} external variables.

- {{summaryInternalVariables}} internal variables.

## Detailed list

{{#summaryRequirementDetails}}
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

{{/summaryRequirementDetails}}
{{/commandRequirementList}}
{{/commandRequirementsAny}}
