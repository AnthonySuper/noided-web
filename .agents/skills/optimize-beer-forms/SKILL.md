---
name: optimize-beer-forms
description: Workflow and boilerplate for implementing HKD forms (Type, Validate, Render) in the optimize-beer project. Use when creating new forms, adding fields to existing forms, or integrating forms into Actions.
---

# `optimize-beer-forms` Skill

Provides procedural guidance for the multi-module form architecture in `optimize-beer`.

## Workflow

Follow these steps in order for every new form:

1.  **Define Type**: Create `lib/OptBeer/Form/Type/<Name>.hs`. Use `$(defineHKDForm ''<Name>F)` and implement `HKDForm` instances.
2.  **Implement Validation**: Create `lib/OptBeer/Form/Validate/<Name>.hs`. Use `validateSubform` and `validateInput`.
3.  **Create Renderer**: Create `lib/OptBeer/Form/Render/<Name>.hs`. Use `fieldWrapModelName` and `subformField`.
4.  **Add Translations**: Define keys in `config/translations/en/form.yaml`. Check global `form.attributes` first.
5.  **Verify**: Write a renderer spec in `test/OptBeer/Form/Render/<Name>Spec.hs` using `assertHasNoBadTranslations`.

## Resources

- **Templates**: See [references/templates.md](references/templates.md) for standard boilerplate for each module.
- **Action Integration**: See [references/integration.md](references/integration.md) for GET/POST handlers.

## Guidelines

- **Blank Forms**: Always use `hkdFormEmpty` for blank form initialization.
- **Database Access**: In validators, use `lift` to access the `TransactM` context (e.g., `lift $ queryMaybe ...`).
- **Error Types**: Use domain-specific error types (e.g., `ValueTaken`, `TooShort`) that map to translation keys.
- **Shadowing**: Avoid shadowing effects (`es`) or monad (`m`) type variables.
