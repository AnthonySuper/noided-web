# Noided-Translate

I18n for Haskell, supporting a custom DSL.
Inspired by Rails i18n, but a bit more complex.

## Translations Format

Translations are defined with YAML or JSON files.
The top-level keys of the file are the *locales*.
The nested keys are the *message keys*.
So, a YAML like this:

```yaml
en:
  page:
    person:
      create:
        title: "Create Person"
      edit:
        title: "Edit Person"
```

Defines the following keys in the `en` locale:

- `page.person.create.title`
- `page.person.edit.title`

## Message Format

Translations use a custom message format, inspired by Unicode's MessageFormat 2.

### Interpolation

You can interpolate values using a `$`.
These will be rendered based on some sensible defaults.
To escape a literal `$`, just type `$$`.

### Calculations

Calculations allow you to perform conditional formatting.
They are wrapped in curly braces `{}`.
Currently, only the `pluralize` helper is supported.
Calculations have syntax like:

```
{ [calculation-name] ( [calcualtion-arg [, calculation-arg]] ) [{ [calculation-matcher, [calculation-matcher]] }]
```

Or, in words:

- An opening curly brace
- A calculation name
- An opening paren
- Zero or more calculation arguments (which are generally going to be variable names)
- A closing paren
- An optional matcher clause, which is:
   - An opening paren
   - A series of one or more internal matching clauses
   - A closing paren
- A closing paren, to end the calculation block

Calculation blocks *may* be nested.

#### Pluralize

The pluralize calculation allows you to match on different forms, like so:

```
{pluralize ($itemCount) {
  one { $itemCount item }
  many { $itemCount items }
  default { $itemCount items }
}}
```

The `default` clause is mandatory, but you may skip the `one` or `many` clauses if you like.
If the passed-in parameter is a non-numeric argument, the `default` clause will always be used.
