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

Because `$`, `{`, and `}` all have meaning in the message format, each has an
escape sequence:

| You write | You get | Notes |
| --------- | ------- | ----- |
| `$$`      | `$`     | Valid anywhere. |
| `{{`      | `{`     | Valid anywhere. |
| `}}`      | `}`     | Only outside of a calculation block. |

A single `}` inside a calculation block always closes that block, so the `}}`
escape is not available there; if it were, the `}}}` that ends a message like
`{pluralize ($n) { default { none } }}` would be ambiguous.

A message must parse completely.
A stray `$`, `{`, or `}` is an error, not a way to cut the message short.

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
It may appear in any position among the clauses; it does not have to be last.
If it is given more than once, the first one wins.
If the passed-in parameter is a non-numeric argument, the `default` clause will always be used.
Any clause name other than `one`, `many`, or `default` is a parse error.
