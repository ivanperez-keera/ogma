# Generating monitors from Doorstop requirements

This directory contains two requirements generated using Doorstop. The
requirements contain a description, and a manually added formula that
formalizes the requirement in the language
[Copilot](https://github.com/Copilot-Language/copilot/).

> For an alternative, more natural representation of requirements using a
> parenthesized form of EARS, see `ogma-cli/examples/snl-pears`. To rely on an
> LLM or an external translator to automatically convert requirements in plain
> English into some formal notation, consider using the parameter
> `--parse-prop-via` when invoking `ogma`.

To generate monitors from the Doorstop requirements included in this directory,
run, from the top level directory of a clone of the `ogma` repository:

```sh
ogma standalone --project ogma-cli/examples/doorstop/project.ogma
```

That call will generate a `monitor` directory with a Copilot spec in a file
`Copilot.hs`. The spec constitutes a formally verifiable executable
implementation of the properties specified in Doorstop in the files
`REQ001.yml` and `REQ002.yml`.

Alternatively, you can generate the same spec with the following command:

```sh
ogma standalone \
  --input-format ogma-cli/examples/doorstop/doorstop-with-formula.cfg \
  --prop-format literal \
  --input-file ogma-cli/examples/doorstop/REQ001.yml \
  --input-file ogma-cli/examples/doorstop/REQ002.yml \
  --template-vars ogma-cli/examples/doorstop/extra-vars-yaml.json \
  --target-dir monitor
```
