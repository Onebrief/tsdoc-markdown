# tsdoc-to-markdown

Generates markdown API documentation from TypeScript source code. Useful for generating docs from code and injecting it into project README files.

> ⚠️ DISCLAIMER ⚠️
> This library is fully functional but, not yet published to NPM.

## Installation

Install the library in your project from [npm](https://www.npmjs.com/package/tsdoc-to-markdown):

```bash
npm install tsdoc-to-markdown -D
```

## Usage

This tool is shipped with a NodeJS [bin](/bin/index.js) script that can be executed with the shortcut `tsdoc`.

e.g. generating the documentation for a source file `./src/index.ts`:

```bash
tsdoc --src=src/index.ts
```

The `--src` parameter accepts a comma separated list of paths and wildcards as well.

e.g.

```bash
tsdoc --src=src/lib/*
tsdoc --src=src/lib/index.ts,src/lib/docs.ts
```

> Note: the library explicitly exports only the documentation of the pattern you provide. It does not follow the TypeScript tree.  

The Markdown documentation is parsed per default in a `./README.md` that finds place where you started the command line.
The output file will be over write unless you specify a `TSDOC_START` and `TSDOC_END` tag (as HTML comment). In such case, the documentation will be parsed within these two tags.

Specifying another output file is also supported with the parameter `--dest`.

Using above script is of course optional. You can also develop your own JavaScript script and use one of the following functions.

<!-- TSDOC_START -->

## :toolbox: Functions

- [buildDocumentation](#gear-builddocumentation)
- [documentationToMarkdown](#gear-documentationtomarkdown)
- [generateDocumentation](#gear-generatedocumentation)

### :gear: buildDocumentation

Build the documentation entries for the selected sources.

| Function | Type |
| ---------- | ---------- |
| `buildDocumentation` | `({ inputFiles, options }: { inputFiles: string[]; options?: CompilerOptions; }) => DocEntry[]` |

Parameters:

* `params.inputFiles`: The list of files to scan and for which the documentation should be build.
* `params.options`: Optional compiler options to generate the docs


### :gear: documentationToMarkdown

Convert the documentation entries to an opinionated Markdown format.

| Function | Type |
| ---------- | ---------- |
| `documentationToMarkdown` | `({ entries, options }: { entries: DocEntry[]; options?: MarkdownOptions; }) => string` |

Parameters:

* `params.entries`: The entries of the documentation (functions, constants and classes).
* `params.options`: Optional configuration to render the Markdown content. See `types.ts` for details.


### :gear: generateDocumentation

Generate documentation and write output to a file.
If the file exists, it will try to insert the docs between <!-- TSDOC_START --> and <!-- TSDOC_END --> comments.
If these does not exist, the output file will be overwritten.

| Function | Type |
| ---------- | ---------- |
| `generateDocumentation` | `({ inputFiles, outputFile, markdownOptions }: { inputFiles: string[]; outputFile: string; markdownOptions?: MarkdownOptions; }) => void` |

Parameters:

* `params.inputFiles`: The list of files to scan for documentation. Absolute or relative path.
* `params.outputFile`: The file to output the documentation in Markdown.
* `params.markdownOptions`: Optional settings passed to the Markdown parser. See `MarkdownOptions` for details.




<!-- TSDOC_END -->

## Useful Resources

- [Using the TypeScript Compiler API](https://github.com/microsoft/TypeScript/wiki/Using-the-Compiler-API)
- [TypeScript AST Viewer](https://ts-ast-viewer.com/#)
- List of [TypeScript node kind](https://github.com/microsoft/TypeScript/blob/main/lib/typescript.d.ts)

## License

MIT © [David Dal Busco](mailto:david.dalbusco@outlook.com)
