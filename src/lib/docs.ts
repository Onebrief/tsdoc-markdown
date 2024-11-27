import hostedGitInfo from 'hosted-git-info';
import {relative, resolve} from 'path/posix';
import type {
  ArrowFunction,
  CompilerOptions,
  Declaration,
  EnumDeclaration,
  FunctionDeclaration,
  NamedDeclaration,
  Node,
  PropertyName,
  Signature,
  SourceFile,
  TypeChecker,
  Symbol as TypeScriptSymbol,
  VariableDeclaration,
  VariableStatement
} from 'typescript';
import {
  createProgram,
  displayPartsToString,
  forEachChild,
  getCombinedModifierFlags,
  isArrowFunction,
  isClassDeclaration,
  isEnumDeclaration,
  isExportDeclaration,
  isFunctionDeclaration,
  isIdentifier,
  isInterfaceDeclaration,
  isMethodDeclaration,
  isModuleDeclaration,
  isNamedExports,
  isPropertySignature,
  isTypeAliasDeclaration,
  isVariableDeclaration,
  isVariableStatement,
  ModifierFlags,
  ModuleKind,
  NodeFlags,
  ScriptTarget,
  SyntaxKind
} from 'typescript';
import type {BuildOptions, DocEntry, DocEntryConstructor, DocEntryType, RepoOptions} from './types';

/** Serialize a symbol into a json object */
const serializeSymbol = ({
  checker,
  symbol,
  doc_type
}: {
  checker: TypeChecker;
  symbol: TypeScriptSymbol;
  doc_type?: DocEntryType;
}): DocEntry => {
  return {
    name: symbol.getName(),
    documentation: displayPartsToString(symbol.getDocumentationComment(checker)),
    type: checker.typeToString(checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration!)),
    jsDocs: symbol.getJsDocTags(),
    ...(doc_type && {doc_type})
  };
};

const serializeEnum = ({
  checker,
  symbol,
  doc_type
}: {
  checker: TypeChecker;
  symbol: TypeScriptSymbol;
  doc_type?: DocEntryType;
}): DocEntry => {
  const {members} = symbol.valueDeclaration as EnumDeclaration;

  const properties: DocEntry[] = members.map((member) => {
    const documentation = displayPartsToString(
      (member as unknown as {symbol: TypeScriptSymbol}).symbol.getDocumentationComment(checker)
    );

    const type = member.initializer?.getText();

    return {
      name: member.name.getText(),
      ...(type !== undefined && {type}),
      ...(documentation !== undefined && documentation !== '' && {documentation})
    };
  });

  return {
    name: symbol.getName(),
    documentation: displayPartsToString(symbol.getDocumentationComment(checker)),
    properties,
    jsDocs: symbol.getJsDocTags(),
    doc_type: doc_type ?? 'enum'
  };
};

/** Serialize a class symbol information */
const serializeClass = ({
  checker,
  symbol
}: {
  checker: TypeChecker;
  symbol: TypeScriptSymbol;
}): DocEntry => {
  const details = serializeSymbol({checker, symbol, doc_type: 'class'});

  // Get the construct signatures
  const constructorType = checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration!);

  details.constructors = constructorType
    .getConstructSignatures()
    .map((signature: Signature) => serializeSignature({checker, signature}))
    .filter(({documentation}) => documentation !== undefined && documentation !== '');

  return details;
};

/** gather all exported nodes */
function collectExportedNames(sourceFile: SourceFile): Set<string> {
  const exportedNames = new Set<string>();
  const declarationsByName = new Map<string, Node>();

  sourceFile.forEachChild(function visit(node) {
    if (isVariableStatement(node)) {
      const isExported = node.modifiers?.some((mod) => mod.kind === SyntaxKind.ExportKeyword);
      for (const decl of node.declarationList.declarations) {
        const nameNode = decl.name;
        if (isIdentifier(nameNode)) {
          const name = nameNode.text;
          declarationsByName.set(name, decl);
          if (isExported) {
            exportedNames.add(name);
          }
        }
      }
    } else if (
      isFunctionDeclaration(node) ||
      isClassDeclaration(node) ||
      isInterfaceDeclaration(node) ||
      isTypeAliasDeclaration(node) ||
      isEnumDeclaration(node)
    ) {
      const nameNode = node.name;
      if (nameNode && isIdentifier(nameNode)) {
        const name = nameNode.text;
        declarationsByName.set(name, node);

        if (node.modifiers?.some((mod) => mod.kind === SyntaxKind.ExportKeyword)) {
          exportedNames.add(name);
        }
      }
    } else if (isExportDeclaration(node)) {
      if (node.exportClause && isNamedExports(node.exportClause)) {
        for (const specifier of node.exportClause.elements) {
          const name = (specifier.propertyName || specifier.name).text;
          exportedNames.add(name);
        }
      }
    }

    node.forEachChild(visit);
  });

  return exportedNames;
}

function isNodeExportedOrPublic(node: Node, exportedNames: Set<string>): boolean {
  let name: string | undefined;
  if (isVariableStatement(node)) {
    // Check if the VariableStatement itself is exported
    const isExportedStatement = node.modifiers?.some(
      (mod) => mod.kind === SyntaxKind.ExportKeyword
    );
    if (isExportedStatement) {
      // Assume all declarations in this statement are exported
      return true;
    }
    // Otherwise, check each declaration
    for (const declaration of node.declarationList.declarations) {
      if (isIdentifier(declaration.name) && exportedNames.has(declaration.name.text)) {
        return true;
      }
    }
  }
  if (
    isVariableDeclaration(node) ||
    isFunctionDeclaration(node) ||
    isClassDeclaration(node) ||
    isInterfaceDeclaration(node) ||
    isTypeAliasDeclaration(node) ||
    isEnumDeclaration(node)
  ) {
    // Ensure the node is a NamedDeclaration
    const namedNode = node as NamedDeclaration;
    const nameNode = namedNode.name;

    if (nameNode && isIdentifier(nameNode)) {
      name = nameNode.text;
    }
  }

  let flags = getCombinedModifierFlags(node as Declaration);

  // For VariableDeclaration, modifiers are on the parent VariableStatement
  if (isVariableDeclaration(node)) {
    const variableStatement = node.parent.parent; // VariableDeclarationList -> VariableStatement
    const parentFlags = getCombinedModifierFlags(variableStatement as unknown as Declaration);
    flags |= parentFlags;
  }

  return (
    (flags & ModifierFlags.Export) !== 0 ||
    (flags & ModifierFlags.Public) !== 0 ||
    (isClassDeclaration(node.parent) &&
      [ModifierFlags.None, ModifierFlags.Static].includes(flags)) ||
    (!!name && exportedNames.has(name))
  );
}

/** Serialize a signature (call or construct) */
const serializeSignature = ({
  checker,
  signature
}: {
  checker: TypeChecker;
  signature: Signature;
}): DocEntryConstructor => {
  const result: Omit<DocEntryConstructor, 'visibility'> = {
    parameters: signature.parameters.map((symbol: TypeScriptSymbol) =>
      serializeSymbol({checker, symbol})
    ),
    returnType: checker.typeToString(signature.getReturnType()),
    documentation: displayPartsToString(signature.getDocumentationComment(checker))
  };

  if (!!signature.declaration && 'modifiers' in signature.declaration) {
    return {
      ...result,
      visibility:
        signature.declaration.modifiers?.[0].kind === SyntaxKind.PrivateKeyword
          ? 'private'
          : 'public'
    };
  }

  return {
    ...result,
    visibility: 'public'
  };
};

// https://stackoverflow.com/a/73338964/5404186
const findDescendantArrowFunction = (node: Node): Node | undefined => {
  if (isArrowFunction(node)) {
    return node;
  }

  return forEachChild(node, findDescendantArrowFunction);
};

// TODO: there is probably a better way
const isTypeKind = (kind: SyntaxKind): boolean => {
  const typeKinds: SyntaxKind[] = [
    SyntaxKind.TypePredicate,
    SyntaxKind.TypeReference,
    SyntaxKind.FunctionType,
    SyntaxKind.ConstructorType,
    SyntaxKind.TypeQuery,
    SyntaxKind.TypeLiteral,
    SyntaxKind.ArrayType,
    SyntaxKind.TupleType,
    SyntaxKind.OptionalType,
    SyntaxKind.RestType,
    SyntaxKind.UnionType,
    SyntaxKind.IntersectionType,
    SyntaxKind.ConditionalType,
    SyntaxKind.InferType,
    SyntaxKind.ParenthesizedType,
    SyntaxKind.ThisType,
    SyntaxKind.TypeOperator,
    SyntaxKind.IndexedAccessType,
    SyntaxKind.MappedType,
    SyntaxKind.LiteralType,
    SyntaxKind.NamedTupleMember,
    SyntaxKind.TemplateLiteralType,
    // SyntaxKind.TemplateLiteralTypeSpan, // This is more of a structural part of template literal types
    SyntaxKind.ImportType
  ];

  return typeKinds.includes(kind);
};

/** visit nodes finding exported classes */
const visit = ({
  checker,
  node,
  types,
  exportedNames,
  ...rest
}: {
  checker: TypeChecker;
  node: Node;
  exportedNames: Set<string>;
} & Source &
  Required<Pick<BuildOptions, 'types'>>): DocEntry[] => {
  // // Only consider exported nodes
  if (!isNodeExportedOrPublic(node, exportedNames)) {
    return [];
  }

  const entries: DocEntry[] = [];

  const addDocEntry = ({
    symbol,
    doc_type,
    node
  }: {
    symbol: TypeScriptSymbol | undefined;
    doc_type: DocEntryType;
    node: Node;
  }) => {
    if (!symbol) {
      return;
    }

    const details = serializeSymbol({checker, symbol, doc_type});
    entries.push({
      ...details,
      ...buildSource({
        node,
        ...rest
      })
    });
  };

  if (isClassDeclaration(node) && node.name) {
    // This is a top level class, get its symbol
    const symbol = checker.getSymbolAtLocation(node.name);

    if (symbol) {
      const classEntry: DocEntry = {
        ...serializeClass({checker, symbol}),
        methods: [],
        ...buildSource({
          node,
          ...rest
        })
      };

      const visitChild = (node: Node) => {
        const docEntries: DocEntry[] = visit({node, checker, types, exportedNames, ...rest});
        // We do not need to repeat the file name for class members
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        classEntry.methods?.push(...docEntries.map(({fileName: _, ...rest}) => rest));
      };

      forEachChild(node, visitChild);

      entries.push(classEntry);
    }
  } else if (isModuleDeclaration(node)) {
    const visitChild = (node: Node) => {
      const docEntries: DocEntry[] = visit({node, checker, types, exportedNames, ...rest});
      entries.push(...docEntries);
    };

    // This is a namespace, visit its children
    forEachChild(node, visitChild);
  } else if (isMethodDeclaration(node)) {
    const symbol = checker.getSymbolAtLocation(node.name);
    addDocEntry({symbol, doc_type: 'method', node});
  } else if (isFunctionDeclaration(node)) {
    const symbol = checker.getSymbolAtLocation((node as FunctionDeclaration).name ?? node);
    addDocEntry({symbol, doc_type: 'function', node});
  } else {
    const arrowFunc: Node | undefined = findDescendantArrowFunction(node);

    if (arrowFunc !== undefined) {
      const symbol = checker.getSymbolAtLocation(
        ((arrowFunc as ArrowFunction).parent as VariableDeclaration).name
      );
      addDocEntry({symbol, doc_type: 'function', node: arrowFunc});
    } else if (isVariableStatement(node)) {
      const {
        declarationList: {declarations, flags}
      } = node as VariableStatement;

      // https://stackoverflow.com/a/69801125/5404186
      const isConst = (flags & NodeFlags.Const) !== 0;

      if (isConst) {
        // TODO: not sure what's the proper casting, VariableDeclaration does not contain Symbol but the test entity effectively does
        const symbol = (declarations[0] as unknown as {symbol: TypeScriptSymbol}).symbol;
        addDocEntry({symbol, doc_type: 'const', node});
      }
    } else if (types && isInterfaceDeclaration(node)) {
      const symbol = checker.getSymbolAtLocation(node.name);

      if (symbol) {
        const members = node.members
          .filter(
            (member) =>
              isPropertySignature(member) && member.name !== undefined && isIdentifier(member.name)
          )
          .map((member) => checker.getSymbolAtLocation(member.name as PropertyName))
          .filter((symbol) => symbol !== undefined)
          .map((symbol) => serializeSymbol({checker, symbol: symbol as TypeScriptSymbol}));

        const interfaceEntry: DocEntry = {
          ...serializeSymbol({checker, doc_type: 'interface', symbol}),
          properties: members,
          ...buildSource({
            node,
            ...rest
          })
        };

        entries.push(interfaceEntry);
      }
    } else if (types && isTypeAliasDeclaration(node)) {
      const symbol = checker.getSymbolAtLocation(node.name);

      if (symbol) {
        const child = node.getChildren().find(({kind}) => isTypeKind(kind));

        const typeEntry: DocEntry = {
          ...serializeSymbol({checker, doc_type: 'type', symbol}),
          ...buildSource({
            node,
            ...rest
          }),
          type: child?.getText().replace(/^"|"$/g, '')
        };

        entries.push(typeEntry);
      }
    } else if (isEnumDeclaration(node)) {
      const symbol = checker.getSymbolAtLocation((node as EnumDeclaration).name)!;
      const details = serializeEnum({checker, symbol});
      entries.push({
        ...details,
        ...buildSource({
          node,
          ...rest
        })
      });
    }
  }

  return entries;
};

const DEFAULT_COMPILER_OPTIONS: CompilerOptions = {
  target: ScriptTarget.ES2020,
  module: ModuleKind.CommonJS,
  strictNullChecks: true
};

type Source = Pick<BuildOptions, 'repo'> & {sourceFile: SourceFile};

const buildSource = ({
  repo,
  node,
  sourceFile
}: Source & {node: Node}): Pick<DocEntry, 'url' | 'fileName'> => {
  const fileName = sourceFile.fileName;

  if (repo === undefined) return {fileName};

  const {line} = sourceFile.getLineAndCharacterOfPosition(node.getStart());
  const filePath = relative(process.cwd(), sourceFile.fileName);

  let url = '';

  if (typeof repo === 'string') {
    const hgi = hostedGitInfo.fromUrl(repo);

    switch (hgi?.type) {
      case 'github':
        url = generateGithubViewUrl(filePath, line, {url: hgi.browse()});
        break;
      case 'gitlab':
        url = generateGitlabViewUrl(filePath, line, {url: hgi.browse()});
        break;
      case 'bitbucket':
        url = generateBitbucketViewUrl(filePath, line, {url: hgi.browse()});
        break;
      case 'gist':
        // Not supported
        break;
    }
  }

  if (typeof repo === 'object') {
    const {url: repoUrl} = repo;

    const hgi = hostedGitInfo.fromUrl(repoUrl);

    switch (hgi?.type) {
      case 'github':
        url = generateGithubViewUrl(filePath, line, {...repo, url: hgi.browse()});
        break;
      case 'gitlab':
        url = generateGitlabViewUrl(filePath, line, {...repo, url: hgi.browse()});
        break;
      case 'bitbucket':
        url = generateBitbucketViewUrl(filePath, line, {...repo, url: hgi.browse()});
        break;
    }
  }

  if (url === '') return {fileName};

  return {
    fileName,
    url
  };
};

const generateGithubViewUrl = (filePath: string, line: number, options: RepoOptions): string => {
  const {url, directory, branch} = options;

  return `${url.replace(/\/+$/, '')}/tree/${branch ?? 'main'}/${directory ? directory.replace(/\/?$/, '/') : ''}${filePath.replace(
    /^\.\.\//,
    ''
  )}#L${line + 1}`;
};

const generateGitlabViewUrl = (filePath: string, line: number, options: RepoOptions): string => {
  const {url, directory, branch} = options;

  return `${url.replace(/\/+$/, '')}/-/blob/${branch ?? 'main'}/${directory ? directory.replace(/\/?$/, '/') : ''}${filePath.replace(
    /^\.\.\//,
    ''
  )}#L${line + 1}`;
};

const generateBitbucketViewUrl = (filePath: string, line: number, options: RepoOptions): string => {
  const {url, directory, branch} = options;

  return `${url.replace(/\/+$/, '')}/src/${branch ?? 'main'}/${directory ? directory.replace(/\/?$/, '/') : ''}${filePath.replace(
    /^\.\.\//,
    ''
  )}#lines-${line + 1}`;
};

/**
 * Build the documentation entries for the selected sources.
 *
 * @param {inputFiles: string[]; options?: CompilerOptions;} params
 * @param {string[]} params.inputFiles The list of files to scan and for which the documentation should be build.
 * @param {CompilerOptions} params.options Optional compiler options to generate the docs
 *
 * @returns An array of documentation entries
 */
export const buildDocumentation = ({
  inputFiles,
  options
}: {
  inputFiles: string[];
  options?: BuildOptions;
}): DocEntry[] => {
  const {
    compilerOptions,
    explore: userExplore,
    repo,
    types: userTypes
  } = options ?? {
    explore: false,
    compilerOptions: DEFAULT_COMPILER_OPTIONS,
    types: false
  };

  const explore = userExplore ?? false;
  const types = userTypes ?? false;

  // Build a program using the set of root file names in fileNames
  const program = createProgram(inputFiles, compilerOptions ?? DEFAULT_COMPILER_OPTIONS);

  const programSourceFiles = program.getSourceFiles();

  const filenamesFullPaths: string[] = inputFiles.map((fileName: string) => resolve(fileName));

  // Visit only the files specified by the developers - no deep visit
  const sourceFiles = programSourceFiles.filter(
    ({isDeclarationFile, fileName}) =>
      !isDeclarationFile && (explore || filenamesFullPaths.includes(resolve(fileName)))
  );

  // Get the checker, we will use it to find more about classes
  const checker = program.getTypeChecker();

  const result: DocEntry[] = [];

  // Visit every sourceFile in the program
  for (const sourceFile of sourceFiles) {
    // Collect exported nodes from the source file
    const exportedNames = collectExportedNames(sourceFile);

    // Walk the tree to search for classes
    forEachChild(sourceFile, (node: Node) => {
      const entries: DocEntry[] = visit({
        checker,
        node,
        sourceFile,
        repo,
        types,
        exportedNames
      });
      result.push(...entries);
    });
  }

  return result;
};
