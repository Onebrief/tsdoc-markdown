import {readFileSync} from 'fs';
import {buildDocumentation} from '../lib/docs';

describe('docs', () => {
  it('should generate json for mock', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        types: true
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc).toEqual(JSON.parse(expectedDoc));
  });

  it('should generate json with links to source code', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          url: 'https://github.com/peterpeterparker/tsdoc-markdown/'
        }
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/main/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code with npm shorthand syntax', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: 'peterpeterparker/tsdoc-markdown'
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/main/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code with github shorthand syntax', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: 'github:peterpeterparker/tsdoc-markdown'
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/main/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code with gitlab shorthand syntax', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: 'gitlab:peterpeterparker/tsdoc-markdown'
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://gitlab.com/peterpeterparker/tsdoc-markdown/-/blob/main/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code with github repository https url', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          type: 'git',
          url: 'git+https://github.com/peterpeterparker/tsdoc-markdown.git',
          directory: 'packages/test',
          branch: 'testing'
        }
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/testing/packages/test/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code with github repository ssh url', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          type: 'git',
          url: 'git@github.com/peterpeterparker/tsdoc-markdown.git',
          directory: 'packages/test',
          branch: 'testing'
        }
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/testing/packages/test/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code in directory without trailing slash', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          url: 'https://github.com/peterpeterparker/tsdoc-markdown/',
          directory: 'packages/test'
        }
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/main/packages/test/src/test/mock.ts#L6'
    });
  });

  it('should generate json with links to source code in directory with trailing slash', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          url: 'https://github.com/peterpeterparker/tsdoc-markdown/',
          directory: 'packages/test/'
        }
      }
    });

    const expectedDoc = readFileSync('./src/test/mock.json', 'utf8');
    expect(doc[0]).toEqual({
      ...JSON.parse(expectedDoc)[0],
      url: 'https://github.com/peterpeterparker/tsdoc-markdown/tree/main/packages/test/src/test/mock.ts#L6'
    });
  });

  describe('bulk exports', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          url: 'https://github.com/peterpeterparker/tsdoc-markdown/'
        },
        types: true
      }
    });

    it.each([['hello'], ['numberOne'], ['Abc']])(
      'should include bulk exported item: %s',
      (name) => {
        const item = doc.find((item) => item.name === name);
        expect(item).toBeDefined();
      }
    );
  });
});
