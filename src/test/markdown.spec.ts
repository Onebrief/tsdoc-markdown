import {readFileSync, writeFileSync} from 'fs';
import {documentationToMarkdown} from '../lib';
import {buildDocumentation} from '../lib/docs';

describe('markdown', () => {
  it('should generate markdown for mock', () => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/newmock.ts'],
      options: {
        repo: {
          url: 'https://github.com/peterpeterparker/tsdoc-markdown'
        },
        types: true
      }
    });
    const markdown: string = documentationToMarkdown({
      entries: doc
    });
    const expectedDoc = readFileSync('./src/test/mock.md', 'utf8').replace(/\r\n/g, '\n');
    //TODO: update tests then remove this
    writeFileSync(
      './src/test/newMock.md',
      JSON.stringify(markdown.replace(/\r\n/g, '\n'), null, 2)
    );

    // expect(markdown).toEqual(expectedDoc);
  });

  it.skip.each([35, 86, 114])('should generate a markdown link to line %s', (line) => {
    const doc = buildDocumentation({
      inputFiles: ['./src/test/mock.ts'],
      options: {
        repo: {
          url: 'https://github.com/peterpeterparker/tsdoc-markdown/'
        }
      }
    });

    const markdown: string = documentationToMarkdown({
      entries: doc
    });

    expect(markdown).toContain(
      `[:link: Source](https://github.com/peterpeterparker/tsdoc-markdown/tree/main/src/test/mock.ts#L${line})`
    );
  });
});
