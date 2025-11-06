import {
    createConnection,
    TextDocuments,
    ProposedFeatures,
    InitializeParams,
    CompletionItem,
    CompletionItemKind,
    TextDocumentPositionParams,
    TextDocumentSyncKind,
    InitializeResult
} from 'vscode-languageserver/node';

import {
    TextDocument
} from 'vscode-languageserver-textdocument';

// Create a connection for the server
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// NLang keywords and built-in functions
const nlangKeywords = [
    'store', 'def', 'if', 'else', 'while', 'return', 'break', 'continue',
    'import', 'from', 'as', 'export', 'assign_main'
];

const nlangBuiltInFunctions = [
    'println', 'print', 'input', 'len', 'str', 'int', 'float', 'bool',
    'abs', 'max', 'min', 'pow'
];

const nlangTypes = [
    'Integer', 'Float', 'Boolean', 'String'
];

connection.onInitialize((params: InitializeParams) => {
    const result: InitializeResult = {
        capabilities: {
            textDocumentSync: TextDocumentSyncKind.Incremental,
            // Tell the client that the server supports code completion
            completionProvider: {
                resolveProvider: true,
                triggerCharacters: ['.', ' ', '(', ')', '{', '}', '[', ']', ',', ';', '=']
            }
        }
    };

    return result;
});

// This handler provides the initial list of completion items
connection.onCompletion(
    (_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
        const completions: CompletionItem[] = [];

        // Add keywords
        nlangKeywords.forEach(keyword => {
            completions.push({
                label: keyword,
                kind: CompletionItemKind.Keyword,
                detail: 'NLang keyword',
                documentation: `NLang language keyword: ${keyword}`
            });
        });

        // Add built-in functions
        nlangBuiltInFunctions.forEach(func => {
            completions.push({
                label: func,
                kind: CompletionItemKind.Function,
                detail: 'NLang built-in function',
                documentation: `NLang built-in function: ${func}()`
            });
        });

        // Add types
        nlangTypes.forEach(type => {
            completions.push({
                label: type,
                kind: CompletionItemKind.TypeParameter,
                detail: 'NLang data type',
                documentation: `NLang data type: ${type}`
            });
        });

        return completions;
    }
);

// This handler resolves additional information for the item selected in the completion list
connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
    // You can add more detailed documentation here based on the item
    if (item.kind === CompletionItemKind.Function) {
        item.documentation = {
            kind: 'markdown',
            value: `**${item.label}** - NLang built-in function\n\nUsage: \`${item.label}(...)\`\n\nThis is a built-in function provided by the NLang standard library.`
        };
    } else if (item.kind === CompletionItemKind.Keyword) {
        item.documentation = {
            kind: 'markdown',
            value: `**${item.label}** - NLang keyword\n\nThis keyword is part of the NLang programming language syntax.`
        };
    }
    return item;
});

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();