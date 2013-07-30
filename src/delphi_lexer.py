#!/usr/bin/python3
# -*- coding: utf-8 -*-


import collections
import re


keywords = ['ABSOLUTE', 
            'ABSTRACT',
            'AND',
            'ARRAY',
            'AS',
            'ASM',
            'ASSEMBLER',
            'ASSEMBLY',
            'AT',
            'BEGIN',
            'CASE',
            'CDECL',
            'CLASS',
            'CONST',
            'CONSTRUCTOR',
            'CONTAINS',
            'DEFAULT',
            'DESTRUCTOR',
            'DISPID',
            'DISPINTERFACE',
            'DIV',
            'DO',
            'DOWNTO',
            'DYNAMIC',
            'ELSE',
            'END',
            'EXCEPT',
            'EXPORT',
            'EXPORTS',
            'EXTERNAL',
            'FAR',
            'FILE',
            'FINAL',
            'FINALIZATION',
            'FINALLY',
            'FOR',
            'FORWARD',
            'FUNCTION',
            'GOTO',
            'HELPER',
            'IF',
            'IMPLEMENTATION',
            'IMPLEMENTS',
            'IN',
            'INDEX',
            'INHERITED',
            'INITIALIZATION',
            'INLINE',
            'INTERFACE',
            'IS',
            'LABEL',
            'LIBRARY',
            'LOCAL',
            'MESSAGE',
            'MOD',
            'NAME',
            'NEAR',
            'NIL',
            'NODEFAULT',
            'NOT',
            'OBJECT',
            'OF',
            'ON',
            'OPERATOR',
            'OR',
            'OUT',
            'OVERLOAD',
            'OVERRIDE',
            'PACKAGE',
            'PACKED',
            'PASCAL',
            'PRIVATE',
            'PROCEDURE',
            'PROGRAM',
            'PROPERTY',
            'PROTECTED',
            'PUBLIC',
            'PUBLISHED',
            'RAISE',
            'READ',
            'READONLY',
            'RECORD',
            'REGISTER',
            'REINTRODUCE',
            'REPEAT',
            'REQUIRES',
            'RESOURCESTRING',
            'SAFECALL',
            'SEALED',
            'SET',
            'SHL',
            'SHR',
            'STATIC',
            'STDCALL',
            'STORED',
            'STRICT',
            'STRING',
            'THEN',
            'THREADVAR',
            'TO',
            'TRY',
            'TYPE',
            'UNIT',
            'UNTIL',
            'USES',
            'VAR',
            'VARARGS',
            'VIRTUAL',
            'WHILE',
            'WITH',
            'WRITE',
            'WRITEONLY',
            'XOR'
            ]

token_patterns = [
    ('COMMENT',                      r'(//[^\n]*)|(\{.*?\})|(\(\*.*?\*\))'),
    ('SYMBOL_AMPERSAND',             r'&'),
    ('SYMBOL_LEFT_PAREN',            r'\('),
    ('SYMBOL_RIGHT_PAREN',           r'\)'),
    ('SYMBOL_ASTERISK',              r'\*'),
    ('SYMBOL_PLUS',                  r'\+'),
    ('SYMBOL_COMMA',                 r'\,'),
    ('SYMBOL_MINUS_SIGN',            r'\-'),
    ('SYMBOL_PERIOD',                r'\.'),
    ('SYMBOL_DOUBLE_PERIOD',         r'\.\.'),
    ('SYMBOL_SOLIDUS',               r'/'),
    ('SYMBOL_COLON',                 r':'),
    ('SYMBOL_ASSIGN',                r':\='),
    ('SYMBOL_SEMICOLON',             r';'),
    ('SYMBOL_LESS_THAN',             r'\<'),
    ('SYMBOL_LESS_OR_EQUAL_THAN',    r'\<\='),
    ('SYMBOL_NOT_EQUALS',            r'\<\>'),
    ('SYMBOL_EQUALS',                r'\='),
    ('SYMBOL_GREATER_THAN',          r'\>'),
    ('SYMBOL_GREATER_OR_EQUAL_THAN', r'\>\='),
    ('SYMBOL_AT',                    r'@'),
    ('SYMBOL_LEFT_BRACKET',          r'\['),
    ('SYMBOL_RIGHT_BRACKET',         r'\]'),
    ('SYMBOL_CIRCUMFLEX',            r'\^'),
    ('FLOAT',                        r'[-]?[0-9]*\.?[0-9]+(e[-+]?[0-9]+)?'),
    ('INTEGER',                      r'\-?[1-9]\d*'),
    ('KEYWORD',                      '(' + '\\b|\\b'.join(keywords) + ')'),
    ('STRING_LITERAL',               r'\'.*?\''),
    ('IDENTIFIER',                   r'\w(\w|_|\d)*'),
    ('NEWLINE',                      r'\n'),
    ('SKIP',                         r'[ \t]')
]

pattern = '|'.join('(?P<%s>%s)' % pair for pair in token_patterns)

Token = collections.namedtuple('Token', 'type val line column')


class DelphiLexer(object):

    def __init__(self, statement):
        self.statement = statement

    def get_token(self):
        line = 1
        column = line_start = 0
        next_token = re.compile(pattern, re.IGNORECASE|re.DOTALL).match
        token = next_token(self.statement)
        while token:
            token_type = token.lastgroup
            if token_type == 'NEWLINE':
                line_start = column
                line += 1
            elif token_type != 'SKIP':
                token_value = token.group(token_type)
                yield Token(token_type, token_value, line, token.start()-line_start)
            column = token.end()
            token = next_token(self.statement, column)
        if column != len(self.statement):
            raise RuntimeError('Unexpected character %r on line %d' % (self.statement[column], line))
