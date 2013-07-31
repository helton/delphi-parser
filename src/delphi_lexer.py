#!/usr/bin/python3
# -*- coding: utf-8 -*-

import delphi_token


class DelphiLexer(object):

    def __init__(self, source_code):
        self.source_code = source_code

    def tokenizer(self):
        line = 1
        column = line_start = 0
        next_token = delphi_token.compiled_pattern.match
        token = next_token(self.source_code)
        while token:
            token_type = delphi_token.get_token_by_name(token.lastgroup)
            if token_type == delphi_token.NEWLINE:
                line_start = column
                line += 1
            elif token_type != delphi_token.SKIP:
                token_value = token.group(token.lastgroup)
                yield delphi_token.Token(token_type, token_value, line, token.start()-line_start)
            column = token.end()
            token = next_token(self.source_code, column)
        if column != len(self.source_code):
            raise RuntimeError('Unexpected character %r on line %d' % (self.source_code[column], line))
