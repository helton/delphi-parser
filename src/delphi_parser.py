#!/usr/bin/python3
# -*- coding: utf-8 -*-

import delphi_token as dt
from delphi_lexer import DelphiLexer


class DelphiParser(object):

    def __init__(self, source_code):
        self.lexer = DelphiLexer(source_code)

    def next_token(self):
        try:
            return next(self.tokenizer)
        except StopIteration:
            pass

    def token_type_is(self, *token_types):
        if self.token:
            return self.token.type in list(token_types)

    def token_value_is(self, *token_values):
        if self.token:
            return self.token.value.upper() in list(token_values)

    def match(self, token_type_expected):
        if not self.token_type_is(token_type_expected):
            raise ValueError('Syntax error: expected %s but got %s' % (token_type_expected), self.token.type)
        else:
            self.token = self.next_token()

    def report_error(self, error_message):
        raise ValueError(error_message)

    def report_syntax_error(self, *expected_values):
        raise ValueError('Line: %s, Column: %s - Expected %s, but "%s" found.' % (self.token.line, self.token.column, ''.join(expected_values), self.token.value))

    def parse(self):
        self.tokenizer = self.lexer.tokenizer()
        self.token = self.next_token()
        self.goal()
        if self.token:
            self.report_error('Source code not fully parsed!')

    # ------------------------------ Grammar Rules ---------------------------------- #

    def add_op(self):
        """
            AddOp:
              Doc: |
                ! -> '+'
                ! -> '-'
                ! -> OR
                ! -> XOR
        """
        pass

    def array_type(self):
        """
            ArrayType:
              Doc: |
                ! -> ARRAY ['[' (Type [','])+ ']'] OF Type
        """
        pass

    def assembler_statement(self):
        """
            AssemblerStatement:
              Doc: |
                ! -> ASM
                !    <assemblylanguage>
                !    END
        """
        pass

    def assembly_attribute(self):
        """
            AssemblyAttribute:
              Doc: |
                ! -> '[' ASSEMBLY ':' Expression ']'
        """
        pass

    def atom(self):
        """
            Atom:
              Doc: |
                ! -> Particle
                !    ( '.' ExtendedIdent
                !      | '[' ExpressionList ']'
                !      | '^'
                !      | '(' (ParameterExpression [','])* ')'
                !    )*
        """
        pass

    def bare_inherited(self):
        """
            BareInherited:
              Doc: |
                ! -> INHERITED
        """
        pass

    def block(self):
        """
            Block:
              Doc: |
                ! -> BEGIN [StatementList] END
                ! -> AssemblerStatement
        """
        pass

    def case_selector(self):
        """
            CaseSelector:
              Doc: |
                ! -> (ExpressionOrRange [','])+
                !    ':' [Statement] [';']
        """
        pass

    def case_statement(self):
        """
            CaseStatement:
              Doc: |
                ! -> CASE Expression OF
                !    (CaseSelector)+
                !    [ELSE [StatementList]]
                !    END
        """
        pass

    def class_helper_type(self):
        """
            ClassHelperType:
              Doc: |
                ! -> CLASS HELPER
                !    ['(' QualifiedIdent ')']
                !    FOR QualifiedIdent
                !    (VisibilitySection)*
                !    END
        """
        pass

    def class_of_type(self):
        """
            ClassOfType:
              Doc: |
                ! -> CLASS OF QualifiedIdent
        """
        pass

    def class_type(self):
        """
            ClassType:
              Doc: |
                ! -> CLASS
                !    [ABSTRACT | SEALED]
                !    ['(' (QualifiedIdent [','])+ ')']
                // The remainder is optional, but only if the base class is specified and lookahead shows that the next token is a semicolon
                !    (VisibilitySection)*
                !    END
        """
        pass

    def constante_decl(self):
        """
            ConstantDecl:
              Doc: |
                ! -> Ident
                !    [':' Type]
                !    '=' TypedConstant
                !    (PortabilityDirective)*
                !    ';'
        """
        pass

    def const_section(self):
        """
            ConstSection:
              Doc: |
                ! -> (CONST|RESOURCESTRING)
                !    (ConstantDecl)+
        """
        pass

    def directive(self):
        """
            Directive:
              Doc: |
                ! -> [';'] ABSTRACT
                ! -> [';'] ASSEMBLER
                ! -> [';'] CDECL
                ! -> [';'] DISPID Expression
                ! -> [';'] DYNAMIC
                ! -> [';'] EXPORT
                ! -> [';'] EXTERNAL [Expression (ExportsSpecifier)*]
                ! -> [';'] FAR
                ! -> [';'] FINAL
                ! -> [';'] FORWARD
                ! -> [';'] INLINE
                ! -> [';'] LOCAL
                ! -> [';'] MESSAGE Expression
                ! -> [';'] NEAR
                ! -> [';'] OVERLOAD
                ! -> [';'] OVERRIDE
                ! -> [';'] PASCAL
                ! -> [';'] REGISTER
                ! -> [';'] REINTRODUCE
                ! -> [';'] SAFECALL
                ! -> [';'] STATIC
                ! -> [';'] STDCALL
                ! -> [';'] VARARGS
                ! -> [';'] VIRTUAL
                ! -> [';'] PortabilityDirective
        """
        pass

    def enumerated_type(self):
        """
            EnumeratedType:
              Doc: |
                ! -> '(' (EnumeratedTypeElement [','])+ ')'
        """
        pass

    def enumerated_type_element(self):
        """
            EnumeratedTypeElement:
              Doc: |
                ! -> Ident [ '=' Expression ]
        """
        pass

    def exception_item(self):
        """
            ExceptionItem:
              Doc: |
                ! -> ON
                !    [Ident ':']
                !    QualifiedIdent DO
                !    [Statement]
                !    [';']
        """
        pass

    def exports_item(self):
        """
            ExportsItem:
              Doc: |
                ! -> Ident (ExportsSpecifier)*
        """
        pass

    def exports_specifier(self):
        """
            ExportsSpecifier:
              Doc: |
                ! -> (INDEX | NAME) Expression
        """
        pass

    def exports_statement(self):
        """
            ExportsStatement:
              Doc: |
                ! -> EXPORTS (ExportsItem [','])+ ';'
        """
        pass

    def expression(self):
        """
            Expression:
              Doc: |
                ! -> SimpleExpression (RelOp SimpleExpression)*
        """
        pass

    def expression_list(self):
        """
            ExpressionList:
              Doc: |
                ! -> (Expression [','])+
        """
        pass

    def expression_or_assignment(self):
        """
            ExpressionOrAssignment:
              Doc: |
                ! -> Expression
                ! -> Expression ':=' Expression
        """
        pass

    def expression_or_range(self):
        """
            ExpressionOrRange:
              Doc: |
                ! -> SimpleExpression ['..' SimpleExpression]
        """
        pass

    def expression_or_range_list(self):
        """
            ExpressionOrRangeList:
              Doc: |
                ! -> (ExpressionOrRange [','])+
        """
        pass

    def extended_ident(self):
        """
            ExtendedIdent:
              Doc: |
                ! -> Ident
                ! -> <keyword>
        """
        pass

    def factor(self):
        """
            Factor:
              Doc: |
                ! -> Atom
                ! -> UnaryOperator Factor
        """
        pass

    def fancy_block(self):
        """
            FancyBlock:
              Doc: |
                ! -> (ImplementationDecl)*
                !    Block
        """
        pass

    def field_decl(self):
        """
            FieldDecl:
              Doc: |
                ! -> IdentList ':' Type (PortabilityDirective)* [';']
        """
        pass

    def field_section(self):
        """
            FieldSection:
              Doc: |
                ! -> [[CLASS] VAR]
                !    (FieldDecl)*
        """
        pass

    def file_type(self):
        """
            FileType:
              Doc: |
                ! -> FILE
                ! -> FILE OF QualifiedIdent
        """
        pass

    def for_statement(self):
        """
            ForStatement:
              Doc: |
                ! -> FOR Ident ':=' Expression (TO | DOWNTO) Expression DO [Statement]
                ! -> FOR Ident IN Expression DO [Statement]
        """
        pass

    def goal(self):
        """
            Goal:
              Doc: |
                ! -> Program
                ! -> Package
                ! -> Unit
        """
        if self.token_type_is(dt.KEYWORD) and self.token_value_is('PROGRAM', 'LIBRARY', 'PACKAGE', 'UNIT'):
            if self.token_value_is('PROGRAM', 'LIBRARY'):
                self.program()
            elif self.token_value_is('PACKAGE'):
                self.package()
            else:
                self.unit()
        else:
            self.report_syntax_error('PROGRAM, PACKAGE or UNIT')

    def goto_statement(self):
        """
            GotoStatement:
              Doc: |
                ! -> GOTO LabelId
        """
        pass

    def ident(self):
        """
            Ident:
              Doc: |
                ! -> <identifier>
                ! -> <semikeyword>
                ! -> '&' <identifier>
                ! -> '&' <semikeyword>
                ! -> '&' <keyword>
        """
        pass

    def ident_list(self):
        """
            IdentList:
              Doc: |
                ! -> (Ident [','])+
        """
        pass

    def if_statement(self):
        """
            IfStatement:
              Doc: |
                ! -> IF Expression THEN [Statement]
                !    [ELSE [Statement]]
        """
        pass

    def implementation_decl(self):
        """
            ImplementationDecl:
              Doc: |
                ! -> LabelDeclSection
                ! -> ConstSection
                ! -> TypeSection
                ! -> VarSection
                ! -> MethodImplementation
                ! -> ExportsStatement
                ! -> AssemblyAttribute
        """
        pass

    def implementation_section(self):
        """
            ImplementationSection:
              Doc: |
                ! -> IMPLEMENTATION
                !    [UsesClause]
                !    (ImplementationDecl)*
        """
        pass

    def init_section(self):
        """
            InitSection:
              Doc: |
                ! -> END
                ! -> Block
                ! -> INITIALIZATION
                !    [StatementList]
                !    [FINALIZATION
                !    [StatementList]]
                !    END
        """
        pass

    def interface_decl(self):
        """
            InterfaceDecl:
              Doc: |
                ! -> ConstSection
                ! -> TypeSection
                ! -> VarSection
                ! -> MethodHeading
        """
        pass

    def interface_section(self):
        """
            InterfaceSection:
              Doc: |
                ! -> INTERFACE
                !    [UsesClause]
                !    (InterfaceDecl)*
        """
        pass

    def interface_type(self):
        """
            InterfaceType:
              Doc: |
                ! -> (INTERFACE | DISPINTERFACE)
                !    ['(' QualifiedIdent ')']
                !    ['[' Expression ']']
                !    (MethodOrProperty)*
                !    END
        """
        pass

    def label_decl_section(self):
        """
            LabelDeclSection:
              Doc: |
                ! -> LABEL (LabelId [','])+ ';'
        """
        pass

    def label_id(self):
        """
            LabelId:
              Doc: |
                ! -> <number>
                ! -> Ident
        """
        pass

    def method_heading(self):
        """
            MethodHeading:
              Doc: |
                ! -> [CLASS]
                !    (PROCEDURE | FUNCTION | CONSTRUCTOR | DESTRUCTOR | OPERATOR)
                !    QualifiedIdent
                !    (
                !      ['(' (Parameter [';'])* ')']
                !      [':' MethodReturnType]
                !      (Directive)*
                !    | '=' Ident
                !    )
                !    [';']
        """
        pass

    def method_implementation(self):
        """
            MethodImplementation:
              Doc: |
                // If the MethodHeading does not include 'external' or 'forward':
                ! -> MethodHeading
                !    FancyBlock ';'
                // If the MethodHeading does include 'external' or 'forward':
                ! -> MethodHeading
        """
        pass

    def method_or_property(self):
        """
            MethodOrProperty:
              Doc: |
                ! -> MethodHeading
                ! -> Property
        """
        pass

    def method_return_type(self):
        """
            MethodReturnType:
              Doc: |
                ! -> QualifiedIdent
                ! -> STRING
        """
        pass

    def mul_op(self):
        """
            MulOp:
              Doc: |
                ! -> '*'
                ! -> '/'
                ! -> DIV
                ! -> MOD
                ! -> AND
                ! -> SHL
                ! -> SHR
        """
        pass

    def open_array(self):
        """
            OpenArray:
              Doc: |
                ! -> ARRAY OF QualifiedIdent
                ! -> ARRAY OF STRING
                ! -> ARRAY OF FILE
                ! -> ARRAY OF CONST
        """
        pass

    def package(self):
        """
            Package:
              Doc: |
                ! -> PACKAGE QualifiedIdent ';'
                !    [RequiresClause]
                !    [UsesClause]
                !    (AssemblyAttribute)*
                !    END '.'
        """
        pass

    def packed_type(self):
        """
            PackedType:
              Doc: |
                ! -> PACKED Type
        """
        pass

    def parameter(self):
        """
            Parameter:
              Doc: |
                ! -> [VAR | CONST | OUT]
                !    IdentList
                !    [':' ParameterType]
                !    ['=' Expression]
        """
        pass

    def parameter_expression(self):
        """
            ParameterExpression:
              Doc: |
                ! -> Expression [':' Expression [':' Expression]]
        """
        pass

    def parameter_type(self):
        """
            ParameterType:
              Doc: |
                ! -> QualifiedIdent
                ! -> STRING
                ! -> FILE
                ! -> OpenArray
        """
        pass

    def parenthesized_expression(self):
        """
            ParenthesizedExpression:
              Doc: |
                ! -> '(' Expression ')'
        """
        pass

    def particle(self):
        """
            Particle:
              Doc: |
                ! -> <number>
                ! -> <stringliteral>
                ! -> Ident
                ! -> NIL
                ! -> ParenthesizedExpression
                ! -> SetLiteral
                ! -> STRING
                ! -> FILE
        """
        pass

    def pointer_type(self):
        """
            PointerType:
              Doc: |
                ! -> '^' Type
        """
        pass

    def portability_directive(self):
        """
            PortabilityDirective:
              Doc: |
                ! -> platform
                ! -> deprecated
                ! -> library
                ! -> experimental
        """
        pass

    def procedure_type(self):
        """
            ProcedureType:
              Doc: |
                ! -> (PROCEDURE | FUNCTION)
                !    ['(' (Parameter [';'])* ')']
                !    [':' MethodReturnType]
                !    (Directive)*
                !    [OF OBJECT]
                !    (Directive)*
        """
        pass

    def program(self):
        """
            Program:
              Doc: |
                ! -> (PROGRAM | LIBRARY) Ident ['(' IdentList ')'] ';'
                ! -> [UsesClause]
                !    (ImplementationDecl)*
                !    InitSection '.'
        """
        self.match(self.token.type)
        self.match(dt.IDENTIFIER)
        if self.token_type_is(dt.SYMBOL_LEFT_PAREN):
            self.match(dt.SYMBOL_LEFT_PAREN)
            self.ident_list()
            self.match(dt.SYMBOL_RIGHT_PAREN)
        self.match(dt.SYMBOL_SEMICOLON)
        if self.token_type_is(dt.KEYWORD) and self.token_value_is('USES'):
            self.uses_clause()
        while self.token_type_is(dt.KEYWORD) and self.token_value_is('IMPLEMENTATION'):
            self.implementation_decl()
        self.init_section()
        self.match(dt.SYMBOL_PERIOD)

    def property(self):
        """
            Property:
              Doc: |
                ! -> [CLASS]
                !    PROPERTY Ident
                !    ['[' (Parameter [';'])+ ']']
                !    [':' MethodReturnType]
                !    (PropertyDirective)*
                !    ';'
        """
        pass

    def property_directive(self):
        """
            PropertyDirective:
              Doc: |
                ! -> ';' DEFAULT
                ! -> DEFAULT Expression
                ! -> DISPID Expression
                ! -> IMPLEMENTS (QualifiedIdent [','])+
                ! -> INDEX Expression
                ! -> NODEFAULT
                ! -> READ Expression
                ! -> READONLY
                ! -> STORED Expression
                ! -> WRITE Expression
                ! -> WRITEONLY
        """
        pass

    def qualified_ident(self):
        """
            QualifiedIdent:
              Doc: |
                ! -> Ident ('.' ExtendedIdent)*
        """
        pass

    def raise_statement(self):
        """
            RaiseStatement:
              Doc: |
                ! -> RAISE [Expression [AT Expression]]
        """
        pass

    def record_helper_type(self):
        """
            RecordHelperType:
              Doc: |
                ! -> RECORD HELPER FOR QualifiedIdent
                !    (VisibilitySection)*
                !    END
        """
        pass

    def record_type(self):
        """
            RecordType:
              Doc: |
                ! -> RECORD
                !    (VisibilitySection)*
                !    [VariantSection]
                !    END
        """
        pass

    def rel_op(self):
        """
            RelOp:
              Doc: |
                ! -> '='
                ! -> '>'
                ! -> '<'
                ! -> '<='
                ! -> '>='
                ! -> '<>'
                ! -> IN
                ! -> IS
                ! -> AS
        """
        pass

    def repeate_statement(self):
        """
            RepeatStatement:
              Doc: |
                ! -> REPEAT [StatementList] UNTIL Expression
        """
        pass

    def requires_clause(self):
        """
            RequiresClause:
              Doc: |
                ! -> REQUIRES (QualifiedIdent [','])+ ';'
        """
        pass

    def set_literal(self):
        """
            SetLiteral:
              Doc: |
                ! -> '[' [ExpressionOrRangeList] ']'
        """
        pass

    def set_type(self):
        """
            SetType:
              Doc: |
                ! -> SET OF Type
        """
        pass

    def simple_expression(self):
        """
            SimpleExpression:
              Doc: |
                ! -> Term (AddOp Term)*
        """
        pass

    def simple_statement(self):
        """
            SimpleStatement:
              Doc: |
                ! -> BareInherited
                ! -> ExpressionOrAssignment
                ! -> GotoStatement
                ! -> Block
                ! -> IfStatement
                ! -> CaseStatement
                ! -> RepeatStatement
                ! -> WhileStatement
                ! -> ForStatement
                ! -> WithStatement
                ! -> TryStatement
                ! -> RaiseStatement
        """
        pass

    def statement(self):
        """
            Statement:
              Doc: |
                ! -> LabelId ':' [SimpleStatement]
                ! -> SimpleStatement
        """
        pass

    def statement_list(self):
        """
            StatementList:
              Doc: |
                ! -> ([Statement] [';'])+
        """
        pass

    def string_type(self):
        """
            StringType:
              Doc: |
                ! -> STRING
                ! -> STRING '[' Expression ']'
        """
        pass

    def term(self):
        """
            Term:
              Doc: |
                ! -> Factor (MulOp Factor)*
        """
        pass

    def try_statement(self):
        """
            TryStatement:
              Doc: |
                ! -> TRY
                !      [StatementList]
                !    ( FINALLY [StatementList]
                !    | EXCEPT (
                !      [StatementList] |
                !      (ExceptionItem)* [ELSE [StatementList]]
                !    )
                !    END
        """
        pass

    def type(self):
        """
            Type:
              Doc: |
                // Note: Delphi assumes that a Type starting with '(' is an enum, not an expression.
                ! -> EnumeratedType
                ! -> ExpressionOrRange
                ! -> ArrayType
                ! -> SetType
                ! -> FileType
                ! -> RecordHelperType
                ! -> RecordType
                ! -> PointerType
                ! -> StringType
                ! -> ProcedureType
                ! -> ClassHelperType
                ! -> ClassOfType
                ! -> ClassType
                ! -> InterfaceType
                ! -> PackedType
        """
        pass

    def type_constant(self):
        """
            TypedConstant:
              Doc: |
                ! -> Expression
                ! -> '(' (QualifiedIdent ':' TypedConstant [';'])+ ')'
                ! -> '(' (TypedConstant [','])+ ')'
                ! -> '(' ')'
        """
        pass

    def type_decl(self):
        """
            TypeDecl:
              Doc: |
                ! -> Ident '=' [TYPE] Type (PortabilityDirective)* ';'
                ! -> Ident '=' CLASS ';'
                ! -> Ident '=' DISPINTERFACE ';'
                ! -> Ident '=' INTERFACE ';'
        """
        pass

    def type_section(self):
        """
            TypeSection:
              Doc: |
                ! -> TYPE (TypeDecl)+
        """
        pass

    def unary_operator(self):
        """
            UnaryOperator:
              Doc: |
                ! -> NOT
                ! -> '+'
                ! -> '-'
                ! -> '@'
                ! -> INHERITED
        """
        pass

    def unit(self):
        """
            Unit:
              Doc: |
                ! -> UNIT Ident (PortabilityDirective)* ';'
                !    InterfaceSection
                !    ImplementationSection
                !    InitSection '.'
        """
        self.match(dt.KEYWORD)
        self.ident()
        while self.token_type_is(dt.KEYWORD) and self.token_value_is('PLATFORM', 'DEPRECATED', 'LIBRARY', 'EXPERIMENTAL'):
            self.portability_directive()
        self.match(dt.SYMBOL_SEMICOLON)
        self.interface_section()
        self.implementation_section()
        self.init_section()
        self.match(dt.SYMBOL_PERIOD)

    def unit_clause(self):
        """
            UsesClause:
              Doc: |
                ! -> (USES | CONTAINS)
                !    (UsedUnit [','])+ ';'
        """
        pass

    def used_unit(self):
        """
            UsedUnit:
              Doc: |
                ! -> Ident
                ! -> Ident IN <stringliteral>
        """
        pass

    def var_decl(self):
        """
            VarDecl:
              Doc: |
                ! -> IdentList ':' Type
                !    (PortabilityDirective)*
                !    [ABSOLUTE Expression | '=' TypedConstant]
                !    (PortabilityDirective)*
                !    ';'
        """
        pass

    def variant_group(self):
        """
            VariantGroup:
              Doc: |
                ! -> ExpressionList ':'
                !    '('
                ! -> (FieldDecl)*
                !    [VariantSection]
                !    ')' [';']
        """
        pass

    def variant_section(self):
        """
            VariantSection:
              Doc: |
                ! -> CASE [Ident ':'] QualifiedIdent OF
                !    (VariantGroup)+
        """
        pass

    def var_section(self):
        """
            VarSection:
              Doc: |
                ! -> (VAR | THREADVAR) (VarDecl)+
        """
        pass

    def visibility(self):
        """
            Visibility:
              Doc: |
                ! -> STRICT PRIVATE
                ! -> STRICT PROTECTED
                ! -> PRIVATE
                ! -> PROTECTED
                ! -> PUBLIC
                ! -> PUBLISHED
        """
        pass

    def visibility_section(self):
        """
            VisibilitySection:
              Doc: |
                ! -> [Visibility]
                !    (VisibilitySectionContent)*
        """
        pass

    def visibility_section_content(self):
        """
            VisibilitySectionContent:
              Doc: |
                ! -> FieldSection
                ! -> MethodOrProperty
                ! -> ConstSection
                ! -> TypeSection
        """
        pass

    def while_statement(self):
        """
            WhileStatement:
              Doc: |
                ! -> WHILE Expression DO [Statement]
        """
        pass

    def with_statement(self):
        """
            WithStatement:
              Doc: |
                ! -> WITH ExpressionList DO [Statement]
        """
        pass
