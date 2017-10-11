import sys
import re
import shlex

################################################################################
################################################################################
 # Welcome to my coursework! This is a predictive parser for Lua.
################################################################################
################################################################################

# I used this at some points to print out debugging info for my own uses. It is
# probably useless now and is designed to do nothing
def verbose_print(message):
    if False:
        print(str(message) + "\n")

################################################################################
# MAIN CALL POINT FOR THE APPLICATION
#------------------------------------------------------------------------------#
# Takes the path to the file, lexes the content into list of tokens and lexes
# the content
################################################################################
def parse(path):
    (tokens, lexical_errors) = Lexer(path).produce_tokens()
    if len(lexical_errors) != 0:
        print("Errors found")
        print("Found " + str(len(lexical_errors)) + " lexical errors. Will not parse file.")
        for error in lexical_errors:
            print(error)
        return


    parser = Parser(tokens)
    success = parser.begin()
    count_errors = len(parser.errors)
    if count_errors == 0:
        # Parse was successful so we print a list of the function declarations
        # in the format FUNCTION(PARAMATER_2, PARAMTER_2, etc)
        print("No errors found")
        count_func = len(parser.functionDeclarations)
        if count_func == 0:
            print("No functions were declared")
        else:
            print("" + str(len(parser.functionDeclarations)) + " functions were declared:")
        for func in parser.functionDeclarations:
            print(func.toString())
        if not anon:
            print("Add -a flag after file name to also list anonymous functions")
    else:
        # The parse was unsuccessful, so we print the error messages
        print("Errors found")
        print("\tNumber of errors: " + str(count_errors))
        for message in parser.errors:
            print(message)

################################################################################
# Represent a token. Each contains its type (i.e. STRING, NUMBER, etc.) plus
# where it was in file and what actual lexeme matched to it if appropriate.
# Note that each kind of token is defined as a name, so in code we can refer
# to tokens as e.g. Token.STRING rather than 'STRING' -> this reduced a lot of
# semantic errors into interpretation ones (e.g. easier to recognise mispelling
# of a variable name than a string!)
################################################################################
class Token(object):
    STRING = 'STRING'
    NUMBER = 'NUMBER'
    SEMICOLON = 'SEMICOLON'
    MINUS = 'MINUS'
    BINOP = 'BINOP'
    EQUALS = 'EQUALS'
    HASHTAG = 'HASHTAG'
    LBRACE = 'LBRACE'
    RBRACE = 'RBRACE'
    LBRACKET = 'LBRACKET'
    RBRACKET = 'RBRACKET'
    RSQ = 'RSQ'
    LSQ = 'LSQ'
    COLON = 'COLON'
    COMMA = 'COMMA'
    NAME = 'NAME'
    DO = 'do'
    END = 'end'
    WHILE = 'while'
    REPEAT = 'repeat'
    UNTIL = 'until'
    IF = 'if'
    THEN = 'then'
    ELSEIF = 'elseif'
    ELSE = 'else'
    FOR = 'for'
    IN = 'in'
    FUNCTION = 'function'
    LOCAL = 'local'
    RETURN = 'return'
    BREAK = 'break'
    NIL = 'nil'
    FALSE = 'false'
    TRUE = 'true'
    AND = 'and'
    OR = 'or'
    NOT = 'not'
    EOF = 'EOF'
    VARARGS = 'VARARGS'
    DOT = 'DOT'

    def __init__(self, kind, content, line_no, start_pos):
        self.kind = kind
        self.content = content
        self.line_no = line_no
        self.start_pos = start_pos

    def __str__(self):
        return self.kind + '{ at line ' + str(self.line_no) + ', col: ' + str(self.start_pos) + ', contains: ' + str(self.content) + '}'

################################################################################
# Lexer class!
# This goes through the file and tokenizes it all to be given to the parser.
# It used regular expressions to match strings. It appenda a special EOF token
# at the end!
################################################################################
class Lexer(object):
    def __init__(self, path):
        self.file = open(path, 'rt').read()

    def produce_tokens(self):
        lexical_errors = []

        # Tokens are matched in order of the priority dictated by this list.
        # I.e.e strings are matched first and so on..
        component_tokens = [
            (Token.STRING, r'\'\'|\"\"|\'.*?[^\\]\'|\".*?[^\\]\"|\[(=)*\[.*?[^\\\]\[]\](=)*\]'),

            # Numbers can be hexadecimal or they can be decimals or they can be
            # appended with an exponential part. I do NOT allow numbers like .1
            # but do allow 1.
            (Token.NUMBER, r'0x[0-9a-f]+|[0-9]+(\.[0-9]*)?([eE]\-?[0-9]+(\.[0-9]+)?)?|[0-9]*(\.[0-9]+)([eE]\-?[0-9]+(\.[0-9]+)?)?'),

            # PROBLEMS:
            # Lexical errors in strings
            # Numbers currently do NOT support xxxx(e|E)yyyyy


            (Token.SEMICOLON, r';'),
            (Token.MINUS, r'[\-]'),
            # Have to match ... before .. and .. before . because otherwise these tokens are split
            (Token.VARARGS, r'\.\.\.'),
            (Token.BINOP, r'\+|\*|\/|\^|\%|\.\.|\<\=|\<|\>\=|\>|==|\~\='),
            (Token.DOT, r'\.'),

            (Token.EQUALS, r'='),
            (Token.HASHTAG, r'\#'),
            (Token.LBRACE, r'[\{]'),
            (Token.RBRACE, r'[\}]'),
            (Token.LBRACKET, r'[\(]'),
            (Token.RBRACKET, r'[\)]'),
            (Token.RSQ, r'[\]]'),
            (Token.LSQ, r'[\[]'),
            (Token.COLON, r'[\:]'),
            (Token.COMMA, r'[\,]'),

            # We skip new lines
            ('NEXT_LINE', r'\n'),

            # Have to do extra checks on names: ensure we haven't used reserved word
            (Token.NAME, r'([A-Za-z_])[A-Za-z_0-9]*'),
            ('WHITESPACE', r'[ \t]+'),
            ('error', r'.'),
        ]

        # These words are reserved and cannot be used as names. SO when we match a name,
        # we check if it belongs to this set. If it does, we change that token's identity
        # from being a name to being the particular keyword it matched.
        reserved = {
            Token.DO, Token.END, Token.WHILE, Token.REPEAT, Token.UNTIL, Token.IF, Token.THEN, Token.ELSEIF, Token.ELSE, Token.FOR, Token.IN, Token.FUNCTION, Token.LOCAL, Token.RETURN, Token.BREAK, Token.NIL, Token.FALSE, Token.TRUE, Token.AND, Token.OR, Token.NOT
        }

        # Disjunct the regexes together!
        full_regex = '|'.join('(?P<%s>%s)' % pair for pair in component_tokens)

        line_count = 1
        tokens = []
        line_begin = 0

        for match in re.finditer(full_regex, self.file):
            kind = match.lastgroup
            content = match.group(kind)
            if kind == 'NEXT_LINE':
                line_count = line_count + 1
                line_begin = match.end()
            elif kind == 'WHITESPACE':
                # We choose to pass the whitespace and ignore it
                pass
            elif kind == 'error':
                err = LexicalError(line_count, content)
                lexical_errors.append(err.message)
            else:
                if kind == Token.NAME and content in reserved:
                    kind = content
                elif kind == 'STRING':
                    verbose_print("got a string btw")

                start_pos = match.start() - line_begin + 1
                tokens.append(Token(kind, content, line_count, start_pos))

        # Finally we append an EOF token
        tokens.append(Token(Token.EOF, 'eof', 0, 0))
        return (tokens, lexical_errors)

# These represent the first sets which are used ina predictive manner. Recall
# that if first(A) does not contain epsilon then it suffices as the first+ set.
# Note that in many cases in my productions, the first+ sets are implicitly crafted
# into the code rather than being precomputed globally here.
# So why have some of them here? Because it reduces verbosity of some code later on.
first = {
    'block': {
        Token.NAME, Token.DO, Token.WHILE, Token.REPEAT, Token.IF, Token.FOR, Token.FUNCTION, Token.LOCAL,
        Token.RETURN, Token.BREAK
    },
    'stat': {
        Token.NAME, Token.DO, Token.WHILE, Token.REPEAT, Token.IF, Token.FOR, Token.FUNCTION, Token.LOCAL
    },
    'unop': {
        Token.NOT, Token.MINUS, Token.HASHTAG
    },
    'parlist': {
        Token.NAME, Token.VARARGS
    },
    'value': {
        Token.NIL, Token.FALSE, Token.TRUE, Token.NUMBER, Token.STRING, Token.VARARGS, Token.FUNCTION, Token.LBRACE, Token.LBRACKET, Token.NAME
    },
    'exp': {
        Token.NIL, Token.FALSE, Token.TRUE, Token.NUMBER, Token.STRING, Token.VARARGS, Token.FUNCTION, Token.LBRACE, Token.LBRACKET, Token.NAME,
        Token.NOT, Token.MINUS, Token.HASHTAG
    },
    'property': {
        Token.LSQ, Token.DOT
    },
    'invocation': {
        Token.LBRACE, Token.LBRACKET, Token.STRING, Token.COLON
    },
    'field': {
        Token.LSQ, Token.NAME,
        Token.NIL, Token.FALSE, Token.TRUE, Token.NUMBER, Token.STRING, Token.VARARGS, Token.FUNCTION, Token.LBRACE, Token.LBRACKET, Token.NAME,
        Token.NOT, Token.MINUS, Token.HASHTAG
    },
    'fieldsep': {
        Token.COMMA, Token.SEMICOLON
    },
    'binop': {
        Token.BINOP, Token.MINUS, Token.AND, Token.OR
    },
    'tableconstructor': {
        Token.LBRACE
    },
    'funcbody': {
        Token.LBRACKET
    },
    'functiondef': {
        Token.FUNCTION
    },
    'args': {
        Token.LBRACKET, Token.LBRACE, Token.STRING
    },
    'suffix_property': {
        Token.LBRACE, Token.LBRACKET, Token.STRING, Token.COLON,
        Token.LSQ, Token.DOT
    },
    'namelist': {
        Token.NAME
    },
    'explist': {
        Token.NIL, Token.FALSE, Token.TRUE, Token.NUMBER, Token.STRING, Token.VARARGS, Token.FUNCTION, Token.LBRACE, Token.LBRACKET, Token.NAME,
        Token.NOT, Token.MINUS, Token.HASHTAG
    },
    'funcname': {
        Token.NAME
    },
    'for_stat' : {
        Token.EQUALS, Token.COMMA
    },
    'local_stat' : {
        Token.FUNCTION, Token.NAME
    },
    'stat_2': {
        Token.LSQ, Token.DOT,
        Token.LBRACE, Token.LBRACKET, Token.STRING, Token.COLON
    },
    'var': {
        Token.LBRACKET, Token.NAME
    }
}


def set_string(token_set):
    # Sorry, we need this because we can't give the semantic names (e.g. +) to a
    # Token kind and still expect the regex compilation to work. Hence we define
    # pretty printing ways of these in here with a lookup table
    lookup = {
        Token.STRING : "STRING",
        Token.NUMBER : "NUMBER",
        Token.SEMICOLON : "';'",
        Token.MINUS : "'-'",
        Token.BINOP : "'+', '*', '/', '^', '%', '..', '<', '<=', '>, '>=', '==', '~='",
        Token.EQUALS : "'='",
        Token.HASHTAG : "'#'",
        Token.LBRACE : "'{'",
        Token.RBRACE : "'}'",
        Token.LBRACKET : "'('",
        Token.RBRACKET : "')'",
        Token.RSQ : "']'",
        Token.LSQ : "'['",
        Token.COLON : "':'",
        Token.COMMA : "','",
        Token.NAME : 'NAME',
        Token.DO : "'do'",
        Token.END : "'end'",
        Token.WHILE : "'while'",
        Token.REPEAT : "'repeat'",
        Token.UNTIL : "'until'",
        Token.IF : "'if'",
        Token.THEN : "'then'",
        Token.ELSEIF : "'elseif'",
        Token.ELSE : "'else'",
        Token.FOR : "'for'",
        Token.IN : "'in'",
        Token.FUNCTION : "'function'",
        Token.LOCAL : "'local'",
        Token.RETURN : "'return'",
        Token.BREAK : "'break'",
        Token.NIL : "'nil'",
        Token.FALSE : "'false'",
        Token.TRUE : "'true'",
        Token.AND : "'and'",
        Token.OR : "'or'",
        Token.NOT : "'not'",
        Token.EOF : 'EOF',
        Token.VARARGS : "'...'",
        Token.DOT : "'.'",
    }
    if len(token_set) == 1:
        tok = token_set.pop()
        if tok == Token.EOF:
            return "'" + lookup[Token.EOF] + "' or subsequent statement starting " + str(first['block'])
        return "'" + lookup[tok] + "'"
    else:
        new_set = set()
        for token in token_set:
            new_set.add(lookup[token])
        result = ', '.join(new_set)
        return "one of " + str(result)

################################################################################
# NOT IN USE ANYMORE? Can't remember. It's late!
################################################################################
class ConsumptionException(Exception):
    def __init__(self, parser, kind):
        lookahead = parser.tokens[parser.ptr]
        line = lookahead.line_no
        col = lookahead.start_pos
        self.message = ("Line " + str(line) + ", column " + str(col) + ". Encountered '" + lookahead.content + "' but expected " + set_string({kind}))

################################################################################
# Risen if we got to a production but our first+ sets did not contain the
# lookahead symbol so there was no way of continuing.
################################################################################
class ProductionExhaustionException(Exception):
    def __init__(self, rule, parser):
        lookahead = parser.tokens[parser.ptr]
        line = lookahead.line_no
        col = lookahead.start_pos
        self.message = ("Line " + str(line) + ", column " + str(col) + ". Encountered '" + lookahead.content + "' but expected " + set_string(first[rule]))
        parser.errors.append(self.message)

################################################################################
# Risen if an earlier recent attempt at error recovery failed to fix this issue.
# The parser then uses this to try and syncrhonize itself so that it can continue
################################################################################
class PanicException(Exception):
    def __init__(self, rule, parser):
        errors.append("PANIC EXCEPTION on line " + parser.tokens[parser.ptr].line_no + "=, with start position " + parser.tokens[parser.ptr].start_pos)


################################################################################
# Encountered some lexical error -> choose not to continue the parse
################################################################################
class LexicalError(Exception):
    def __init__(self, line, content):
        self.message = "Line " + str(line) + ", encountered lexical error with: '" + str(content) + "'"


################################################################################
# Class representation of function declaration objects. Essentially a 3-tuple.
################################################################################
class FunctionDeclaration(object):
    def __init__(self, name, parameters, modifier, line, pos):
        self.name = name # Name of the function
        self.parameters = parameters # List of the names of its paramaters
        self.modifier = modifier #Local or anonymous or empty string
        self.line = line
        self.pos = pos

    def toString(self):
        '''
        Return a string representation of the function declaration
        '''
        result = self.modifier + " " + self.name + " "
        if len(self.parameters) == 0:
            return result + "() at Line " + str(self.line) + ", column " + str(self.pos)
        else:
            result += "("
            for p in range(len(self.parameters) - 1):
                result += self.parameters[p] + ', '
            return result + self.parameters[len(self.parameters) - 1] + ") at Line " + str(self.line) + ", column " + str(self.pos)

################################################################################
# Class representation of the parser. Its functions almost completely represent
# the productions of the Lua grammar I have designed for this coursework.
# It also has some functions that provide a well-defined and well abstracted
# interface to the lexer; in particular the way of checking if the lookahead
# matches the desired token, and for consuming said lookahead.
# Note that this performs a predictive parse!
################################################################################
class Parser(object):
    def __init__(self, tokens):
        self.tokens = tokens # List of lexed tokens, always ends with EOF token
        self.errors = [] #List of error messages to report
        self.functionDeclarations = [] # List of function declarations to report
        self.ptr = 0 # Integer index of lookahead symbol in self.tokens
        self.problem = False # If we try to consume a token and fail, we flag this up. If we succeed, we unflag.
        self.suffix_stack = [] # Stack used only in very special cases of the kleene_suffix production. You should read stat_1 or stat_2 and kleen_suffix production rules.

    # Function checks if lookahead matches the parameter, if so, it is consumed
    # and True is returned. Else we need to do some error handling...
    def consume(self, kind):
        if self.match(kind):
            # Lookahead and expected token match! So consume, and return True
            self.problem = False
            self.consumeAny()
            return True
        else:
            if not self.problem:
                # Out previous consumption worked, so (locally) this is the first error.
                # For now, we'll attempt to continue by assuming the token was there
                self.problem = True

                err = ConsumptionException(self, kind)
                self.errors.append(err.message)
                raise err
            else:
                # We hads two consecutive consumption failures!! We panic and let
                #  the production itself attempt a resynchronization
                raise PanicException("Attempt to resynchronize!")

    # Function consumes the current lookahead symbol
    # Warning: Only use if have checked for a match before hand!!!!!!!!
    def consumeAny(self):
        self.ptr += 1

    # Function returns True if lookahead is of TokenType: kind. Else False
    def match(self, kind):
        return kind == self.tokens[self.ptr].kind

    # We check if the lookahead is in the supplied token set. This is useful if
    # we want to pass it first set for instance. Rather than chain a load of
    # if match() or match ()... statements together.
    def lookaheadIsInSet(self, tokenSet):
        return self.tokens[self.ptr].kind in tokenSet

    ########################################################################
    # THE FOLLOWING FUNCTIONS IN THIS CLASS REPRESENT THE PRODUCTION
    # RULES FOR THE DERIVED GRAMMAR
    ########################################################################

    # First point of entry to parser.
    # TODO maybe rename as start(self)
    def begin(self):
        try:
            return self.block() and self.consume(Token.EOF)
        except ConsumptionException:
            return self.try_sync()
        except ProductionExhaustionException:
            return self.try_sync()
        except PanicException:
            return self.try_sync()

    def try_sync(self):
        while (self.ptr < len(self.tokens)):
            if self.ptr == len(self.tokens) - 1:
                try:
                    return self.consume(Token.EOF)
                except ConsumptionException:
                    pass
            elif self.tokens[self.ptr].kind == Token.EOF:
                try:
                    return self.consume(Token.EOF)
                except ConsumptionException:
                    pass
            elif self.tokens[self.ptr].kind not in {
                Token.DO, Token.IF, Token.LOCAL, Token.WHILE, Token.REPEAT,
                Token.FOR, Token.RETURN, Token.BREAK, Token.END, Token.FUNCTION
            }:
                print(self.tokens[self.ptr])
                self.consumeAny()
            elif self.tokens[self.ptr].kind == Token.END:
                self.consumeAny()
                return self.begin()
            else:
                # Tokens is in the sync set!
                return self.begin()

    # I removed chunk and just used block.
    def block(self):
        '''block ::=
            multistat laststat
        '''
        # Note epsilon IS possible here. In both of the following.
        #So empty blocks can and do happen gracefully!
        return self.multistat() and self.laststat()

    # Multiple statements, with each statement optionally followed by a semicolon
    def multistat(self):
        '''multistat ::= (stat (;)?)*'''
        verbose_print('multistat')
        if self.lookaheadIsInSet(first['stat']):
            return (
                self.stat()
                and self.opt_semicolon()
                and self.multistat()
            )

            # Idea if stat was false: Try to syncrhonize AT THIS point to the next stat or laststat invocation?
        else:
            # EPSILON
            return True

    # This is the most "essential" nonterminal in a way. In that it defines
    # program statements for Lua. Which are contained in blocks. But may of course
    # contain their own blocks as constituent pieces.
    def stat(self):
        '''stat ::=
            Name stat_1
            | '(' exp ')' stat_2
            | do_block
            | while exp do block end
            | repeat block until exp
            | if exp then block (elseif exp then block)* (else block)? end
            | for Name for_stat
            | function funcname funcbody
        '''
        # Looking through all of the productions... most are simple: they match the grammar!
        if self.match(Token.NAME):
            return (
                self.consume(Token.NAME)
                and self.stat_1()
            )
        if self.match(Token.LBRACKET):
            return (
                self.consume(Token.LBRACKET)
                and self.exp()
                and self.consume(Token.RBRACKET)
                and self.stat_2()
            )
        if self.match(Token.DO):
            return (
                self.do_block()
            )
        if self.match(Token.WHILE):
            return (
                self.consume(Token.WHILE)
                and self.exp()
                and self.do_block()
            )
        if self.match(Token.REPEAT):
            return (
                self.consume(Token.REPEAT)
                and self.block()
                and self.consume(Token.UNTIL)
                and self.exp()
            )
        if self.match(Token.IF):
            intermediate_result = (
                self.consume(Token.IF)
                and self.exp()
                and self.consume('then')
                and self.block()
            )

            # We've just consumed the if exp then block part.
            # But now we have to check for zero or more elseif blocks.

            # Loop through whilst the lookahead is ELSEIF. Then derive the
            # elseif blocks.
            while self.match(Token.ELSEIF):
                intermediate_result = (
                    intermediate_result
                    and self.consume(Token.ELSEIF)
                    and self.exp()
                    and self.consume(Token.THEN)
                    and self.block()
                )

            # Now we've handled all of our elseif blocks. SO check for zero
            # or one ending else block.
            if self.match(Token.ELSE):
                intermediate_result = (
                    intermediate_result
                    and self.consume(Token.ELSE)
                    and self.block()
                )
            return (
                intermediate_result
                and self.consume(Token.END)
            )

        if self.match(Token.FOR):
            return (
                self.consume(Token.FOR)
                and self.consume(Token.NAME)
                and self.for_stat()
            )

        line = self.tokens[self.ptr].line_no
        pos = self.tokens[self.ptr].start_pos
        if self.match(Token.FUNCTION):
            self.consume(Token.FUNCTION)
            # Funcname will return a string of the name of this function
            name = self.funcname()
            # Funcbody will colate the parameters and is supplied the name
            # plus the modifier. Here, there is no modifier (this is NOT a local func)
            self.funcbody(name, "", line, pos)
            return True

        if self.match(Token.LOCAL):
            return (
                self.consume(Token.LOCAL)
                and self.local_stat()
            )
        else:
            raise ProductionExhaustionException('stat', self)
    # except PanicException:
    #     while (self.tokens[self.ptr].kind not in first['stat']
    #         or self.tokens[self.ptr].kind not in first['laststat']
    #         or self.tokens[self.ptr].kind != Token.EOF
    #     ):
    #         self.consumeAny()
    #
    #     if self.tokens[self.ptr].kind in first['stat']:
    #         return self.multistat()
    #     if self.tokens[self.ptr].kind in first['laststat']:
    #         return self.laststat()
    #     else:
    #         #Got an EOF
    #         return self.consume(Token.EOF)

    # This is essentially a more general case of stat_2, which you should read.
    def stat_1(self):
        '''stat_1 ::=
            {',', var} `=´ explist
            | stat_2
        '''
        # If raises a PanicException, it bubbles up for handling in the stat production
        if self.lookaheadIsInSet({Token.COMMA, Token.EQUALS}):
            return (
                self.kleene_comma_var()
                and self.consume(Token.EQUALS)
                and self.explist()
            )
        else:
            return self.stat_2()

    # For context, you should know kleene_suffix = (property|invocation)*
    def stat_2(self):
        '''stat_2 ::=
            kleene_suffix property (',', var)* '=' explist
            | kleene_suffix invocation
        '''
        # If raises a PanicException, it bubbles up for handling in the stat production

        # So here we have to be careful. We invoke kleene_suffix, but as this
        # consists of a kleene closure of a disjunction between property or invocation,
        # we would consume the property or the invocation before kleene_suffix then
        # chooses epsilon. Essentially we need unbounded lookahead.

        # Instead, we design a situation of "lookback". We maintain a stack of the
        # last invocation that kleene_suffix made: property or invocation. Then we just invoke
        # kleene_suffix and check which one we got. And progress from there.

        # Why a stack and not just a flag? Because this deals with nesting of
        # variable property accesses within function calls. This issue caused me
        # IMMENSE difficulty for a lot of the coursework!

        # Start by pushing None on. If k_s chose epsilon, we have None at the pop.
        # Else it will have the production that SHOULD have been made after a K-s.
        # I.e. the 'context sensitive' part!
        self.suffix_stack.append(None)
        k_s = self.kleene_suffix()
        last = self.suffix_stack.pop()

        if last == 'invocation':
            # Functioncalls are their own statements. Looks like we're done here!
            return True
        if last == 'property':
            # Indices are followed by assignments
            return (
                self.kleene_comma_var()
                and self.consume(Token.EQUALS)
                and self.explist()
            )
        else:
            raise ProductionExhaustionException('stat_2', self)

    # This one uses a stack just to judge its context. More on this is discussed
    # in the places where it is used
    def kleene_suffix(self):
        '''
        kleene_suffix ::= (property|invocation)*
        '''

        # Each time we call kleene_suffix we push None onto the suffix stack.
        # The top of this stack is supposed to reflect the resulting invocation that
        # kleene_suffix last made. If after this production has succeeded we
        # still have None then Epsilon was produced.

        # Why do we care whether it called property or invocation? Because in some places
        # we require arbitrary lookahead to determine whether we have made an
        # invocation or a property access (e.g. indexing). As such we have to
        # be careful in working out exactly when we are allowed to assign to
        # a sentence in this grammar.

        # You may read more about this in places where kleene_suffix is called!
        if self.lookaheadIsInSet(first['property']):
            # Replace old value with property
            self.suffix_stack.pop()
            self.suffix_stack.append('property')

            return (
                self.property()
                and self.kleene_suffix()
            )
        if self.lookaheadIsInSet(first['invocation']):
            # Replace old value with call

            self.suffix_stack.pop()
            self.suffix_stack.append('invocation')
            return (
                self.invocation()
                and self.kleene_suffix()
            )
        else:
            verbose_print("epsilon")
            return True


    # Do blocks are common to many productions so this encourages code reuse!!!
    def do_block(self):
        '''do_block ::=
            do block end
        '''
        return (
            self.consume(Token.DO)
            and self.block()
            and self.consume(Token.END)
        )

    def local_stat(self):
        '''local_stat ::=
            function Name funcbody
            | namelist ('=' explist)?
        '''
        line = self.tokens[self.ptr].line_no
        pos = self.tokens[self.ptr].start_pos
        if self.match(Token.FUNCTION):
            self.consume(Token.FUNCTION)
            if self.match(Token.NAME):
                name = self.tokens[self.ptr].content
                self.consume(Token.NAME)
                return self.funcbody(name, "local", line, pos)
            else:
                # Force consumption exception!
                self.consume(Token.NAME)

        if self.match(Token.NAME):
            # Must be a namelist so we derive that and then attempt the optional
            # equals explist
            intermediate_result = self.namelist()
            if self.match(Token.EQUALS):
                return (
                    intermediate_result
                    and self.consume(Token.EQUALS)
                    and self.explist()
                )
            else:
                return intermediate_result
        else:
            raise ProductionExhaustionException('local_stat', self)

    def for_stat(self):
        '''for_stat ::=
            '=' exp ',' exp (',' exp)? do_block
            | (',', Name)* in explist do_block
        '''
        if self.match(Token.EQUALS):
            intermediate_result = (
                self.consume(Token.EQUALS)
                and self.exp()
                and self.consume(Token.COMMA)
                and self.exp()
            )
            if self.match(Token.COMMA):
                intermediate_result = (
                    intermediate_result
                    and self.consume(Token.COMMA)
                    and self.exp()
                )
            return (
                intermediate_result
                and self.do_block()
            )
        if self.match(Token.COMMA) or self.match(Token.IN):
            return (
                self.kleene_comma_name()
                and self.consume(Token.IN)
                and self.explist()
                and self.do_block()
            )
        else:
            raise ProductionExhaustionException('for_stat', self)

    # Defines an optional ending statement
    def laststat(self):
        '''laststat ::=
            EPSILON
            | return opt_explist (;)?
            | break (;)?
        '''
        if self.match(Token.BREAK):
            return self.consume(Token.BREAK) and self.opt_semicolon()
        if self.match(Token.RETURN):
            return (
                self.consume(Token.RETURN)
                and self.opt_explist()
                and self.opt_semicolon()
            )
        else:
            # EPSILON
            return True

    # Production for named (non anonymous) functions. This one is very important.
    # For each successful constituent production it appends the contents to the
    # buffer. Finally it returns the resulting name for the parent ProductionExhaustionException
    # to pass down to funcbody which handles storing the full function signature
    def funcname(self):
        ''' funcname ::=
            Name ('.' Name)* (':' Name)?
        '''
        if self.match(Token.NAME):
            name = self.tokens[self.ptr].content # Take the literal from the name token
            self.consume(Token.NAME)
            while self.match(Token.DOT):
                name += '.' # keep appending content as we go through!!!
                self.consume(Token.DOT)
                if self.match(Token.NAME):
                    name += self.tokens[self.ptr].content
                    self.consume(Token.NAME)
                else:
                    # Force a consumption exception:
                    self.consume(Token.NAME)
            if self.match(Token.COLON):
                name += ':'
                self.consume(Token.COLON)
                if self.match(Token.NAME):
                    name += self.tokens[self.ptr].content
                    self.consume(Token.NAME)
                else:
                    # Force a consumption exception:
                    self.consume(Token.NAME)
            return name
        else:
            raise ProductionExhaustionException('funcname', self)

    def namelist(self):
        '''namelist ::=
            Name kleene_comma_name
        '''
        if self.match(Token.NAME):
            return (
                self.consume(Token.NAME)
                and self.kleene_comma_name()
            )
        else:
            raise ProductionExhaustionException(self, "namelist")

    def kleene_comma_name(self):
        '''kleene_comma_name ::=
            (',' Name)*
        '''
        intermediate_result = True
        # Keep trying to consume , Name whilst possible
        while self.match(Token.COMMA):
            intermediate_result = (
                intermediate_result
                and self.consume(Token.COMMA)
                and self.consume(Token.NAME)
            )
        return intermediate_result

    def kleene_comma_var(self):
        '''kleene_comma_var ::=
            (',' var)*
        '''
        intermediate_result = True
        # Keep trying to consume , var whilst possible
        while self.match(Token.COMMA):
            intermediate_result = (
                intermediate_result
                and self.consume(Token.COMMA)
                and self.var()
            )
        return intermediate_result

    def explist(self):
        '''explist ::=
                exp (',' exp)*
        '''
        verbose_print('explist')
        if self.lookaheadIsInSet(first['exp']):
            intermediate_result = self.exp()
            # Keep trying to consume , exp whilst possible
            while self.match(Token.COMMA):
                intermediate_result = (
                    intermediate_result
                    and self.consume(Token.COMMA)
                    and self.exp()
                )
            return intermediate_result
        else:
            raise ProductionExhaustionException('explist', self)

    # This essentiually comprises a removal of the left recursion on the original
    # production for exp but after some other very heavily grammar manipulations.
    # But common features to the start of an expression are factored into
    # here for brevity.
    def value(self):
        '''value ::=
            nil
            | false
            | true
            | Number
            | String
            | '...'
            | functiondef
            | tableconstructor
            | '(' exp ')' kleene_suffix
            | Name kleene_suffix
        '''
        if self.match(Token.NIL):
            return self.consume(Token.NIL)
        if self.match(Token.FALSE):
            return self.consume(Token.FALSE)
        if self.match(Token.TRUE):
            return self.consume(Token.TRUE)
        if self.match(Token.NUMBER):
            return self.consume(Token.NUMBER)
        if self.match(Token.STRING):
            return self.consume(Token.STRING)
        if self.match(Token.VARARGS):
            return self.consume(Token.VARARGS)
        if self.match(Token.FUNCTION):
            return self.functiondef()
        if self.match(Token.LBRACE):
            return self.tableconstructor()
        if self.match(Token.LBRACKET):
            intermediate_result = (
                self.consume(Token.LBRACKET)
                and self.exp()
                and self.consume(Token.RBRACKET)
            )
            self.suffix_stack.append(None)
            self.kleene_suffix()
            self.suffix_stack.pop()
            return intermediate_result
        if self.match(Token.NAME):
            self.suffix_stack.append(None)
            intermediate_result = (
                self.consume(Token.NAME)
                and self.kleene_suffix()
            )
            self.suffix_stack.pop()
            return intermediate_result
        else:
            raise ProductionExhaustionException('value', self)

    def exp(self):
        '''exp ::=
            unop exp
            | value opt_exp_ext
        '''
        if self.lookaheadIsInSet(first['unop']):
            return (
                self.unop()
                and self.exp()
            )
        else:
            return self.value() and self.opt_exp_ext()

    def opt_exp_ext(self):
        '''opt_exp_ext ::=
            binop exp
            | EPSILON
        '''
        if self.lookaheadIsInSet({Token.BINOP, Token.MINUS, Token.OR, Token.AND}):
            return self.binop() and self.exp()
        else:
            # EPSILON production
            return True

    def property(self):
        '''property ::=
            '[' exp ']'
            | '.' Name
        '''
        if self.match(Token.LSQ):
            return (
                self.consume(Token.LSQ)
                and self.exp()
                and self.consume(Token.RSQ)
            )
        if self.match(Token.DOT):
            return (
                self.consume(Token.DOT)
                and self.consume(Token.NAME)
            )
        else:
            raise ProductionExhaustionException('property', self)

    def invocation(self):
        '''invocation ::=
            args
            | ':' Name args
        '''
        if self.match(Token.COLON):
            return (
                self.consume(Token.COLON)
                and self.consume(Token.NAME)
                and self.args()
            )
        else:
            return self.args()

    def var(self):
        '''var ::=
            '(' exp ')' suffix_property
            | Name var_ext
        '''
        if self.match(Token.LBRACKET):
            return (
                self.consume(Token.LBRACKET)
                and self.exp()
                and self.consume(Token.RBRACKET)
                and self.suffix_property()
            )
        if self.match(Token.NAME):
            return (
                self.consume(Token.NAME)
                and self.var_ext()
            )
        else:
            raise ProductionExhaustionException('var', self)

    def var_ext(self):
        '''var_ext ::=
            EPSILON
            | suffix_property
        '''
        if (
            self.lookaheadIsInSet(first['property'])
            or self.lookaheadIsInSet(first['invocation'])
        ):
            return self.suffix_property()

        else:
            return True

    # Here we expect epsilon or a kleene closure of suffix followed by an property
    def suffix_property(self):
        '''suffix_property ::=
            (suffix)* property
        '''

        self.suffix_stack.append(None)
        intermediate_result = self.kleene_suffix()
        last = self.suffix_stack.pop()

        if last == 'property':
            return intermediate_result
        else:
            raise ProductionExhaustionException('suffix_property', self)

    def args(self):
        '''args ::=
            '(' (explist)? ')'
            | tableconstructor
            | String
        '''
        if self.match(Token.LBRACKET):
            return (
                self.consume(Token.LBRACKET)
                and self.opt_explist()
                and self.consume(Token.RBRACKET)
            )
        if self.match(Token.LBRACE):
            return self.tableconstructor()
        if self.match(Token.STRING):
            return self.consume(Token.STRING)
        else:
            raise ProductionExhaustionException('args', self)

    # Defining a closure (an anonymous function)
    def functiondef(self):
        '''functiondef ::=
            'function' funcbody
        '''
        line = self.tokens[self.ptr].line_no
        pos = self.tokens[self.ptr].start_pos
        if self.match(Token.FUNCTION):
            if anon:
                return (
                    self.consume(Token.FUNCTION)
                    and self.funcbody("<ANONYMOUS FUNCTION> with arguments:", "", line, pos)
                )
            else:
                return (
                    self.consume(Token.FUNCTION)
                    and self.funcbody("<ANONYMOUS FUNCTION> with arguments:", None, line, pos)
                )
            # We supply the funcbody production a name of None to reflect the
            # fact this is anonymous, and we provide anonymous as a kind of modifier.
        else:
            raise ProductionExhaustionException('functiondef', self)

    # We provide the funcbody the name of the function that was previously defined.
    # Note that this may very will be None if the function was a closure (an
    # anonymous function)
    def funcbody(self, name, modifier, line_no, start_pos):
        '''funcbody ::=
            '(' (parlist)? ')' block end
        '''
        if self.match(Token.LBRACKET):
            self.consume(Token.LBRACKET)
            parlist = self.parlist()
            if modifier != None:
                self.functionDeclarations.append(FunctionDeclaration(name, parlist, modifier, line_no, start_pos))
            return (
                self.consume(Token.RBRACKET)
                and self.block()
                and self.consume(Token.END)
            )
        else:
            raise ProductionExhaustionException('funcbody', self)

    # Defines a list of paramaters. Instead of a true false value, it returns
    # and expression to define the parameters of the function being defined
    def parlist(self):
        '''parlist ::=
            namelist (',' '...')?
            | '...'
        '''
        parameters = []
        if self.match(Token.VARARGS):
            parameters.append('...')
            self.consume(Token.VARARGS)
            return parameters

        if self.match(Token.NAME):
            parameters.append(self.tokens[self.ptr].content)
            self.consume(Token.NAME)
            while self.match(Token.COMMA):
                self.consume(Token.COMMA)
                if self.match(Token.VARARGS):
                    parameters.append('...')
                    self.consume(Token.VARARGS)
                    return parameters
                if self.match(Token.NAME):
                    parameters.append(self.tokens[self.ptr].content)
                    self.consume(Token.NAME)
                else:
                    raise ProductionExhaustionException('parlist')
            return parameters
        else:
            # EPSILON
            return parameters

    def tableconstructor(self):
        '''tableconstructor ::=
            '{' (fieldlist)? '}'
        '''
        if self.match(Token.LBRACE):
            intermediate_result = self.consume(Token.LBRACE)
            if self.lookaheadIsInSet(first['field']):
                intermediate_result = (
                    intermediate_result
                    and self.fieldlist()
                )
            return (
                intermediate_result
                and self.consume(Token.RBRACE)
            )
        else:
            raise ProductionExhaustionException('tableconstructor', self)

    def fieldlist(self):
        '''fieldlist ::=
            field (fieldsep field)* (fieldsep)?
        '''









        # Be careful!
        result = self.field()
        while self.lookaheadIsInSet(first['fieldsep']):
            self.fieldsep()
            if self.lookaheadIsInSet(first['field']):
                result = (
                    self.field()
                    and result
                )
            else:
                return result
        return result

    def field(self):
        '''field ::=
            `[´ exp `]´ `=´ exp
            | Name `=´ exp
            | exp
            Slight issue in that Name \in first[exp]. But is resolved here (see comments)
        '''
        if self.match(Token.LSQ):
            return (
                self.consume(Token.LSQ)
                and self.exp()
                and self.consume(Token.RSQ)
                and self.consume(Token.EQUALS)
                and self.exp()
            )
        if self.match(Token.NAME):
            # Using a lookahead of 2 here just to avoid having to manipulate the
            # grammar excessively. This is the obly place we use lookahead of more than 1
            if self.tokens[self.ptr + 1].kind == Token.EQUALS:
                return (
                    self.consume(Token.NAME)
                    and self.consume(Token.EQUALS)
                    and self.exp()
                )
            else:
                # We have a Name, but this forms part of the exp production
                return self.exp()
        else:
            return self.exp()

    # Just one of two simple terminals
    def fieldsep(self):
        '''fieldsep ::=
            ;
            | ,
        '''
        if self.lookaheadIsInSet(first['fieldsep']):
            self.consumeAny()
        else:
            raise ProductionExhaustionException('fieldsep', self)

    def unop(self):
        '''unop ::=
            -
            | #
            | not
        '''
        if self.lookaheadIsInSet(first['unop']):
            self.consumeAny()
            return True
        else:
            # Encountered TERMINAL but expected unary operation in first['unop']
            raise ProductionExhaustionException('unop', self)

    # We resolve most of the binops into a single BINOP token. But the MINUS has
    # to be its own because it is also a unop. Similarly, and/or are their own
    # because they're required for keyword checks!
    def binop(self):
        '''binop ::=
            BINOP
            | -
            | and
            | or
        '''
        if self.match(Token.BINOP):
            return self.consume(Token.BINOP)
        if self.match(Token.MINUS):
            return self.consume(Token.MINUS)
        if self.match(Token.AND):
            return self.consume(Token.AND)
        if self.match(Token.OR):
            return self.consume(Token.OR)

        else:
            # Encountered TERMINAL but expected binary operation from {-,*,/,...}
            raise ProductionExhaustionException('binop', self)

    def opt_explist(self):
        '''opt_explist ::=
            explist
            | EPSILON
        '''
        if self.lookaheadIsInSet(first['exp']):
            return self.explist()
        else:
            return True

    def opt_semicolon(self):
        '''opt_semicolon ::=
            ';'
            | EPSILON
        '''
        if self.match(Token.SEMICOLON):
            return self.consume(Token.SEMICOLON)
        else:
            return True

anon = False
################################################################################
# Allows you to run code by running this in terminal: python3 Luaparser.py <path>
if __name__ == "__main__":
    if len(sys.argv) > 2:
        if sys.argv[2] == '-a':
            anon = True
    parse(sys.argv[1])
    # parseHardCodedList()
################################################################################
