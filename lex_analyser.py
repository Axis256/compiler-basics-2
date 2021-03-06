class Analyser:

    def __init__(self, tokens):
        self.tokens = tokens
        self.cur_line = self.tokens[0]
        self.cur_token = self.cur_line[0]
        self.lexeme_table = LexemeTable()
        self.cur_line_num = 1
        self.error_log = []

        self.err_dict = {
            1: 'program does not start with variable declaration',
            2: 'invalid operator',
            3: 'invalid identifier',
            4: 'separator missing in variable declaration block',
            5: 'unacceptable keyword in variable declaration block',
            6: 'unacceptable keyword in calculations block',
            7: 'calculations block does not start with BEGIN statement',
            8: 'program does not end with END statement',
            9: 'additional code after END statement'
        }

        self.keywords = [
            'Var',
            'Begin',
            'End',
            'For',
            'To',
            'Do',
            ':Boolean;',
            ':Decimal;'
        ]

        self.un_ops = [
            '.NOT.'
        ]

        self.bin_ops = [
            '.AND.',
            '.XOR.',
            '.OR.',
            '-',
            '+',
            '*',
            '/',
            '>',
            '<',
            '==',
            ':='
        ]

    def is_ident(self, token):
        if token[0].isalpha() and token.isalnum():
            return True
        else:
            return False

    def is_const(self, token):
        if token.isdigit() or (token[0] == token[-1] == "'"):
            return True
        else:
            return False

    def add_to_lexeme_table(self, token):
        if token in self.keywords:
            self.lexeme_table.lexemes.append((token, 'keyword', self.cur_line_num))
        elif token in self.un_ops:
            self.lexeme_table.lexemes.append((token, 'unary operator', self.cur_line_num))
        elif token in self.bin_ops:
            self.lexeme_table.lexemes.append((token, 'binary operator', self.cur_line_num))
        elif self.is_ident(token):
            self.lexeme_table.lexemes.append((token, 'identifier', self.cur_line_num))
            if token not in self.lexeme_table.symbols:
                self.lexeme_table.symbols.append(token)
        elif self.is_const(token):
            if token.isdigit():
                token = str(hex(int(token)))
            self.lexeme_table.lexemes.append((token, 'constant', self.cur_line_num))
            if token not in self.lexeme_table.symbols:
                self.lexeme_table.symbols.append(token)
        else:
            self.error(2)

    def next_line(self):
        self.tokens = self.tokens[1:]
        self.cur_line = self.tokens[0] if len(self.tokens) > 0 else None
        self.cur_token = self.cur_line[0] if len(self.tokens) > 0 else None
        self.cur_line_num += 1

    def next_token(self):
        if len(self.cur_line) == 1:
            self.next_line()
        else:
            self.cur_line = self.cur_line[1:]
            self.cur_token = self.cur_line[0]

    def analyze(self):
        if self.cur_token == 'Var':
            self.analyze_var_declaration()
        else:
            self.error(1)
        self.analyze_calc_description()
        with open('tables.txt', 'w') as output_file:
            output_file.write('Lexemes:\n')
            for lexeme in self.lexeme_table.lexemes:
                output_file.write('Line ' + str(lexeme[2]) +  ', ' + lexeme[1] + ', ' + lexeme[0] + '\n')
            output_file.write('\nSymbols:\n')
            for symbol in self.lexeme_table.symbols:
                output_file.write(symbol + '\n')
        with open('error log.txt', 'w') as error_file:
            for line in self.error_log:
                error_file.write(line)
        # print(table.idents)
        # print(table.keywords)
        # print(table.constants)
        # print(table.idents)

    def analyze_var_declaration(self):
        if self.cur_token == 'Var':
            self.analyze_ident()
            self.next_token()
            while self.cur_token not in (':Boolean;', ':Decimal;'):
                if len(self.cur_line) == 1:
                    break
                if self.cur_line[1] in (':Boolean;', ':Decimal;'):
                    self.analyze_ident()
                    self.next_token()
                elif self.cur_token[-1] == ',':
                    self.cur_token = self.cur_token[:-1]
                    self.analyze_ident()
                    self.next_token()
                elif self.cur_line[1] == ',':
                    self.analyze_ident()
                    self.next_token()
                    self.next_token()
                else:
                    self.analyze_ident()
                    self.error(4)
                    self.next_token()
            self.analyze_keyword()
            self.next_token()
            self.analyze_var_declaration()

    def analyze_calc_description(self):
        if self.cur_token != 'Begin':
            self.error(7)
        while self.cur_token != 'End':
            if self.cur_token == '{':
                while self.cur_token != '}':
                    self.next_token()
                self.next_token()
            if len(self.tokens) == len(self.cur_line) == 1:
                self.analyze_operator()
                self.error(8)
                break
            self.analyze_operator()
            self.next_token()
        if self.cur_token == 'End':
            self.analyze_keyword()
            if len(self.cur_line) > 1 or len(self.tokens) > 1:
                self.error(9)


    def analyze_operator(self):
        if self.is_const(self.cur_token):
            self.analyze_const()
        elif self.is_ident(self.cur_token):
            self.analyze_ident()
        elif self.cur_token in self.keywords:
            if self.cur_token in ('Var', 'Begin', ':Boolean;', ':Decimal;'):
                self.error(6)
            else:
                self.analyze_keyword()
        elif self.cur_token in self.bin_ops or self.cur_token in self.un_ops:
            self.add_to_lexeme_table(self.cur_token)
        else:
            self.error(2)

    def analyze_ident(self):
        if self.is_ident(self.cur_token):
            self.add_to_lexeme_table(self.cur_token)
        else:
            self.error(3)

    def analyze_const(self):
        self.add_to_lexeme_table(self.cur_token)

    def analyze_keyword(self):
        if self.cur_token in self.keywords:
            self.add_to_lexeme_table(self.cur_token)
        else:
            self.error(5)

    def error(self, err_key):
        error_message = 'Error: ' + self.err_dict[err_key]
        if err_key in (1, 7, 8, 9):
            error_message += '.\n'
        else:
            error_message += ' detected.\t' + self.cur_token + ', line ' + str(self.cur_line_num) + '.\n'
        self.error_log.append(error_message)

class LexemeTable:
    def __init__(self):
        self.lexemes = []
        self.symbols = []
