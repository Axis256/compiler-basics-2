class Analyser:

    def __init__(self, tokens):
        self.tokens = tokens
        self.cur_line = self.tokens[0]
        self.cur_token = self.cur_line[0]

        self.keywords = [
            'Var',
            'Begin',
            'End',
            'For',
            'To',
            'Do'
        ]

        self.un_ops = [
            '.NOT.'
        ]

        self.bin_ops = [
            '.AND.'
            '.XOR.'
            '.OR.'
            '-'
            '+'
            '*'
            '/'
            '>'
            '<'
            '=='
        ]

    def next_line(self):
        self.tokens = self.tokens[1:]
        self.cur_line = self.tokens[0]
        self.cur_token = self.cur_line[0]

    def next_token(self):
        self.cur_line = self.cur_line[1:]
        self.cur_token = self.cur_line[0]

    def analyze_program(self):
        pass
