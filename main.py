import os
from lex_analyser import Analyser

test_dir = 'C:\\Users\\Vlad\\Documents\\GitHub\\compiler-basics-2\\test files\\'

print('\nChoose input file:\n')

i = 0
for file in os.listdir(test_dir):
    if file.endswith('.txt'):
        i += 1
        print(str(i) + '.', file)

# file_num = int(input('\n> '))

tokens = []
with open(test_dir + os.listdir(test_dir)[0]) as file:
    for line in file:
        tokens.append(line.replace('\n', '').split(' '))

anal = Analyser(tokens)
print(anal.tokens, anal.cur_line, anal.cur_token)
anal.next_token()
print(anal.tokens, anal.cur_line, anal.cur_token)
anal.next_token()
print(anal.tokens, anal.cur_line, anal.cur_token)
anal.next_line()
print(anal.tokens, anal.cur_line, anal.cur_token)
anal.next_token()
print(anal.tokens, anal.cur_line, anal.cur_token)
anal.next_line()
print(anal.tokens, anal.cur_line, anal.cur_token)
