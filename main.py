import os
from lex_analyser import Analyser, LexemeTable

test_dir = 'C:\\Users\\vladl\\Documents\\GitHub\\compiler-basics-2\\test files\\'

print('\nChoose input file:\n')

i = 0
for file in os.listdir(test_dir):
    if file.endswith('.txt'):
        i += 1
        print(str(i) + '.', file)

file_num = int(input('\n> '))

tokens = []
with open(test_dir + os.listdir(test_dir)[file_num - 1]) as file:
    for line in file:
        tokens.append(line.replace('\n', '').split(' '))

Analyser(tokens).analyze()
