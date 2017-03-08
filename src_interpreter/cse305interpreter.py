import string;

def interpreter(infil, outfil):
    fin = open(infil, 'r');
    fout = open(outfil, 'w');

'''
def numbers_to_strings(argument):
    switcher = {
        0: "zero",
        1: "one",
        2: "two",
    }
    return switcher.get(argument, "nothing")
'''

    stack = [];

    for line in fin:
        command = line.split(' ')[0];

        if command == 'push':
            stack.append(line.split(' ')[1]);
        elif command == 'pop':
            stack.pop(); #add verification test cases
        elif command == ':true:':
            stack.append(":true:");
        elif command == ':false:':
            stack.append(":false:");
        elif command == ':error:':
            stack.append(":error:");
        elif command == 'add':
            addn(stack.pop(),stack.pop());
        elif command == 'sub':
            subs(stack.pop(),stack.pop());
        elif command == 'mul':
            mult(stack.pop(),stack.pop());
        elif command == 'div':
            divs(stack.pop(),stack.pop());
        elif command == 'rem':
            remn(stack.pop(),stack.pop());
        elif command == 'neg':
            negv(stack.pop(),stack.pop());
        elif command == 'swap':
            swappy();
        elif command == 'quit':
            #write to output file & stop interpreter

    fin.close();
    fout.close();
