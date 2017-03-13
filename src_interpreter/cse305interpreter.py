import string;

def is_digit(n):
    try:
        int(n)
        return True
    except ValueError:
        return  False


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

    global stack = [];
    global length = len(stack);

    for line in fin:
        command = line.split(' ')[0];
        num = line.split(' ')[1];

        if command == 'push':
            if is_digit(num):
                stack.append(num);
            elif '"' in num:
                stack.append(num[1:len(num)-1]);
            elif num.isalphanum():
                stack.append(num);
            else:
                stack.append(':error:');
        elif command == 'pop':
            if length!=0:
                stack.pop();
            else:
                stack.append(':error');
        elif command == ':true:':
            stack.append(':true:');
        elif command == ':false:':
            stack.append(':false:');
        elif command == ':error:':
            stack.append(':error:');
        elif command == 'add':
            if length!=0 and length!=1 and stack[length-1].isdigit() and stack[length-2].isdigit():
                stack.append(int(stack.pop()) + int(stack.pop()));
            else:
                stack.append(':error:');
        elif command == 'sub':
            if length!=0 and length!=1 and stack[length-1].isdigit() and stack[length-2].isdigit():
                stack.append(-int(stack.pop()) + int(stack.pop()));
            else:
                stack.append(':error:');
        #elif command == 'mul':
        #    mult(stack.pop(),stack.pop());
        #elif command == 'div':
        #    divs(stack.pop(),stack.pop());
        #elif command == 'rem':
        #    remn(stack.pop(),stack.pop());
        #elif command == 'neg':
        #    negv(stack.pop(),stack.pop());
        #elif command == 'swap':
        #    swappy();
        elif command == 'show':
            print(stack);
        elif command == 'quit':
            fout.write(stack);

    fin.close();
    fout.close();
