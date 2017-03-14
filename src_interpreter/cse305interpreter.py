import string;

def is_digit(n):
    try:
        int(n)
        return True
    except ValueError:
        return  False


def interpreter():#infil, outfil):
    fin = open('/home/anandi/Desktop/cse305interpreter/src_interpreter/in.txt', 'r');
    fout = open('/home/anandi/Desktop/cse305interpreter/src_interpreter/out.txt', 'w');

    stack = [];
    length = len(stack);

    for line in fin:
        command = line;

        if 'push' in command:
            num = line.split(' ')[1];
            if is_digit(num):
                stack.append(num);
            elif '"' in num:
                num = num.replace('"', '')
                stack.append(num);
            elif num.isalnum(): #not working for names
                stack.append(num);
            else:
                stack.append(':error:');
        elif 'pop' in command:
            if len(stack)!=0:
                stack.pop();
            else:
                stack.append(':error');
        elif ':true:' in command:
            stack.append(':true:');
        elif ':false:' in command:
            stack.append(':false:');
        elif ':error:' in command:
            stack.append(':error:');
        elif 'add' in command:
            if len(stack)!=0 and len(stack)!=1 and is_digit(stack[len(stack)-2]) and is_digit(stack[len(stack)-1]):
                stack.append(str( int(stack.pop()) + int(stack.pop()) ) + '\n');
            else:
                stack.append(':error:\n');
        elif 'sub' in command:
            if len(stack)!=0 and len(stack)!=1 and is_digit(stack[len(stack)-2]) and is_digit(stack[len(stack)-1]):
                stack.append(str( -int(stack.pop()) + int(stack.pop()) ) + '\n');
            else:
                stack.append(':error:\n');
        elif 'mul' in command:
            if len(stack)!=0 and len(stack)!=1 and is_digit(stack[len(stack)-2]) and is_digit(stack[len(stack)-1]):
                stack.append(str( int(stack.pop()) * int(stack.pop()) ) + '\n');
            else:
                stack.append(':error:\n');
        elif 'div' in command:
            if len(stack)!=0 and len(stack)!=1 and is_digit(stack[len(stack)-2]) and is_digit(stack[len(stack)-1]) and stack[len(stack)-1]!=0:
                op2 = stack.pop();
                op1 = stack.pop();
                stack.append(str( int(int(op1) / int(op2)) ) + '\n');
            else:
                stack.append(':error:\n');
        elif 'rem' in command:
            if len(stack)!=0 and len(stack)!=1 and is_digit(stack[len(stack)-2]) and is_digit(stack[len(stack)-1]) and stack[len(stack)-1]!=0:
                op2 = stack.pop();
                op1 = stack.pop();
                stack.append(str( int(int(op1) % int(op2)) ) + '\n');
            else:
                stack.append(':error:\n');
        elif 'neg' in command:
            if len(stack)!=0 and is_digit(stack[len(stack)-1]):
                stack.append(str( -int(stack.pop()) ) + '\n');
            else:
                stack.append(':error:\n');
        elif 'swap' in command:
            if len(stack)!=0 and len(stack)!=1:
                op1 = stack.pop();
                op2 = stack.pop();
                stack.append(op1);
                stack.append(op2);
            else:
                stack.append(':error:\n');
        elif 'quit' in command:
            while (len(stack)!=0):
                a = str(stack.pop());
                fout.write(a);

    fin.close();
    fout.close();
