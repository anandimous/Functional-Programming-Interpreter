import string;

def is_digit(n):
    try:
        int(n)
        return True
    except ValueError:
        return  False

def is_name(m):
    alphaList = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    if m[0] in alphaList:
        return True;
    else:
        return False;

def has_name_val(l, val_dict):
    if l in val_dict.keys():
        return True;
    else:
        return False;

def has_name_val_int(k, val_dict):
    if k in val_dict.keys() and is_digit(val_dict[k]):
        return True;
    else:
        return False;

def has_name_val_bool(j, val_dict):
    if j in val_dict.keys() and (val_dict[j]==':true:\n' or val_dict[j]==':false:\n'):
        return True;
    else:
        return False;

def interpreter(infil, outfil):
    fin = open(infil, 'r');
    fout = open(outfil, 'w');

    stack = [];
    namedict = {};
    dict_stack = [];
    dict_stack.append(namedict);
    global alpList;

    for command in fin:
        alpList = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

        #NAMEVAL CHANGES IMPLEMENTED!!
        if 'push' in command:
            line = command.split(' ', 1)[1];
            if is_digit(line):
                stack.append(line);
            elif line[0] in alpList:
                stack.append(line[0:len(line)-1] + '\n');
            elif line[0] is chr(45):
                if line[1] == '0':
                    stack.append('0\n');
                elif is_digit(line[1]) == True:
                    stack.append(line);
                else:
                    stack.append(':error:\n')
            elif '"' in line:
                line = line.replace('"', '')
                stack.append(line);
            else:
                stack.append(':error:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'pop' in command:
            if len(stack)!=0:
                stack.pop();
            else:
                stack.append(':error:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif ':true:' in command:
            stack.append(':true:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif ':false:' in command:
            stack.append(':false:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif ':error:' in command:
            stack.append(':error:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'add' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    stack.append(str( int(nameval[op1]) + int(nameval[op2]) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    stack.append(str( int(nameval[op1]) + int(op2) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    stack.append(str( int(op1) + int(nameval[op2]) ) + '\n');
                    dict_stack.append(nameval);
                else:
                    stack.append(str( int(op1) + int(op2) ) + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'sub' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    stack.append(str( int(nameval[op1]) - int(nameval[op2]) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    stack.append(str( int(nameval[op1]) - int(op2) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    stack.append(str( int(op1) - int(nameval[op2]) ) + '\n');
                    dict_stack.append(nameval);
                else:
                    stack.append(str( int(op1) - int(op2) ) + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'mul' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    stack.append(str( int(nameval[op1]) * int(nameval[op2]) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    stack.append(str( int(nameval[op1]) * int(op2) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    stack.append(str( int(op1) * int(nameval[op2]) ) + '\n');
                    dict_stack.append(nameval);
                else:
                    stack.append(str( int(op1) * int(op2) ) + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'div' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)) and (int(stack[len(stack)-1]) != 0 or int(nameval(stack[len(stack)-1]) != 0)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    stack.append(str( int(int(nameval[op1]) / int(nameval[op2])) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    stack.append(str( int(int(nameval[op1]) / int(op2)) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    stack.append(str( int(int(op1) / int(nameval[op2])) ) + '\n');
                    dict_stack.append(nameval);
                else:
                    stack.append(str( int(int(op1) / int(op2)) ) + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'rem' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)) and (int(stack[len(stack)-1]) != 0 or int(nameval(stack[len(stack)-1]) != 0)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    stack.append(str( int(int(nameval[op1]) % int(nameval[op2])) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    stack.append(str( int(int(nameval[op1]) % int(op2)) ) + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    stack.append(str( int(int(op1) % int(nameval[op2])) ) + '\n');
                    dict_stack.append(nameval);
                else:
                    stack.append(str( int(int(op1) % int(op2)) ) + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'neg' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)):
                op1 = stack.pop();
                if is_name(op1):
                    stack.append(str( -int(nameval[op1]) ) + '\n');
                    dict_stack.append(nameval);
                else:
                    stack.append(str( -int(op1) ) + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'swap' in command:
            if len(stack)!=0 and len(stack)!=1:
                op1 = stack.pop();
                op2 = stack.pop();
                stack.append(op1);
                stack.append(op2);
            else:
                stack.append(':error:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'show' in command:
            print(stack);

    #                            END OF PART 1                              #

        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'and' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (stack[len(stack)-2]==':true:\n' or stack[len(stack)-2]==':false:\n' or has_name_val_bool(stack[len(stack)-2],nameval)) and (stack[len(stack)-1]==':true:\n' or stack[len(stack)-1]==':false:\n' or has_name_val_bool(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    if nameval[op1]==':true:\n' and nameval[op2]==':true:\n':
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                elif is_name(op1):
                    if nameval[op1]==':true:\n' and op2==':true:\n':
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                elif is_name(op2):
                    if op1==':true:\n' and nameval[op2]==':true:\n':
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                else:
                    if op1==':true:\n' and op2==':true:\n':
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'or' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (stack[len(stack)-2]==':true:\n' or stack[len(stack)-2]==':false:\n' or has_name_val_bool(stack[len(stack)-2],nameval)) and (stack[len(stack)-1]==':true:\n' or stack[len(stack)-1]==':false:\n' or has_name_val_bool(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    if nameval[op1]==':false:\n' and nameval[op2]==':false:\n':
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                elif is_name(op1):
                    if nameval[op1]==':false:\n' and op2==':false:\n':
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                elif is_name(op2):
                    if op1==':false:\n' and nameval[op2]==':false:\n':
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                else:
                    if op1==':false:\n' and op2==':false:\n':
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'not' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and (stack[len(stack)-1]==':true:\n' or stack[len(stack)-1]==':false:\n' or has_name_val_bool(stack[len(stack)-1],nameval)):
                op1 = stack.pop();
                if is_name(op1):
                    if nameval[op1]==':true:\n':
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
                else:
                    if op1==':true:\n':
                        stack.append(':false:\n');
                        dict_stack.append(nameval);
                    else:
                        stack.append(':true:\n');
                        dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'equal' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    result = ':true:' if int(nameval[op1]) == int(nameval[op2]) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    result = ':true:' if int(nameval[op1]) == int(op2) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    result = ':true:' if int(op1) == int(nameval[op2]) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
                else:
                    result = ':true:' if int(op1) == int(op2) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'lessThan' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and (is_digit(stack[len(stack)-2]) or has_name_val_int(stack[len(stack)-2],nameval)) and (is_digit(stack[len(stack)-1]) or has_name_val_int(stack[len(stack)-1],nameval)):
                op2 = stack.pop();
                op1 = stack.pop();
                if is_name(op1) and is_name(op2):
                    result = ':true:' if int(nameval[op1]) < int(nameval[op2]) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
                elif is_name(op1):
                    result = ':true:' if int(nameval[op1]) < int(op2) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
                elif is_name(op2):
                    result = ':true:' if int(op1) < int(nameval[op2]) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
                else:
                    result = ':true:' if int(op1) < int(op2) else ':false:';
                    stack.append(result + '\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'bind' in command:
            if len(stack)!=0 and len(stack)!=1 and is_name(stack[len(stack)-2]):
                op1 = stack.pop();
                nameval = dict_stack.pop();
                if (is_digit(op1) or op1==':true:' or op1==':false:' or op1==':unit:' or isinstance(op1, str) or (is_name(op1) and has_name_val(op1, nameval))) and op1!=':error:\n':
                    op2 = stack.pop();
                    if is_name(op1) and has_name_val(op1, nameval):
                        nameval[op2] = nameval[op1];
                        stack.append(':unit:\n');
                        dict_stack.append(nameval);
                    else:
                        nameval[op2] = op1;
                        stack.append(':unit:\n');
                        dict_stack.append(nameval);
                else:
                    stack.append(op1);
                    stack.append(':error:\n');
                    dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'if' in command:
            nameval = dict_stack.pop();
            if len(stack)!=0 and len(stack)!=1 and len(stack)!=2 and (stack[len(stack)-3] == ':true:\n' or stack[len(stack)-3] == ':false:\n' or has_name_val_bool(stack[len(stack)-3],nameval)):
                op1 = stack.pop();
                op2 = stack.pop();
                op3 = stack.pop();
                if is_name(op3):
                    if nameval[op3]==':true:\n':
                        stack.append(op1);
                        dict_stack.append(nameval);
                    else:
                        stack.append(op2);
                        dict_stack.append(nameval);
                else:
                    if op3==':true:\n':
                        stack.append(op1);
                        dict_stack.append(nameval);
                    else:
                        stack.append(op2);
                        dict_stack.append(nameval);
            else:
                stack.append(':error:\n');
                dict_stack.append(nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'let' in command:
            if len(dict_stack)!=0:
                stack.append('$lstack$');
                local_nameval = {};
                local_nameval.update(dict_stack[len(dict_stack)-1]);
                dict_stack.append(local_nameval);
            else:
                stack.append('$lstack$');
                local_nameval = {};
                dict_stack.append(local_nameval);
        #NAMEVAL CHANGES IMPLEMENTED!!
        elif 'end' in command:
            op1 = stack.pop();
            if len(stack)!=1:
                while (stack[len(stack)-1]!='$lstack$'):
                    stack.pop();
                stack.pop();    #to remove $lstack$
                dict_stack.pop();   #to remove top level bind dictionary
                stack.append(op1);
            else:
                stack.pop();    #to remove $lstack$
                dict_stack.pop();   #to remove top level bind dictionary
                stack.append(op1);
        #NAMEVAL CHANGES IMPLEMENTED!
        elif 'quit' in command:
            while (len(stack)!=0):
                a = str(stack.pop());
                fout.write(a);

    fin.close();
    fout.close();
