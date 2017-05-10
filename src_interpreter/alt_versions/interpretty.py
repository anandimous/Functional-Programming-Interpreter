import copy
class Literal:
    def __init__(self,value,type):
        self.v = value
        self.t = type

class enviorment:
    def __init__(self,i,b,t):
        self.i = i
        self.b = b
        self.t = t
        self.children = []
        self.parent = None
        self.scopetrack = 0
        self.function = {}
        self.funParent = None
        self.parameter = None
        self.commandStack = list()
        self.execParent = None
        self.commandParent = list()
        self.currentCommand = list()
        self.returned = None
        self.name = None

    def add_child(self, node):
        self.children.append(copy.deepcopy(node))

    def pushInt(self,value):
        number = Literal(int(value),"int")
        self.i.append(number)
        self.scopetrack = self.scopetrack + 1

    def pushString(self,value):
        string = Literal(value,"string")
        self.i.append(string)
        self.scopetrack = self.scopetrack + 1


    def pushName(self,value):
        name = Literal(value,"name")
        self.i.append(name)
        self.scopetrack = self.scopetrack + 1

    def pushBool(self,value):
        Bool = Literal(value,"bool")
        self.i.append(Bool)
        self.scopetrack = self.scopetrack + 1

    def pushError(self):
        error = Literal(":error:","error")
        self.i.append(error)
        self.scopetrack = self.scopetrack + 1

    def pushUnit(self):
        unit = Literal(":unit:","unit")
        self.i.append(unit)
        self.scopetrack = self.scopetrack + 1

    def pushFunc(self,value):
        function = Literal(value,"funct")
        self.i.append(function)
        self.scopetrack = self.scopetrack + 1

    def pushBack(self,thing):
        self.i.append(thing)
        self.scopetrack = self.scopetrack + 1

    def pop(self):
        self.scopetrack = self.scopetrack - 1
        return self.i.pop()

    def outputList(self,outfile):
        for items in reversed(self.i):
            if items.t == "int":
                outfile.write(str(int(items.v)))
            else:
                outfile.write(str(items.v))
            outfile.write("\n")


def interpreter(input, output):
        inputfile = open(input, "r")
        global outputfile
        outputfile = open(output, "w")
        linereader = inputfile.readlines()
        inputstack = enviorment(list(),{},{})
        command = []
        for stuff in linereader:
            command.append(str(stuff))
        inputstack.currentCommand = command
        #mainMethod(inputstack,False,0)
        for items in reversed(mainMethod(inputstack,False,0,False,"Neither")):
            if items.t == "int":
                outputfile.write(str(int(items.v)))
            else:
                outputfile.write(str(items.v))
            outputfile.write("\n")
        for things in inputstack.i:
           print("value : " + str(things.v))
           print("type : " + things.t)
           if things.v in inputstack.b:
             print("name value : " + str(inputstack.b[things.v]))
             print("name type : " + str(inputstack.t[things.v]))
           print()

def mainMethod(inputstack,inFunc,scope,returned,inOut):

    if returned == True:
        inputstack.pushBack(inputstack.returned)

    inMethod = False
    command = []
    t = command
    y = list()
    inMethodScope = 0
    copyOfInputCommand = copy.deepcopy(inputstack.currentCommand)
    for lines in copyOfInputCommand:
        del inputstack.currentCommand[:-1]
        if lines.split(' ' , 1)[0] == "push" and inMethod == False:
            x = lines.split(' ',1)[1].strip()
            if x[0].isalpha() or isInt(x) or (x.startswith('"') and x.endswith('"')) or x in inputstack.b:
                if isInt(x):
                    #push int
                    inputstack.pushInt(int(x))
                if x.startswith('"') and x.endswith('"'):
                    #push string
                    inputstack.pushString(x[1:-1])
                if x[0].isalpha():
                    if x in inputstack.t:
                        if inputstack.t[x] == "funct":
                            inputstack.pushFunc(x)
                        else:
                            inputstack.pushName(x)
                    else:
                    #push name
                        inputstack.pushName(x)


            else:
                inputstack.pushError()
        if lines.strip() == "pop" and inMethod == False:
            if not inputstack:
                inputstack.pushError()
            else:
                inputstack.pop()
        if lines.strip() == ":false:" and inMethod == False:
            inputstack.pushBool(":false:")
        if lines.strip() == ":true:" and inMethod == False:
            inputstack.pushBool(":true:")
        if lines.strip() == ":error:" and inMethod == False:
            inputstack.pushError()


        #ADD
        if lines.strip() == "add" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int":
                    total = value1.v + value2.v
                    inputstack.pushInt(total)
                else:

                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")):
                            if value1.t == "name" and value2.t != "name":
                                if isinstance(inputstack.b[value1.v], int):
                                    total = value2.v + inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value2.t == "name" and value1.t != "name":
                                if isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v] + value1.v
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()

                            if value1.t == "name" and value2.t == "name":
                                if isinstance(inputstack.b[value1.v], int) and isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v] + inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()

        #Substract
        if lines.strip() == "sub" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int":
                    total = value2.v - value1.v
                    inputstack.pushInt(total)
                else:
                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")):
                            if value1.t == "name" and value2.t != "name":
                                if isinstance(inputstack.b[value1.v], int):
                                    total = value2.v-inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value2.t == "name" and value1.t != "name":
                                if isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v] - value1.v
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value1.t == "name" and value2.t == "name":
                                if isinstance(inputstack.b[value1.v], int) and isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v]-inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #MULTIPLY
        if lines.strip() == "mul" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int":
                    total = value1.v * value2.v
                    inputstack.pushInt(total)
                else:
                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")):
                            if value1.t == "name" and value2.t != "name":
                                if isinstance(inputstack.b[value1.v], int):
                                    total = value2.v * inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value2.t == "name" and value1.t != "name":
                                if isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v] * value1.v
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value1.t == "name" and value2.t == "name":
                                if isinstance(inputstack.b[value1.v], int) and isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v]*inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #DIVIDE
        if lines.strip() == "div" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int" and value1.v != 0:
                    total = value2.v/value1.v
                    div = Literal(total,"int")
                    inputstack.pushInt(total)
                else:
                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")) and value1.v != 0:
                            if value1.t == "name" and value2.t != "name":
                                if isinstance(inputstack.b[value1.v], int):
                                    total = value2.v/inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value2.t == "name" and value1.t != "name":
                                if isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v] / value1.v
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value1.t == "name" and value2.t == "name":
                                if isinstance(inputstack.b[value1.v], int) and isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v]/inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #REMAINDER
        if lines.strip() == "rem" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int" and value1.v != 0:
                    total = value2.v%value1.v
                    inputstack.pushInt(total)
                else:
                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")) and value1.v != 0:
                            if value1.t == "name" and value2.t != "name":
                                if isinstance(inputstack.b[value1.v], int):
                                    total = value2.v%inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value2.t == "name" and value1.t != "name":
                                if isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v] % value1.v
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()
                            if value1.t == "name" and value2.t == "name":
                                if isinstance(inputstack.b[value1.v], int) and isinstance(inputstack.b[value2.v], int):
                                    total = inputstack.b[value2.v]%inputstack.b[value1.v]
                                    inputstack.pushInt(total)
                                else:
                                    inputstack.pushBack(value2)
                                    inputstack.pushBack(value1)
                                    inputstack.pushError()

                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #NEGATIVE
        if lines.strip() == "neg" and inMethod == False:
            if len(inputstack.i) == 0:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                if value1.t == "int" or (value1.t == "name" and value1.v in inputstack.b):
                    if value1.v in inputstack.b:
                        if isinstance(inputstack.b[value1.v], int):
                            neg = inputstack.b[value1.v] * -1
                            inputstack.pushInt(neg)
                        else:
                            inputstack.pushBack(value1)
                            inputstack.pushError()
                    else:
                        neg = value1.v * -1
                        inputstack.pushInt(neg)
                else:
                    inputstack.pushBack(value1)
                    inputstack.pushError()
        #SWAP
        if lines.strip() == "swap" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                inputstack.pushBack(value1)
                inputstack.pushBack(value2)
        #QUIT
        if lines.strip() == "quit" and inMethod == False:
            return inputstack.i
        # AND
        if lines.strip() == "and" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "bool" and value2.t == "bool":
                    if value1.v == ":true:" and value2.v == ":true:":
                        inputstack.pushBool(":true:")
                    else:
                        inputstack.pushBool(":false:")
                else:
                    if (value1.t == "bool" or value1.v in inputstack.b) and (value2.t == "bool" or value2.v in inputstack.b):
                        if value1.t == "name" and value2.t != "name":
                            if inputstack.t[value1.v] == "bool":
                                if inputstack.b[value1.v] == ":true:" and value2.v == ":true:":
                                     inputstack.pushBool(":true:")
                                else:
                                     inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t != "name" and value2.t == "name":
                            if inputstack.t[value2.v] == "bool":
                                if inputstack.b[value2.v] == ":true:" and value1.v == ":true:":
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t == "name" and value2.t == "name":
                            if inputstack.t[value1.v] == "bool" and inputstack.t[value2.v] == "bool":
                                if inputstack.b[value2.v] == ":true:" and inputstack.b[value1.v] == ":true:":
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #OR
        if lines.strip() == "or" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "bool" and value2.t == "bool":
                    if value1.v == ":true:" or value2.v == ":true:":
                        inputstack.pushBool(":true:")
                    else:
                        inputstack.pushBool(":false:")
                else:
                    if (value1.t == "bool" or value1.v in inputstack.b) and (
                            value2.t == "bool" or value2.v in inputstack.b):
                        if value1.t == "name" and value2.t != "name":
                            if inputstack.t[value1.v] == "bool":
                                if inputstack.b[value1.v] == ":true:" or value2.v == ":true:":
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t != "name" and value2.t == "name":
                            if inputstack.t[value2.v] == "bool":
                                if inputstack.b[value2.v] == ":true:" or value1.v == ":true:":
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t == "name" and value2.t == "name":
                            if inputstack.t[value1.v] == "bool" and inputstack.t[value2.v] == "bool":
                                if inputstack.b[value2.v] == ":true:" or inputstack.b[value1.v] == ":true:":
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #NOT
        if lines.strip() == "not" and inMethod == False:
            if len(inputstack.i) == 0:
                inputstack.pushError()
            else:
                value = inputstack.pop()
                if value.t == "bool":
                    if value.v == ":true:":
                        inputstack.pushBool(":false:")
                    else:
                        inputstack.pushBool(":true:")
                else:
                    if value.v in inputstack.b:
                        if inputstack.t[value.v] == "bool":
                            if inputstack.b[value.v] == ":true:":
                                inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBool(":true:")
                        else:
                            inputstack.pushBack(value)
                            inputstack.pushError()
                    else:
                        inputstack.pushBack(value)
                        inputstack.pushError()
        #EQUAL
        if lines.strip() == "equal" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int":
                    if value1.v == value2.v:
                        inputstack.pushBool(":true:")
                    else:
                        inputstack.pushBool(":false:")
                else:
                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")):
                        if value1.t == "name" and value2.t != "name":
                            if inputstack.t[value1.v] == "int":
                                if inputstack.b[value1.v] == value2.v:
                                     inputstack.pushBool(":true:")
                                else:
                                     inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t != "name" and value2.t == "name":
                            if inputstack.t[value2.v] == "int":
                                if inputstack.b[value2.v] == value1.v:
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t == "name" and value2.t == "name":
                            if inputstack.t[value1.v] == "int" and inputstack.t[value2.v] == "int":
                                if inputstack.b[value2.v] == inputstack.b[value1.v]:
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()

        #LESS THAN
        if lines.strip() == "lessThan" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                value2 = inputstack.pop()
                if value1.t == "int" and value2.t == "int":
                    if value1.v > value2.v:
                        inputstack.pushBool(":true:")
                    else:
                        inputstack.pushBool(":false:")
                else:
                    if (value1.t == "int" or (value1.v in inputstack.b and value1.t == "name")) and (value2.t == "int" or (value2.v in inputstack.b and value2.t == "name")):
                        if value1.t == "name" and value2.t != "name":
                            if inputstack.t[value1.v] == "int":
                                if inputstack.b[value1.v] > value2.v:
                                     inputstack.pushBool(":true:")
                                else:
                                     inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t != "name" and value2.t == "name":
                            if inputstack.t[value2.v] == "int":
                                if inputstack.b[value2.v] < value1.v:
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                        if value1.t == "name" and value2.t == "name":
                            if inputstack.t[value1.v] == "int" and inputstack.t[value2.v] == "int":
                                if inputstack.b[value2.v] < inputstack.b[value1.v]:
                                    inputstack.pushBool(":true:")
                                else:
                                    inputstack.pushBool(":false:")
                            else:
                                inputstack.pushBack(value2)
                                inputstack.pushBack(value1)
                                inputstack.pushError()
                    else:
                        inputstack.pushBack(value2)
                        inputstack.pushBack(value1)
                        inputstack.pushError()
        #BIND
        if lines.strip() == "bind" and inMethod == False:
            if len(inputstack.i) <= 1:
                inputstack.pushError()
            else:
                value1 = inputstack.pop()
                valueName = inputstack.pop()
                if valueName.t == "name" and not value1.t == "error":
                    if value1.t == "name":
                        if value1.v in inputstack.b:
                            inputstack.pushUnit()
                            inputstack.b[valueName.v] = inputstack.b[value1.v]
                            inputstack.t[valueName.v] = inputstack.t[value1.v]
                        else:
                            inputstack.pushBack(valueName)
                            inputstack.pushBack(value1)
                            inputstack.pushError()
                    else:
                        unit = Literal(":unit:","unit")
                        inputstack.pushUnit()
                        inputstack.b[valueName.v] = value1.v
                        inputstack.t[valueName.v] = value1.t
                else:
                    inputstack.pushBack(valueName)
                    inputstack.pushBack(value1)
                    inputstack.pushError()

        #IF
        if lines.strip() == "if" and inMethod == False:
            if len(inputstack.i) < 3:
                inputstack.pushError()
            else:
                valuex = inputstack.pop()
                valuey = inputstack.pop()
                valueBool = inputstack.pop()
                if valueBool.t != "bool" and not valueBool.v in inputstack.t:
                    inputstack.pushBack(valueBool)
                    inputstack.pushBack(valuey)
                    inputstack.pushBack(valuex)
                    inputstack.pushError()
                else:
                    if valueBool.v in inputstack.t:
                        if inputstack.t[valueBool.v] == "bool":
                            if inputstack.b[valueBool.v] == ":true:":
                                inputstack.pushBack(valuex)
                            else:
                                inputstack.pushBack(valuey)
                        else:
                            inputstack.pushBack(valueBool)
                            inputstack.pushBack(valuey)
                            inputstack.pushBack(valuex)
                            inputstack.pushError()
                    else:
                        if valueBool.v == ":true:":
                            inputstack.pushBack(valuex)
                        else:
                            inputstack.pushBack(valuey)
        #let end
        if lines.strip() == "let" and inMethod == False:
            inputstack.parent = copy.deepcopy(inputstack)
            inputstack.add_child(inputstack)
            inputstack = inputstack.children[0]



        if lines.strip() == "end" and inMethod == False:
            if len(inputstack.i) >= len(inputstack.parent.i):
                x = inputstack.pop()
                inputstack = inputstack.parent
                inputstack.pushBack(x)
            else:
                inputstack = inputstack.parent



        #creates command stack
        if inMethod == True:
            inputstack.commandStack.append(lines)

        #funtion starts
        if (lines.split(' ',2)[0] == "fun" or lines.split(' ',2)[0] == "inOutFun")  and lines.split(' ',2)[1][0].isalpha() and lines.split(' ',2)[2][0].isalpha() and lines.split(' ',2)[1] != lines.split(' ',2)[2] and lines.split(' ',2)[1]:
            if inMethod == False and inMethodScope == 0:
                functionName = lines.split(' ',2)[1]
                functionParam = lines.split(' ',2)[2]
                inputstack.b[functionName] = copy.deepcopy(inputstack)
                if lines.split(' ',2)[0] == "fun":
                    inputstack.t[functionName] = "funct"
                else:
                    inputstack.t[functionName] = "inOutFun"
                a = inputstack
                inputstack = inputstack.b[functionName]
                inputstack.funParent = a
                inputstack.parameter = functionParam
                inMethod = True
            else:
                inMethodScope += 1


        #funends
        if lines.strip() == "funEnd":
            if inMethod == True:
                if inMethodScope == 0:
                    inputstack = inputstack.funParent
                    inputstack.pushUnit()
                    inMethod = False
                else:
                    inMethodScope = inMethodScope - 1
            else:
                if inFunc == True and inputstack.execParent.returned == None:
                    mainMethod(inputstack.execParent,False,scope - 1,False, "Neither")
                    inputstack.execParent.b[inputstack.name] = inputstack.b[inputstack.parameter.strip()]
                    inputstack.execParent.t[inputstack.name] = inputstack.t[inputstack.parameter.strip()]
                if inFunc == True and inputstack.execParent.returned != None:
                    mainMethod(inputstack.execParent,False,scope -1, True, "Neither")





        #funCall
        if lines.strip() == "call" and inMethod == False:
            if len(inputstack.i) < 2:
                inputstack.pushError()
            else:
                funName = inputstack.pop()
                funArg = inputstack.pop()
                if funName.v in inputstack.t:
                    if inputstack.t[funName.v] != "funct" and inputstack.t[funName.v] != "inOutFun" or funArg.t == "error":
                        inputstack.pushBack(funArg)
                        inputstack.pushBack(funName)
                        inputstack.pushError()
                    else:
                        if inputstack.t[funName.v] == "funct":
                            funStack = inputstack.b[funName.v]
                            if funArg.t == "name" or funArg.t == "funct":
                                if not funArg.v in inputstack.b:
                                    inputstack.pushBack(funArg)
                                    inputstack.pushBack(funName)
                                    inputstack.pushError()
                                else:
                                    funStack.b[funStack.parameter.strip()] = inputstack.b[funArg.v]
                                    funStack.t[funStack.parameter.strip()] = inputstack.t[funArg.v]
                            else:
                                funStack.b[funStack.parameter.strip()] = funArg.v
                                funStack.t[funStack.parameter.strip()] = funArg.t
                            funEnvo = copy.deepcopy(funStack)
                            funEnvo.currentCommand = funEnvo.commandStack
                            funEnvo.execParent = inputstack
                            funEnvo.commandParent = inputstack.currentCommand
                            funEnvo.execParent.currentCommand = inputstack.currentCommand
                            mainMethod(funEnvo,True,scope - 1,False,"fun")

                        if inputstack.t[funName.v] == "inOutFun":
                            funStack = inputstack.b[funName.v]
                            if funArg.t == "name":
                                if not funArg.v in inputstack.b:
                                    inputstack.pushBack(funArg)
                                    inputstack.pushBack(funName)
                                    inputstack.pushError()
                                else:
                                    funStack.name = funArg.v
                                    funStack.b[funStack.parameter.strip()] = inputstack.b[funArg.v]
                                    funStack.t[funStack.parameter.strip()] = inputstack.t[funArg.v]
                            else:
                                funStack.b[funStack.parameter.strip()] = funArg.v
                                funStack.t[funStack.parameter.strip()] = funArg.t
                            funEnvo = copy.deepcopy(funStack)
                            funEnvo.currentCommand = funEnvo.commandStack
                            funEnvo.execParent = inputstack
                            funEnvo.commandParent = inputstack.currentCommand
                            funEnvo.execParent.currentCommand = inputstack.currentCommand
                            mainMethod(funEnvo,True,scope - 1,False,"inOut")


        if lines.strip() == "return" and inFunc == True:
            if inOut == "fun":
                p = inputstack.i.pop()
                if p.t == "name" and p.v in inputstack.b:
                    d = Literal(inputstack.b[p.v],inputstack.t[p.v])
                    inputstack.execParent.returned = d
                else:
                    if p.t == "funct":
                        d = Literal(p.v,p.t)
                        inputstack.execParent.returned = d

                    else:
                        d = Literal(p.v,p.t)
                        inputstack.execParent.returned = d
            else:
                p = inputstack.i.pop()
                if p.t == "name" and p.v in inputstack.b:
                    d = Literal(inputstack.b[p.v],p.t)
                    inputstack.execParent.returned = d
                    inputstack.execParent.b[p.v] = inputstack.b[p.v]
                    inputstack.execParent.t[p.v] = inputstack.t[p.v]
                    inputstack.execParent.b[inputstack.name] = inputstack.b[p.v]
                    inputstack.execParent.t[inputstack.name] = inputstack.b[p.v]
                else:
                    inputstack.execParent.returned = p
                    inputstack.execParent.b[inputstack.name] = inputstack.b[inputstack.parameter.strip()]
                    inputstack.execParent.t[inputstack.name] = inputstack.t[inputstack.parameter.strip()]





def hasNumbers(inputString):
    return any(char.isdigit() for char in inputString)

def isInt(s):
    if s[0] == '-':
        return s[1:].isdigit()
    return s.isdigit()


# interpreter("/home/anandi/Desktop/cse305interpreter/src_interpreter/in.txt","/home/anandi/Desktop/cse305interpreter/src_interpreter/out.txt");
