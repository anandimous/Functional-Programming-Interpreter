#    create list of all chars, remove char from list if encountered in the input string
#    check if list of chars is empty, if yes, print true, else false
import string;

def hw1(input, output):
    fi = open(input, 'r');
    fo = open(output, 'w');

    char_list = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];
    line_list = [];

    for line in fi:
        for chars in line:
            line_list.append(chars);
            if chars in char_list:
                char_list.remove(chars);
        if(len(char_list)) == 0:
            fo.write("true");
            fo.write("\n");
            char_list=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];
        else:
            fo.write("false");
            fo.write("\n");
            char_list=['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'];

    fi.close();
    fo.close();
