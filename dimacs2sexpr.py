


import sys, os

# Python likes to bottom out on .dimacs files so let's increase this a bit.
sys.setrecursionlimit(1000000000)

def num2var(x):
    # negative literal
    if x.startswith("-"):
        return ["~", "v{}".format(x[1:])]
    else:
        return "v{}".format(x)

def disjunction(clause):
    if len(clause) == 1:
        return clause[0]
    stack = []
    for i, cl in enumerate(clause):
        if i == len(clause) - 1:
            formula = cl
        else:
            stack.append(cl)
    while len(stack) > 0:
        cl = stack.pop()
        formula = ["+", cl, formula]
    return formula


def conjunction(clause):
    if len(clause) == 1:
        return clause[0]
    stack = []
    for i, cl in enumerate(clause):
        if i == len(clause) - 1:
            formula = cl
        else:
            stack.append(cl)
    while len(stack) > 0:
        cl = stack.pop()
        formula = ["*", cl, formula]
    return formula


def process(fname):
    if fname.endswith(".dimacs"):
        fname = fname[:-7]
    with open(fname + ".dimacs", "rb") as f:
        first = f.readline()
        first = first.split(" ")
        num_vars = first[2]
        num_clauses = first[3]
        clauses = []
 
        for i, line in enumerate(f):
            if len(line.lstrip().rstrip()) == 0: continue
            line = line.split(" ")
            if line[0] == 'c': continue # comment
            line = line[:-1] # get rid of 0 terminator
            clause = [num2var(x) for x in line]
            clauses.append(clause)

    disjuncts = [disjunction(cl) for cl in clauses]
    conjunct = conjunction(disjuncts)

    with open(fname + ".sexpr", "wb") as f:
        f.write(str(conjunct))
        f.write(".")

def main():
    if len(sys.argv) == 2:
        process(sys.argv[1])
        print "Processed {}".format(sys.argv[1])
    elif len(sys.argv) == 1:
        files = [i for i in os.listdir(os.getcwd()) if i.endswith(".dimacs")]
        for f in files:
            process(f)
    else:
        print '''usage: Python dimacs2sexpr.py for all files in directory.
                        Python dimacs2sexpr.py FILENAME for a single file.'''
        

if __name__ == "__main__":
    main()
