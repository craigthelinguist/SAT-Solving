


import sys

def num2var(x):
    return "v{}".format(x)

def disjunction(clause):
    if len(clause) == 1:
        return clause[0]
    return ['+', clause[0], disjunction(clause[1:])]

def conjunction(clause):
    if len(clause) == 1:
        return clause[0]
    return ['*', clause[0], disjunction(clause[1:])]


def main():
    fname = sys.argv[1]
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

if __name__ == "__main__":
    main()
