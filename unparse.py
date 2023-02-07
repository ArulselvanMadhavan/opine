import ast

if __name__ == "__main__":
    with open("test.py", "r") as source:
        tree = ast.parse(source.read())
        print(ast.unparse(tree))
        #ast.print(tree)
