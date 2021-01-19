''' compile with:
 cd sacsnowuh
 f2py3 -c -m main *.f90 *.f
in the sacsnowuh directory
'''
import sacsnowuh.main as model

def print_doc():
    # Use a breakpoint in the code line below to debug your script.
    print(model.sacsnow.__doc__)


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    print_doc()