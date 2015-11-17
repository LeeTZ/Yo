#! /usr/bin/python
import os
import re
import sys
try:
  from cStringIO import StringIO
except:
  from StringIO import StringIO

def process(input_file):
  invalidchar = ('\t','{','}')
  blockcomment = ['#(',')#']

  stack = [0]
  output = StringIO()
  newindent = False
  commented = False
  linejoin  = False

  for i, line in enumerate(input_file):
    lineout = remove_inline(line)

    if lineout:
      for x in invalidchar:
        if x in lineout:
          error("SyntaxError: Invalid character {} found on line {}".format(x,i))          

      # Check if first statement is a block comment
      lstripline = lineout.lstrip()

      if len(lstripline) > 1 and blockcomment[0] == lstripline[:2]:
        commented = True

      # Checks if line gets uncommented
      if commented:
          if len(lineout) > 1 and blockcomment[1] == lineout[-2:]:
            commented = False
      else:

        if not linejoin:
          wcount  = len(lineout) - len(lineout.lstrip(' '))

          # If the previous line began an indentation, add the new indentation level to the block (so long as the new indentation
          # level is greater than the previous one)
          if newindent == True:
            if wcount > stack[-1]:
              stack.append(wcount)
              newindent = False
            else:
              error("IndentationError on line {}".format(i))

          # If the indentation level is greater than expected, throw an error
          if wcount > stack[-1]:     
            error("IndentationError on line {}".format(i))

          else:

            # If the indentation level is less than the current level, return to a previous indentation block. Throw an error if you return to an indentation level that doesn't exist
            while(wcount < stack[-1]):
              lineout = "}\n" + lineout
              stack.pop()

            if wcount != stack[-1]:
              error("IndentationError on line {}".format(i))

        # Given that the indentation level is correct, check for the start of a new code block (where a line ends with a ':') and insert a '{'. At the end of a line, add a semicolon ';' unless if there is a linejoin character '\'.
        if lineout[-1] == ':':
          lineout = lineout + '{\n'
          newindent = True

        elif lineout[-1] == '\\':
          linejoin = True
          lineout = lineout[:-1]

        else:
          lineout = lineout + ';\n'
          linejoin = False
        
        output.write(lineout)

  while 0 < stack[-1]:
    output.write("}\n")
    stack.pop()

  return output

def error(msg):
  sys.stderr.write(msg+"\n")
  sys.exit(2)

def remove_inline(line):
  if "##" in line:
    regex = re.compile("^(.*?)#.*|.*")
    m = regex.match(line)
    comments_removed = m.group(1)
  else:
    comments_removed = line
  return comments_removed.rstrip()

def usage():
  print"""
  python preprocessor.py [input.yo]
  """

if __name__ == "__main__":
  
  if len(sys.argv) != 2:
    usage()
    sys.exit(2)

  try:
    f_in = open(sys.argv[1],"r")
  except IOError:
    error("IOError: Cannot read input file %s.\n" % sys.argv[1])

  name_ext = os.path.basename(f_in.name)
  dir_ext = os.path.dirname(f_in.name)+"/"

  if name_ext.lower().endswith(".yo"):
    fname = os.path.splitext(name_ext)[0]
  else:
    error('NameError: Input must have yo file extension')

  out_str = process(f_in)

  f_out = open(dir_ext+ "intermediate/" + fname+".yo", 'w')
  f_out.write(out_str.getvalue())
