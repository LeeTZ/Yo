Language Reference Manual
========================

Syntax Notations
-----------------

In this section, we define types or identifiers in regular expression. The following notations are used in this document to show lexical and syntactic rules.

* **Dash** ``-`` is a shorthand for writing continuous elements. 
* **Brackets** ``[ ]`` enclose optional items and select exact one of them. If there is a caret ``^`` in the ``[ ^ ]``, it selects exact one of character that not belongs to the following list, for example  ``[ ^a-z]`` means any character other than a to z.
* **Parenthesis** ``( )`` enclose alternate item choices, which are separated from each other by vertical bars ``|``.
* **Asterisks** ``*`` indicate items to be repeated zero or more times.
* **Question mark** ``?`` is a sign of option.
* **Double colon with an equal sign** ``::=`` is used for definition.
* **Braces** ``{n}``  matches when the preceding character, or character range, occurs **n** times exactly.
* ``{n,m}`` matches when the preceding character occurs at least n times but not more than m times, for example, ``ba{2,3}b`` will match ``baab`` and ``baaab`` but not ``bab`` or ``baaaab``. Values are enclosed in braces.

Below we will write **Yo** formal syntax definition in code box. The terminals are marked in bold while non-terminals are in regular font.


Lexical Conventions
--------------------
This chapter presents the lexical conventions of **Yo**. This section describes which tokens are valid, including the naming convention of identifiers, reserved keywords, operators, separators and whitespaces.


Comments
~~~~~~~~~
Single line comment is made with a leading ``#`` in the line:

.. code-block:: none

    # This is a single line comment

Multi-line comment starts with ``#(`` and ended with ``#)``

.. code-block:: none

    #( This is a multiline
    comment #)


.. note:: Nested comments are not allowed in **Yo**.



Identifiers
~~~~~~~~~~~~
An identifier of **Yo** is a case-sensitive string different from any reserved words (see next chapter). It starts with a letter or an underscore, optionally followed by a series of characters (letter, underscorce, number). The length varies from 1 to 256.

Formally, an identifier can be any non-reserved word expressed in regular expression as:

**Identifier** ::= [**a**-**zA**-**Z_**][**a**-**zA**-**Z0**-**9_**]{0,255}

.. tip:: _number _number1 number2 number_3 Number

.. error:: 2num *num func $2 Int Double Bool

.. note:: ``Int``, ``Double``, ``Bool`` are illegal because they are keywords. A list of keyword can be found in **reserved words**.


Reserved Words
~~~~~~~~~~~~~~
This is a list of reserved words in **Yo**. Since they are used by the language, these words are not available for naming variable or functions. The reserved words are consistent of keywords, built-in-type words and special constants.

* Keywords: ``break``, ``continue``, ``for``, ``while``, ``if``, ``else``, ``eval``, ``func``, ``global``, ``in``, ``struct``, ``return``.
* Built-in types: ``Bool``, ``Int``, ``Double``, ``log``.
* Constants: ``true``, ``false``.


Operators
~~~~~~~~~
An operator is a special token that performs an operation, such as addition or subtraction, on either one, two, or three operands. A full coverage of operators can be found in a later chapter, See chapter `Expression and Operators <Expression and Operators>`__


Separators
~~~~~~~~~~
A separator separates tokens. White space (see next section) is a separator, but it is not a token. The other separators are all single-character tokens themselves: 
``( )``,  ``[ ]``,  ``,``.


New Line
~~~~~~~~
A physical line ends with an explicit ``\n`` input from the user while a logical line contains a complete statement. A logical line can be consist of multiple physical lines, all except the last one ending with an explicit ``\``.

.. code-block:: none

	line 1 \
  		line 1 continued \
	line 1 last line


Whitespace
~~~~~~~~~~
Whitespace characters such as tab and space are generally used to separate tokens. But **Yo** is not a free-format language, which means in some cases, the position and number of whitespaces matters to the code interpretation. Leading tab whitespace is used to denote code blocks and to compute the code hierarchy (similar to curly brackets in C-family languages). Briefly, an extra leading tab lowers the level of this line in the code hierarchy.

In contrast to **Python**, **Yo** only accepts tabs ``\t`` for leading indent, and space is not allowed. In other words, space should not appear at the beginning of any line (except for a continuing physical line where all the leading whitespaces are ignored).

.. code-block:: none

	im_a_parent
		im_a_child
			im_a_grandchild
		im_another_child
			im_a_grandchild

Usually, ``for``,``while``,``if``,``else`` and function definition may start a new code block. The code block ends with an un-indent. In the above example ``im_a_child`` and ``im_another_child`` are at the same code indention level.


Types
-------

**Yo** is a statically and strongly typed programming language, which means the type for each variable, expression or function is determined at compile time and remain unchanged throughout the program.

**Yo** has an object-oriented model in which every value is an object and each operation is a method call. We have a pure and uniform object model in the sense that the traditional primitive values (integers, double-precision floating numbers) and functions are incorporated into the object model.

we will show the definition of type and list the built-in types which can be used as building blocks for the user-defined types. ``Type`` in **Yo** is a blueprint for objects, which resembles the concept of ``class`` in other languages such as C++, Java and Python. 

.. note:: The concept of ``type`` in **Yo** resembles the ``class`` in other languages such as C++, Java and Python, which serves as the blueprint for objects. There are three kinds of types: value types, function types and the ``None`` type. For the sake of definition, we will mention function in this section, but the details will be covered in later sections.

In this section, we first list some built-in types as an introduction to our type system. Then we give the formal definition of type and show how users define types in their program.


Built-in Types
~~~~~~~~~~~~~~
Below we list the built-in types in **Yo**. As they are used as the building blocks for the program, **Yo** provides **literals** to initialize them conveniently in users' source code. The operators on this types are covered in next section.

