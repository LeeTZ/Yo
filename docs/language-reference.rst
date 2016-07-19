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

 ``Identifier ::= [**a**-**zA**-**Z_**][**a**-**zA**-**Z0**-**9_**]{0,255}``

.. tip:: _number _number1 number2 number_3 Number

.. error:: 2num *num func $2 Int Double Bool

.. note:: ``Int``, ``Double``, ``Bool`` are illegal because they are keywords. A list of keyword can be found in **reserved words**.


Reserved Words
~~~~~~~~~~~~~~
This is a list of reserved words in **Yo**. Since they are used by the language, these words are not available for naming variable or functions. The reserved words are consistent of keywords, built-in-type words and special constants.

