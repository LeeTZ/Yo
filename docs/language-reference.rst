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

::

    # This is a single line comment

Multi-line comment starts with ``#(`` and ended with ``#)``

::

    #( This is a multiline
    comment #)


.. note:: Nested comments are not allowed in **Yo**.