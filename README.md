     _____           _ _____
    | __  |___ ___ _| |   __|___ ___ ___ ___
    |    -| .'|   | . |__   | -_|   |_ -| -_|
    |__|__|__,|_|_|___|_____|___|_|_|___|___|


RandSense v 0.5
===================

RandSense is a random sentence generator. It uses a context-free grammar to construct grammatically correct yet often silly and nonsensical sentences.

RandSense is built using the [SimpleNLG](http://code.google.com/p/simplenlg/) default lexicon, with some of my own modifications.

Usage
===================

This version of RandSense is meant to be modular. If you want to use it, just move it into your project. The "Main" object is unnecessary and can be deleted, but it's in the repo to demonstrate the ease of getting a sentence and seeing its parts, as well as for testing while modifying the code.

Many thanks
===================

- [Antonio Zamora](http://www.scientificpsychic.com/az.html)
- [Adam Parrish](http://www.decontextualize.com/teaching/dwwp/topics-ii-recursion-and-context-free-grammars/)
- [SimpleNLG](http://code.google.com/p/simplenlg/)

To do list
===================

- Make weighted paths
- Add proper nouns
- Add negations
- Add commas, semicolons, colons
- Add more tenses
- Adjectives: comparative, superlative
- Make "Warriors engage" type constructions possible (right now the logic is in place only for "a warrior" or "the warriors")
- Determiners can have <singularorplural/> tag, so check for this and do a choice on the noun if so

Known bugs
===================

- Some questions are being "rendered" as statements
- 'else' is ending sentences sometimes