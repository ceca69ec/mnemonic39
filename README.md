mnemonic39
==========

**Implementation of [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
 in rust for use on command line interface.**

## Disclaimer

* **Don't trust, verify**

    Compare the results of this tool with others. Verify the implementation (and the tests).

    **Use at your won risk.**

## Features

* **Generate a list of valid last words**

    Insert a mnemonic missing the last word and a list of possible valid last words (that
 fulfill the checksum) is showed.

* **Generate extended keys**

    Insert entropy or mnemonic to show extended root keys.

* **Generate mnemonics**

    Insert entropy and optional passphrase to generate a mnemonic in any of the languages
 supported in bip-0039.
* **Generate multiple mnemonics based on a list of valid words**

    Insert 12, 15, 18, 21 or 24 valid words in any supported language and flag `-g` to show
 multiple mnemonics with valid checksum.

* **Generate seed**

    When hexadecimal entropy is inserted the tool show the seed to.

* **Passphrase**

    Optional passphrase can be used with the entropy or mnemonic for additional protection.

* **Suggestions**

    Show suggestion of a valid mnemonic word if one invalid is inserted.

* **Support all languages specified on
 [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039/bip-0039-wordlists.md)**

    - [Chinese (Simplified)](https://github.com/bitcoin/bips/blob/master/bip-0039/chinese_simplified.txt)
    - [Chinese (Traditional)](https://github.com/bitcoin/bips/blob/master/bip-0039/chinese_traditional.txt)
    - [Czech](https://github.com/bitcoin/bips/blob/master/bip-0039/czech.txt)
    - [English](https://github.com/bitcoin/bips/blob/master/bip-0039/english.txt)
    - [French](https://github.com/bitcoin/bips/blob/master/bip-0039/french.txt)
    - [Italian](https://github.com/bitcoin/bips/blob/master/bip-0039/italian.txt)
    - [Japanese](https://github.com/bitcoin/bips/blob/master/bip-0039/japanese.txt)
    - [Korean](https://github.com/bitcoin/bips/blob/master/bip-0039/korean.txt)
    - [Portuguese](https://github.com/bitcoin/bips/blob/master/bip-0039/portuguese.txt)
    - [Spanish](https://github.com/bitcoin/bips/blob/master/bip-0039/spanish.txt)

* **Transposition**

    Insert mnemonic and target language flag to result in words in the same 'position' on the
 new word-list.

## What this tool *don't* do
* **Generate entropy**

    Pseudo-random generators are not used. You have to insert entropy, if it's the case.

* **[Derivation](https://crates.io/crates/derivation32)**

    This tool do not generate any type of address.
