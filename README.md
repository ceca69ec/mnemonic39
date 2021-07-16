mnemonic39
==========

**Implementation of [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
 in rust for use on command line interface.**

## Disclaimer

* **Don't trust, verify**

    Compare the results of this tool with others. Verify the implementation (and the tests).

    **Use at your won risk.**

## Examples

```console
$ mnemonic39 06e9375a0b76cb5de997f93d6c707508 -p "バンドメイド"
alone endless story black hole puzzle play you dice glory bubble awkward
016b1146bb155c7c4ad0de6172bd4b7b6e65a487411c015c19512b8182e98e42fba7cc9da1c7631445fbfaea70057243c52d337c2d7be51786f5023086ae1bf7
xprv9s21ZrQH143K2pFLbGtCAETJEGS9egv9K1AhjHaACu22pEp5CUoSgFCwHYTfUE3aPKPgwccAJqneZoHX1J6iRvkkbxTuYdNhGSUHqfoWzDy
yprvABrGsX5C9jant7STRdfpNKYoQEabbJueE7gvWgU3auPusLdJT8y1JJs5JkRFU8hVnxWVh6CimW9CT5u5izWjEASMUJAL8YCBYAXwEGcYVem
zprvAWgYBBk7JR8GjQdaFzTSaQeJaCj3Xvu99ED9J5MvxumnvSSXho8ZvNXDKxNqU3MRCbdJSZoHEAVkLNWeSgvk2Q7xLdrkiT1fotbacrsgQox
```

```console
$ mnemonic39 alone endless story black hole puzzle play you dice glory bubble awkward -p "バンドメイド"
06e9375a0b76cb5de997f93d6c707508
016b1146bb155c7c4ad0de6172bd4b7b6e65a487411c015c19512b8182e98e42fba7cc9da1c7631445fbfaea70057243c52d337c2d7be51786f5023086ae1bf7
xprv9s21ZrQH143K2pFLbGtCAETJEGS9egv9K1AhjHaACu22pEp5CUoSgFCwHYTfUE3aPKPgwccAJqneZoHX1J6iRvkkbxTuYdNhGSUHqfoWzDy
yprvABrGsX5C9jant7STRdfpNKYoQEabbJueE7gvWgU3auPusLdJT8y1JJs5JkRFU8hVnxWVh6CimW9CT5u5izWjEASMUJAL8YCBYAXwEGcYVem
zprvAWgYBBk7JR8GjQdaFzTSaQeJaCj3Xvu99ED9J5MvxumnvSSXho8ZvNXDKxNqU3MRCbdJSZoHEAVkLNWeSgvk2Q7xLdrkiT1fotbacrsgQox
```

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

## Help

```bash
mnemonic39 1.0.2
Enter optional language and hexadecimal entropy with optional passphrase or
enter a list of mnemonic words with optional passphrase to see information
about it. It can show a list of valid last words to fulfil a list of mnemonic
words missing the last one. Generate mnemonic using a list of valid words.

USAGE:
    mnemonic39 [FLAGS] [OPTIONS] <DATA>...

FLAGS:
    -c, --chinese        Mnemonic with chinese simplified words
    -e, --english        Mnemonic with english words
    -f, --french         Mnemonic with french words
    -g, --generate       Generate valid mnemonics permuting words
    -h, --help           Prints help information
    -i, --italian        Mnemonic with italian words
    -j, --japanese       Mnemonic with japanese words
    -k, --korean         Mnemonic with korean words
    -o, --portuguese     Mnemonic with portuguese words
    -s, --spanish        Mnemonic with spanish words
    -t, --traditional    Mnemonic with chinese traditional words
    -V, --version        Prints version information
    -z, --czech          Mnemonic with czech words

OPTIONS:
    -p <passphrase>        optional passphrase to be used with the mnemonic

ARGS:
    <DATA>...    hexadecimal entropy, seed or a list of mnemonic words
```

## What this tool *don't* do
* **Generate entropy**

    Pseudo-random generators are not used. You have to insert entropy, if it's the case.

* **[Derivation](https://crates.io/crates/derivation32)**

    This tool do not generate any type of address.
