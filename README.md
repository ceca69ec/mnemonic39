mnemonic39
==========

**Implementation of [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki) in rust for use on command line interface.**

## Disclaimer

* **Don't trust, verify**
 - Compare the results of this tool with others. Verify the implementation (and the tests). **Use at your won risk.**

## Features

* **Generate a list of valid last words**
 - Insert a mnemonic lacking the last word and a list of possible valid last words (that fulfill the checksum) is showed.
* **Generate extended keys**
 - Insert entropy or mnemonic to show extended root keys.
* **Generate mnemonics**
 - Insert entropy and optional passphrase to generate mnemonic in any of the languages supported in bip-0039.
* **Generate multiple mnemonics based on a list of valid words**
 - Insert 12, 15, 18, 21 or 24 valid words in any supported language and flag `-g` to show multiple mnemonics with valid checksum.
* **Generate seed**
 - When hexadecimal entropy is inserted the tool show the seed to.
* **Passphrase**
 - Optional passphrase can be used with the entropy or mnemonic for additional protection.
* **Suggestions**
 - Show suggestion of a valid mnemonic word if one invalid is inserted.
* **Support all languages specified on [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039-wordlists.md)**
 - Chinese (Simplified)
 - Chinese (Traditional)
 - Czech
 - English
 - French
 - Italian
 - Japanese
 - Korean
 - Portuguese
 - Spanish
* **Transposition**
 - Insert mnemonic and target language flag to result in words in the same 'position' on the new word-list.

## What this tool *don't* do
* **Generate entropy**
 - Pseudo-random generators are not used. You have to insert entropy, if it's the case (see more bellow).
* **[Derivation](https://github.com/ceca69ec/derivation32)**
 - This tool do not generate any type of address.

## Suggestion for generating entropy

* **Disclaimer**
 - This is just one (and maybe not the best one) of various methods that can be used to generate random numbers. Again: use at your won risk.
* **What you need**
 - A `d20` (a dice with 20 faces, very common on rpg).
* **Values**
 - **1-9**: `use the value`.
 - **10**: `a`
 - **11**: `b`
 - **12**: `c`
 - **13**: `d`
 - **14**: `e`
 - **15**: `f`
 - **16**: `0 (zero)`
 - **17-20**: `throw again`
* **Method**
 - Throw the dice and type the value as described above.
 - Repeat the process until you have enough hexadecimal numbers (`32` is the minimum and result in a mnemonic with `12` words).
 - Use those 32 (or 40, 48, 56, 64) numbers in the this tool and it will generate the seed, mnemonic and extended root keys (you can insert optional passphrase too).
 - Always use a live Linux-gnu distribution loaded to the ram (*toram* on Debian and *copytoram* on Arch) with all hard drives and any type of internet connection disabled. **Don't keep the entropy in any form.**
