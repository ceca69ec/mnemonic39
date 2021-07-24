//! **Implementation of [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
//!  in rust for use on command line interface.**
//!
//! ## Disclaimer
//!
//! * **Don't trust, verify**
//!
//!     Compare the results of this tool with others. Verify the implementation (and the tests).
//!
//!     **Use at your won risk.**
//!
//! ## Examples
//!
//! ```console
//! $ mnemonic39 06e9375a0b76cb5de997f93d6c707508 -p "バンドメイド"
//! alone endless story black hole puzzle play you dice glory bubble awkward
//! 016b1146bb155c7c4ad0de6172bd4b7b6e65a487411c015c19512b8182e98e42fba7cc9da1c7631445fbfaea70057243c52d337c2d7be51786f5023086ae1bf7
//! xprv9s21ZrQH143K2pFLbGtCAETJEGS9egv9K1AhjHaACu22pEp5CUoSgFCwHYTfUE3aPKPgwccAJqneZoHX1J6iRvkkbxTuYdNhGSUHqfoWzDy
//! yprvABrGsX5C9jant7STRdfpNKYoQEabbJueE7gvWgU3auPusLdJT8y1JJs5JkRFU8hVnxWVh6CimW9CT5u5izWjEASMUJAL8YCBYAXwEGcYVem
//! zprvAWgYBBk7JR8GjQdaFzTSaQeJaCj3Xvu99ED9J5MvxumnvSSXho8ZvNXDKxNqU3MRCbdJSZoHEAVkLNWeSgvk2Q7xLdrkiT1fotbacrsgQox
//! ```
//!
//! ```console
//! $ mnemonic39 alone endless story black hole puzzle play you dice glory bubble awkward -p "バンドメイド"
//! 06e9375a0b76cb5de997f93d6c707508
//! 016b1146bb155c7c4ad0de6172bd4b7b6e65a487411c015c19512b8182e98e42fba7cc9da1c7631445fbfaea70057243c52d337c2d7be51786f5023086ae1bf7
//! xprv9s21ZrQH143K2pFLbGtCAETJEGS9egv9K1AhjHaACu22pEp5CUoSgFCwHYTfUE3aPKPgwccAJqneZoHX1J6iRvkkbxTuYdNhGSUHqfoWzDy
//! yprvABrGsX5C9jant7STRdfpNKYoQEabbJueE7gvWgU3auPusLdJT8y1JJs5JkRFU8hVnxWVh6CimW9CT5u5izWjEASMUJAL8YCBYAXwEGcYVem
//! zprvAWgYBBk7JR8GjQdaFzTSaQeJaCj3Xvu99ED9J5MvxumnvSSXho8ZvNXDKxNqU3MRCbdJSZoHEAVkLNWeSgvk2Q7xLdrkiT1fotbacrsgQox
//! ```
//!
//! ## Features
//!
//! * **Generate a list of valid last words**
//!
//!     Insert a mnemonic missing the last word and a list of possible valid last words (that
//!  fulfill the checksum) is showed.
//!
//! * **Generate extended keys**
//!
//!     Insert entropy or mnemonic to show extended root keys.
//!
//! * **Generate mnemonics**
//!
//!     Insert entropy and optional passphrase to generate a mnemonic in any of the languages
//!  supported in bip-0039.
//!
//! * **Generate multiple mnemonics based on a list of valid words**
//!
//!     Insert 12, 15, 18, 21 or 24 valid words in any supported language and flag `-g` to show
//!  multiple mnemonics with valid checksum.
//!
//! * **Generate seed**
//!
//!     When hexadecimal entropy is inserted the tool show the seed to.
//!
//! * **Passphrase**
//!
//!     Optional passphrase can be used with the entropy or mnemonic for additional protection.
//!
//! * **Suggestions**
//!
//!     Show suggestion of a valid mnemonic word if one invalid is inserted.
//!
//! * **Support all languages specified on
//!  [bip-0039](https://github.com/bitcoin/bips/blob/master/bip-0039/bip-0039-wordlists.md)**
//!
//!     - [Chinese (Simplified)](https://github.com/bitcoin/bips/blob/master/bip-0039/chinese_simplified.txt)
//!     - [Chinese (Traditional)](https://github.com/bitcoin/bips/blob/master/bip-0039/chinese_traditional.txt)
//!     - [Czech](https://github.com/bitcoin/bips/blob/master/bip-0039/czech.txt)
//!     - [English](https://github.com/bitcoin/bips/blob/master/bip-0039/english.txt)
//!     - [French](https://github.com/bitcoin/bips/blob/master/bip-0039/french.txt)
//!     - [Italian](https://github.com/bitcoin/bips/blob/master/bip-0039/italian.txt)
//!     - [Japanese](https://github.com/bitcoin/bips/blob/master/bip-0039/japanese.txt)
//!     - [Korean](https://github.com/bitcoin/bips/blob/master/bip-0039/korean.txt)
//!     - [Portuguese](https://github.com/bitcoin/bips/blob/master/bip-0039/portuguese.txt)
//!     - [Spanish](https://github.com/bitcoin/bips/blob/master/bip-0039/spanish.txt)
//!
//! * **Transposition**
//!
//!     Insert mnemonic and target language flag to result in words in the same 'position' on the
//!  new word-list.
//!
//! ## Help
//!
//! ```shell
//! mnemonic39 1.0.5
//! Enter optional language and hexadecimal entropy with optional passphrase or
//! enter a list of mnemonic words with optional passphrase to see information
//! about it. It can show a list of valid last words to fulfil a list of mnemonic
//! words missing the last one. Generate mnemonic using a list of valid words.
//!
//! USAGE:
//!     mnemonic39 [FLAGS] [OPTIONS] <DATA>...
//!
//! FLAGS:
//!     -c, --chinese        Mnemonic with chinese simplified words
//!     -e, --english        Mnemonic with english words
//!     -f, --french         Mnemonic with french words
//!     -g, --generate       Generate valid mnemonics permuting words
//!     -h, --help           Prints help information
//!     -i, --italian        Mnemonic with italian words
//!     -j, --japanese       Mnemonic with japanese words
//!     -k, --korean         Mnemonic with korean words
//!     -o, --portuguese     Mnemonic with portuguese words
//!     -s, --spanish        Mnemonic with spanish words
//!     -t, --traditional    Mnemonic with chinese traditional words
//!     -V, --version        Prints version information
//!     -z, --czech          Mnemonic with czech words
//!
//! OPTIONS:
//!     -p <passphrase>        optional passphrase to be used with the mnemonic
//!
//! ARGS:
//!     <DATA>...    hexadecimal entropy, seed or a list of mnemonic words
//! ```
//!
//! ## Installation
//!
//! You have to install [rust](https://www.rust-lang.org/tools/install) and a
//!  [linker](https://gcc.gnu.org/wiki/InstallingGCC) if you don't already have them.
//!
//! ```shell
//! $ cargo install mnemonic39
//! ```
//!
//! ## What this tool *don't* do
//! * **Generate entropy**
//!
//!     Pseudo-random generators are not used. You have to insert entropy, if it's the case.
//!
//! * **[Derivation](https://crates.io/crates/derivation32)**
//!
//!     This tool do not generate any type of address.

use clap::{App, Arg, ArgGroup, ArgMatches};
use hmac::{Hmac, Mac, NewMac};
use itertools::Itertools;
use pbkdf2::pbkdf2;
use sha2::{Digest, Sha512};
use strsim::normalized_levenshtein;
use unicode_normalization::UnicodeNormalization;

mod wordlists {
    pub mod chinese_simplified;
    pub mod chinese_traditional;
    pub mod czech;
    pub mod english;
    pub mod french;
    pub mod italian;
    pub mod japanese;
    pub mod korean;
    pub mod portuguese;
    pub mod spanish;
}

use wordlists::*;

/// Instructions to use the binary in command line interface.
const ABOUT: &str =
"Enter optional language and hexadecimal entropy with optional passphrase or
enter a list of mnemonic words with optional passphrase to see information
about it. It can show a list of valid last words to fulfil a list of mnemonic
words missing the last one. Generate mnemonic using a list of valid words.";

/// Number of bits represented in a single mnemonic word.
const BITS_WORD: usize = 11;

/// Number of bits represented in a single mnemonic word in float.
const BITS_WORD_F: f64 = 11.0;

/// List of valid language flags.
const LANG_FLAGS: [&str; 10] = ["c", "e", "f", "i", "j", "k", "o", "s", "t", "z"];

/// List of valid number of hexadecimal characters in entropy input.
const LEN_ENT: [usize; 5] = [32, 40, 48, 56, 64];

/// Number of hexadecimal characters contained in a string representing a seed.
const LEN_SEED: usize = 128;

/// Ideographic space used to separate japanese mnemonic words when showed.
const IDS: &str = "\u{3000}";

/// Maximum number of words in a valid mnemonic.
const MAX_WORDS: usize = 24;

/// Minimal confidence to suggest a word in place of an invalid (max 1.0).
const MIN_CONFIDENCE: f64 = 0.7;

/// Number of bytes of checksum.
const NBBY_CSUM: usize = 4;

/// Number of bytes of a root key (payload only).
const NBBY_XKEY: usize = 78;

/// Number of rounds of hmac sha512 to generate the seed.
const NB_ROUNDS: u32 = 2048;

/// List of valid number of bytes used as entropy to generate mnemonics.
const NB_BYTES: [usize; 5] = [16, 20, 24, 28, 32];

/// List of valid number of words in mnemonic.
const NB_WORDS: [usize; 5] = [12, 15, 18, 21, 24];

/// Common space used to separate mnemonic words (except in japanese).
const SPC: &str = "\u{20}";

/// Array containing wordlists of all bip-0032 supported languages.
const WORDLISTS: [[&str; 2048]; 10] = [
    chinese_simplified::CS,
    chinese_traditional::CT,
    czech::CZ,
    english::E,
    french::F,
    italian::I,
    japanese::J,
    korean::K,
    portuguese::P,
    spanish::S
];

/// Main net version of bip-0032 extended private keys.
const XPRV: [u8; 4] = [0x04, 0x88, 0xad, 0xe4];

/// Main net version of bip-0049 extended private keys.
const YPRV: [u8; 4] = [0x04, 0x9d, 0x78, 0x78];

/// Main net version of bip-0084 extended private keys.
const ZPRV: [u8; 4] = [0x04, 0xb2, 0x43, 0x0c];

/// Error types for 'mnemonic' project.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd)]
#[doc(hidden)]
pub enum Error {
    /// Invalid checksum encountered.
    Checksum,
    /// If an invalid entropy is inserted.
    Data(String, usize),
    /// Found use of an invalid flag in the current context.
    Flag(String),
    /// Invalid hexadecimal value represented in string.
    HexStr(String),
    /// Invalid input in hmac function.
    Hmac,
    /// Error occurred while generating last word
    Last,
    /// If an invalid number of bytes is found.
    NbBytes(usize),
    /// Error message presented if an invalid number of words is inserted.
    NbWords(usize),
    /// Fatal error while reading required argument.
    Parse,
    /// If the target and resulting language to transpose are the same.
    SameLang,
    /// Found invalid seed.
    Seed(String),
    /// Found an invalid word and possibly provided a suggestion.
    Word(String, String)
}

/// Functions to manipulate data in form of arbitrary number of bytes [u8].
trait BytesManipulation {
    /// Generate a checksum according to informed bytes.
    fn checksum(&self) -> u8;

    /// Validate the checksum contained in the last word.
    fn checksum_validation(&self) -> bool;

    /// Receives bytes and return string of hexadecimal characters.
    fn hex_string(&self) -> String;

    /// Receives entropy as bytes and returns a String of mnemonic words.
    fn mnemonic_string(&self, lang: &[&str; 2048]) -> Result<String, Error>;

    /// Retrieve the length of entropy bytes excluding the checksum.
    fn unchecked_len(&self) -> usize;
}

/// Functions to manipulate seed in form of bytes [u8; 64]
trait SeedBytesManipulation {
    /// Generate extended key based on target seed and version prefix.
    fn extended_key(&self, version: &[u8; 4]) -> Result<String, Error>;
}

/// Functions to manipulate strings in various occasions.
trait StringManipulation {
    /// Retrieve a wordlist based on the target option (english is default).
    fn choose_lang(&self) -> Result<[&str; 2048], Error>;

    /// Return a wordlist based in the target mnemonic (validate all words).
    fn detect_lang(&self) -> Result<[&str; 2048], Error>;

    /// Transform string of hexadecimal characters into a vector of bytes.
    fn hex_bytes(&self) -> Result<Vec<u8>, Error>;

    /// Test if an string of arbitrary length contains only hexadecimal chars.
    fn is_hex(&self) -> bool;

    /// Returns the first invalid word and the percentage of valid that are
    /// on the wordlist.
    fn invalid_word(&self, wordlist: &[&str; 2048]) -> Option<(String, f64)>;

    /// Generate a list of valid last words for a mnemonic missing one.
    fn last_word(&self) -> Result<Vec<String>, Error>;

    /// Retrieve bytes representation of a mnemonic preserving the checksum.
    fn mnemonic_bytes(&self) -> Result<Vec<u8>, Error>;

    /// Generate the seed used to create the root key.
    fn seed_bytes(&self, passphrase: &str) -> [u8; 64];

    /// Show information about entropy informed by user.
    fn show_entropy(&self, option: &str, pass: &str) -> Result<(), Error>;

    /// Show a list of last possible words based on a mnemonic missing one.
    fn show_last(&self) -> Result<(), Error>;

    /// Show information about mnemonic informed by user.
    fn show_mnemonic(&self, pass: &str) -> Result<(), Error>;

    /// Generate valid mnemonics doing permutations on a vector of Strings.
    fn show_permutation(&self) -> Result<(), Error>;

    /// Show information about seed informed by user.
    fn show_seed(&self) -> Result<(), Error>;

    /// Show transposition from one language to another on cli.
    fn show_transposition(&self, opt: &str, pass: &str) -> Result<(), Error>;

    /// Provides suggestion for an invalid word in the specified wordlist.
    fn suggestion(&self, wordlist: &[&str; 2048]) -> String;

    /// Generate transposition of a mnemonic in one language to another.
    fn transposition(&self, opt: &str) -> Result<String, Error>;
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Error::Checksum =>
                write!(f, "invalid checksum (consider using flag '\x1b[33mg\x1b[m')"),
            Error::Data(e, n) =>
                write!(f, "invalid data '\x1b[33m{}\x1b[m' (length: \x1b[33m{}\x1b[m)", e, n),
            Error::Flag(l) =>
                write!(f, "'\x1b[33m{}\x1b[m' flag invalid in this context", l),
            Error::HexStr(s) =>
                write!(f, "invalid hexadecimal string '\x1b[33m{}\x1b[m'", s),
            Error::Hmac => write!(f, "invalid input in hmac function"),
            Error::Last => write!(f, "cannot generate last word"),
            Error::NbBytes(b) =>
                write!(f, "invalid number of bytes '\x1b[33m{}\x1b[m'", b),
            Error::NbWords(n) =>
                write!(f, "invalid number of words '\x1b[33m{}\x1b[m'", n),
            Error::Parse => write!(f, "cannot parse required argument"),
            Error::SameLang => write!(f, "same language, nothing to do"),
            Error::Seed(s) => write!(f, "invalid seed '\x1b[33m{}\x1b[m'", s),
            Error::Word(w, s) => write!(
                f, "invalid word '\x1b[33m{}\x1b[m'{}", w, if s.is_empty() {
                    String::new()
                } else {
                    format!(" (did you mean \x1b[32m{}\x1b[m?)", s)
                }
            )
        }
    }
}

/// Implementation of trait BytesManipulation for [u8].
impl BytesManipulation for [u8] {
    #[inline]
    fn checksum(&self) -> u8 {
        let len_bytes = self.unchecked_len();
        let bits = len_bytes * 8 / 32;
        let out = sha2::Sha256::digest(&self[..len_bytes])[0]; // first byte
        // zeroes least significant bits and return the most to place
        out >> (8 - bits) << (8 - bits)
    }

    #[inline]
    fn checksum_validation(&self) -> bool {
        self[self.len() - 1] == self.checksum()
    }

    #[inline]
    fn hex_string(&self) -> String {
        let mut result = String::new();
        for byte in self {
            result = format!("{}{:02x}", result, byte);
        }
        result
    }

    #[inline]
    fn mnemonic_string(&self, lang: &[&str; 2048]) -> Result<String, Error> {
        if !NB_BYTES.contains(&self.len()) {
            return Err(Error::NbBytes(self.len()));
        }
        let nb_bytes = self.unchecked_len();
        let check = self.checksum();
        let separator = if lang == &japanese::J { IDS } else { SPC };

        // partially a copy from 'https://github.com/rust-bitcoin/rust-bip39/'
        let mut bits = [false; BITS_WORD * MAX_WORDS];
        for i in 0..nb_bytes {
            for j in 0..8 {
                bits[i * 8 + j] = (self[i] & (1 << (7 - j))) > 0;
            }
        }
        for i in 0..nb_bytes / 4 {
            bits[8 * nb_bytes + i] = (check & (1 << (7 - (i % 8)))) > 0;
        }

        let mut words: String = String::new();
        let nb_words = nb_bytes * 3 / 4;
        for i in 0..nb_words {
            let mut idx = 0;
            for j in 0..11 {
                if bits[i * 11 + j] {
                    idx += 1 << (10 - j);
                }
            }
            words = words + lang[idx] + separator;
        }
        Ok(String::from(words.trim_end()))
    }

    #[inline]
    fn unchecked_len(&self) -> usize {
        if self.len() % 2 != 0 { return self.len() - 1; }
        self.len()
    }
}

/// Implementation of enum Error.
impl Error {
    /// Retrieve the status code to be showed when exiting because of an error.
    #[doc(hidden)]
    pub fn status(&self) -> i32 {
        match self {
            Error::Checksum => 1,
            Error::Data(..) => 2,
            Error::Flag(_) => 3,
            Error::HexStr(_) => 4,
            Error::Hmac => 5,
            Error::Last => 6,
            Error::NbBytes(_) => 7,
            Error::NbWords(_) => 8,
            Error::Parse => 9,
            Error::SameLang => 10,
            Error::Seed(_) => 11,
            Error::Word(..) => 12
        }
    }
}

/// Implementation of trait SeedBytesManipulation for [u8].
impl SeedBytesManipulation for [u8; 64] {
    #[inline]
    fn extended_key(&self, version: &[u8; 4]) -> Result<String, Error> {
        let mut hmac = Hmac::<Sha512>::new_from_slice(b"Bitcoin seed").map_err(|_| Error::Hmac)?;
        hmac.update(self);
        let hmac = hmac.finalize().into_bytes();
        let mut payload = [0u8; NBBY_XKEY];
        payload[..4].copy_from_slice(version);         // version prefix
        payload[4] = 0x00;                             // depth
        payload[5..9].copy_from_slice(&[0x00; 4]);     // parent fingerprint
        payload[9..13].copy_from_slice(&[0x00; 4]);    // child number
        payload[13..45].copy_from_slice(&hmac[32..]);  // chain code
        payload[45] = 0x00;                            // start of private data
        payload[46..].copy_from_slice(&hmac[..32]);    // private key
        let mut xkey = [0u8; NBBY_XKEY + NBBY_CSUM];
        xkey[..78].copy_from_slice(&payload);          // payload
        xkey[78..].copy_from_slice(                    // checksum
            &sha2::Sha256::digest(&sha2::Sha256::digest(&payload))[..NBBY_CSUM]
        );
        Ok(bs58::encode(xkey).into_string())
    }
}

/// Implementation of trait StringManipulation for str.
impl StringManipulation for str {
    #[inline]
    fn detect_lang(&self) -> Result<[&str; 2048], Error> {
        let mut invalid = String::new();
        let mut best = (english::E, 0.0);

        // try all because ambiguity of words in different languages
        for wordlist in &WORDLISTS {
            match self.invalid_word(wordlist) {
                Some(inv) => {
                    if best.1 == 0.0 || best.1 < inv.1 {
                        invalid = inv.0;
                        best = (*wordlist, inv.1);
                    }
                },
                None => return Ok(*wordlist)
            }
        }
        if invalid.is_empty() {
            invalid = String::from(self.split_whitespace().collect::<Vec<&str>>()[0]);
        }
        let suggestion = if best.1 == 0.0 { String::new() } else { invalid.suggestion(&best.0) };
        Err(Error::Word(invalid, suggestion))
    }

    #[inline]
    fn hex_bytes(&self) -> Result<Vec<u8>, Error> {
        let mut out = Vec::new();
        for index in (0..self.len()).step_by(2) {
            out.push(
                u8::from_str_radix(&self[index..index + 2], 16)
                    .map_err(|_| Error::HexStr(String::from(self)))?
            );
        }
        Ok(out)
    }

    #[inline]
    fn is_hex(&self) -> bool {
        for c in self.chars() {
            if !c.is_ascii_hexdigit() { return false }
        }
        true
    }

    #[inline]
    fn invalid_word(&self, wordlist: &[&str; 2048]) -> Option<(String, f64)> {
        let nb_words = self.split_whitespace().count();
        let mut nb_ok = 0;
        let mut invalid = String::new();
        if nb_words == 0 { return Some((String::from(""), 0.0)); }
        for word in self.split_whitespace() {
            if wordlist.contains(&word) {
                nb_ok += 1;
            } else if invalid.is_empty() {
                invalid = String::from(word);
            }
        }
        if invalid.is_empty() {
            None
        } else {
            Some((invalid, nb_ok as f64 * 100.0 / nb_words as f64))
        }
    }

    #[inline]
    fn choose_lang(&self) -> Result<[&str; 2048], Error> {
        match self {
            "c" => Ok(chinese_simplified::CS),
            "e" => Ok(english::E),
            "f" => Ok(french::F),
            "i" => Ok(italian::I),
            "j" => Ok(japanese::J),
            "k" => Ok(korean::K),
            "o" => Ok(portuguese::P),
            "s" => Ok(spanish::S),
            "t" => Ok(chinese_traditional::CT),
            "z" => Ok(czech::CZ),
            "" => Ok(english::E),
            _ => Err(Error::Flag(String::from(self)))
        }
    }

    #[inline]
    fn last_word(&self) -> Result<Vec<String>, Error> {
        let nb_words = self.split_whitespace().count();
        if !NB_WORDS.contains(&(nb_words + 1)) { return Err(Error::NbWords(nb_words)); }
        let bytes = self.mnemonic_bytes()?;
        let wordlist = self.detect_lang()?;
        let nb_available_bits = BITS_WORD - (nb_words + 1) * BITS_WORD / 32;
        let nb_last_words = 2u8.pow(nb_available_bits as u32);
        let separator = if wordlist == japanese::J { IDS } else { SPC };
        let mut out: Vec<String> = Vec::new();

        for nonce in 0..nb_last_words {
            let mut temp = bytes[..bytes.len() - 1].to_vec();
            temp.push(bytes[bytes.len() - 1] + nonce);
            match temp.mnemonic_string(&wordlist)?.rsplit_once(separator) {
                Some(tuple) => out.push(String::from(tuple.1)),
                None => return Err(Error::Last)
            }
        }
        Ok(out)
    }

    #[inline]
    fn mnemonic_bytes(&self) -> Result<Vec<u8>, Error> {
        let mnemonic = self.split_whitespace()
            .map(String::from)
            .collect::<Vec<String>>();
        if !NB_WORDS.contains(&mnemonic.len()) && !NB_WORDS.contains(&(mnemonic.len() + 1)) {
            return Err(Error::NbWords(mnemonic.len()));
        }
        let wordlist = self.detect_lang()?;
        let nb_bytes = (mnemonic.len() as f64 * BITS_WORD_F / 8.0).ceil() as usize;

        // partially a copy from 'https://github.com/rust-bitcoin/rust-bip39/'
        let mut entropy = [0; 33]; // maximum, has to be trimmed at the end
        let mut cursor = 0;
        let mut offset = 0;
        let mut remainder = 0;

        for word in mnemonic {
            let index = wordlist.iter().position(|&x| x == word)
                .ok_or_else(|| Error::Word(word, String::new()))?;

            remainder |= ((index as u32) << (32 - BITS_WORD)) >> offset;
            offset += BITS_WORD;

            while offset >= 8 {
                entropy[cursor] = (remainder >> 24) as u8;
                cursor += 1;
                remainder <<= 8;
                offset -= 8;
            }
        }
        if offset != 0 { entropy[cursor] = (remainder >> 24) as u8; }
        Ok(entropy[..nb_bytes].to_vec()) // trim (in case of 21 or less words)
    }

    #[inline]
    fn seed_bytes(&self, passphrase: &str) -> [u8; 64] {
        let mut seed = [0u8; 64];
        let mnemonic_nfkd = self.nfkd().collect::<String>();
        let passphrase_nfkd = passphrase.nfkd().collect::<String>();
        pbkdf2::<Hmac<Sha512>>(
            mnemonic_nfkd.as_bytes(),
            format!("mnemonic{}", passphrase_nfkd).as_bytes(),
            NB_ROUNDS,
            &mut seed
        );
        seed
    }

    #[inline]
    fn show_entropy(&self, option: &str, pass: &str) -> Result<(), Error> {
        let mnemonic = self.hex_bytes()?.mnemonic_string(&option.choose_lang()?)?;
        let seed = mnemonic.seed_bytes(pass);

        println!(
            "{}\n{}\n{}\n{}\n{}",
            mnemonic,
            seed.hex_string(),
            seed.extended_key(&XPRV)?,
            seed.extended_key(&YPRV)?,
            seed.extended_key(&ZPRV)?
        );
        Ok(())
    }

    #[inline]
    fn show_last(self: &str) -> Result<(), Error> {
        for valid_last_word in self.last_word()? {
            println!("{}", valid_last_word);
        }
        Ok(())
    }

    #[inline]
    fn show_mnemonic(&self, pass: &str) -> Result<(), Error> {
        let nb_words = self.split_whitespace().count();
        if !NB_WORDS.contains(&nb_words) { return Err(Error::NbWords(nb_words)); }
        let raw_bytes = self.mnemonic_bytes()?;

        if raw_bytes.checksum_validation() {
            let seed = self.seed_bytes(pass);

            println!(
                "{}\n{}\n{}\n{}\n{}",
                raw_bytes[..raw_bytes.len() - 1].hex_string(), // trim checksum
                seed.hex_string(),
                seed.extended_key(&XPRV)?,
                seed.extended_key(&YPRV)?,
                seed.extended_key(&ZPRV)?
            );
        } else {
            return Err(Error::Checksum);
        }
        Ok(())
    }

    #[inline]
    fn show_permutation(&self) -> Result<(), Error> {
        let nb_words = self.split_whitespace().count();
        if !NB_WORDS.contains(&nb_words) { return Err(Error::NbWords(nb_words)); }

        println!("Press CTRL-C to stop.");

        for permutation in self.split_whitespace().permutations(nb_words) {
            let new_mn = permutation.join(SPC);
            if new_mn.mnemonic_bytes()?.checksum_validation() {
                println!("{}", new_mn);
            }
        }
        Ok(())
    }

    #[inline]
    fn show_seed(&self) -> Result<(), Error> {
        if self.len() != LEN_SEED || !self.is_hex() {
            return Err(Error::Seed(String::from(self)));
        }

        let mut seed = [0u8; 64];
        seed.copy_from_slice(&self.hex_bytes()?);

        println!(
            "{}\n{}\n{}",
            seed.extended_key(&XPRV)?,
            seed.extended_key(&YPRV)?,
            seed.extended_key(&ZPRV)?
        );
        Ok(())
    }

    #[inline]
    fn show_transposition(&self, opt: &str, pass: &str) -> Result<(), Error> {
        let raw_bytes = self.mnemonic_bytes()?;

        if raw_bytes.checksum_validation() {
            let transposed = self.transposition(opt)?;

            println!("{}", transposed);

            let seed = transposed.seed_bytes(pass);

            println!(
                "{}\n{}\n{}\n{}\n{}",
                raw_bytes[..raw_bytes.len() - 1].hex_string(),
                seed.hex_string(),
                seed.extended_key(&XPRV)?,
                seed.extended_key(&YPRV)?,
                seed.extended_key(&ZPRV)?
            );
        } else {
            return Err(Error::Checksum);
        }
        Ok(())
    }

    #[inline]
    fn suggestion(&self, wordlist: &[&str; 2048]) -> String {
        let mut candidate = ("", 0.0);
        for word in wordlist {
            let confidence = normalized_levenshtein(self, word);
            if (candidate.1 < confidence || candidate.0.is_empty()) &&
                confidence >= MIN_CONFIDENCE {
                candidate = (word, confidence);
            }
        }
        String::from(candidate.0)
    }

    #[inline]
    fn transposition(&self, opt: &str) -> Result<String, Error> {
        let nb_words = self.split_whitespace().count();
        if !NB_WORDS.contains(&nb_words) { return Err(Error::NbWords(nb_words)); }
        let new_lang = opt.choose_lang()?;
        let orig_lang = self.detect_lang()?;
        if orig_lang == new_lang { return Err(Error::SameLang); }
        let separator = if new_lang == japanese::J { IDS } else { SPC };
        let mut result = String::new();

        for word in self.split_whitespace() {
            let index = orig_lang.iter().position(|&x| x == word)
                .ok_or_else(|| Error::Word(String::from(word), String::new()))?;

            result = result + new_lang[index] + separator;
        }
        Ok(String::from(result.trim_end()))
    }
}

/// Handle arguments and call functions accordingly.
#[doc(hidden)]
pub fn handle_arguments(matches: &ArgMatches) -> Result<(), Error> {
    let data = matches.values_of("DATA")
        .ok_or(Error::Parse)?
        .collect::<Vec<&str>>();

    // validate the number of data values
    if data.len() == 1 && data[0].len() != LEN_SEED && !LEN_ENT.contains(&data[0].len()) {
        return Err(Error::Data(String::from(data[0]), data[0].len()));
    } else if data.len() != 1 && !NB_WORDS.contains(&data.len()) &&
        !NB_WORDS.contains(&(data.len() + 1)) {
        return Err(Error::NbWords(data.len()));
    }

    let passphrase = matches.value_of("passphrase").unwrap_or("");
    let mut lang_flag = String::new();
    for flag in &LANG_FLAGS {
        if matches.is_present(flag) {
            lang_flag = String::from(*flag);
            break; // accepts only one language flag
        }
    }

    // if flag 'g' with entropy or invalid number of words
    if matches.is_present("generate") && !NB_WORDS.contains(&data.len()) {
        if data.len() == 1 { // if just one, treat as entropy
            return Err(Error::Flag(String::from("g")));
        } else { // treat as invalid number of words to generate
            return Err(Error::NbWords(data.len()));
        }
    }

    if data.len() == 1 && matches.is_present("passphrase") {
        if LEN_ENT.contains(&data[0].len()) && data[0].is_hex() {
            data[0].show_entropy(&lang_flag, passphrase)?;
        } else {
            return Err(Error::Data(String::from(data[0]), data[0].len()));
        }
    } else if data.len() == 1 { // possible entropy or seed
        if data[0].len() == LEN_SEED && data[0].is_hex() {
            data[0].show_seed()?;
        } else if LEN_ENT.contains(&data[0].len()) && data[0].is_hex() {
            data[0].show_entropy(&lang_flag, "")?;
        } else {
            return Err(Error::Data(String::from(data[0]), data[0].len()));
        }
    } else if NB_WORDS.contains(&(data.len() + 1)) && lang_flag.is_empty() {
        data.join(SPC).show_last()?;
    } else if NB_WORDS.contains(&data.len()) && matches.is_present("generate"){
        data.join(SPC).show_permutation()?;
    } else if !lang_flag.is_empty() && !matches.is_present("generate") &&
        NB_WORDS.contains(&data.len()) {
        data.join(SPC).show_transposition(&lang_flag, passphrase)?;
    } else if NB_WORDS.contains(&data.len()) {
        data.join(SPC).show_mnemonic(passphrase)?;
    } else {
        return Err(Error::NbWords(data.len()));
    }
    Ok(())
}

/// Initialize clap app.
#[doc(hidden)]
pub fn init_clap() -> App<'static, 'static> {
    App::new("mnemonic39")
        .about(ABOUT)
        .arg(
            Arg::with_name("DATA")
                .help("hexadecimal entropy, seed or a list of mnemonic words")
                .max_values(MAX_WORDS as u64)
                .multiple(true)
                .required(true)
        ).arg(
            Arg::with_name("c")
                .help("Mnemonic with chinese simplified words")
                .long("chinese")
                .short("c")
        ).arg(
            Arg::with_name("e")
                .help("Mnemonic with english words")
                .long("english")
                .short("e")
        ).arg(
            Arg::with_name("f")
                .help("Mnemonic with french words")
                .long("french")
                .short("f")
        ).arg(
            Arg::with_name("generate")
                .group("flags")
                .help("Generate valid mnemonics permuting words")
                .long("generate")
                .short("g")
        ).arg(
            Arg::with_name("i")
                .help("Mnemonic with italian words")
                .long("italian")
                .short("i")
        ).arg(
            Arg::with_name("j")
                .help("Mnemonic with japanese words")
                .long("japanese")
                .short("j")
        ).arg(
            Arg::with_name("k")
                .help("Mnemonic with korean words")
                .long("korean")
                .short("k")
        ).arg(
            Arg::with_name("o")
                .help("Mnemonic with portuguese words")
                .long("portuguese")
                .short("o")
        ).arg(
            Arg::with_name("passphrase")
                .help("optional passphrase to be used with the mnemonic")
                .short("p")
                .takes_value(true)
        ).arg(
            Arg::with_name("s")
                .help("Mnemonic with spanish words")
                .long("spanish")
                .short("s")
        ).arg(
            Arg::with_name("t")
                .help("Mnemonic with chinese traditional words")
                .long("traditional")
                .short("t")
        ).arg(
            Arg::with_name("z")
                .help("Mnemonic with czech words")
                .long("czech")
                .short("z")
        ).group(
            ArgGroup::with_name("flags")
                .args(&LANG_FLAGS)
        ).version(clap::crate_version!())
}

/// Tests for the library.
#[cfg(test)]
mod tests {
    use super::*;

    // https://github.com/trezor/python-mnemonic/blob/master/vectors.json
    const TV_ENTROPY: [&str; 24] = [
        "00000000000000000000000000000000",
        "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
        "80808080808080808080808080808080",
        "ffffffffffffffffffffffffffffffff",
        "000000000000000000000000000000000000000000000000",
        "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
        "808080808080808080808080808080808080808080808080",
        "ffffffffffffffffffffffffffffffffffffffffffffffff",
        "0000000000000000000000000000000000000000000000000000000000000000",
        "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
        "8080808080808080808080808080808080808080808080808080808080808080",
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        "9e885d952ad362caeb4efe34a8e91bd2",
        "6610b25967cdcca9d59875f5cb50b0ea75433311869e930b",
        "68a79eaca2324873eacc50cb9c6eca8cc68ea5d936f98787c60c7ebc74e6ce7c",
        "c0ba5a8e914111210f2bd131f3d5e08d",
        "6d9be1ee6ebd27a258115aad99b7317b9c8d28b6d76431c3",
        "9f6a2878b2520799a44ef18bc7df394e7061a224d2c33cd015b157d746869863",
        "23db8160a31d3e0dca3688ed941adbf3",
        "8197a4a47f0425faeaa69deebc05ca29c0a5b5cc76ceacc0",
        "066dca1a2bb7e8a1db2832148ce9933eea0f3ac9548d793112d9a95c9407efad",
        "f30f8c1da665478f49b001d94c5fc452",
        "c10ec20dc3cd9f652c7fac2f1230f7a3c828389a14392f05",
        "f585c11aec520db57dd353c69554b21a89b20fb0650966fa0a9d6f74fd989d8f",
    ];
    const TV_MNEMONIC: [[&str; 24]; 24] = [
        [
            "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon",
            "abandon", "abandon", "abandon", "about", "", "", "", "", "", "", "", "", "", "", "",
            ""
        ],
        [
            "legal", "winner", "thank", "year", "wave", "sausage", "worth", "useful", "legal",
            "winner", "thank", "yellow", "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "letter", "advice", "cage", "absurd", "amount", "doctor", "acoustic", "avoid",
            "letter", "advice", "cage", "above", "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "wrong",
            "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon",
            "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon",
            "abandon", "agent", "", "", "", "", "", ""
        ],
        [
            "legal", "winner", "thank", "year", "wave", "sausage", "worth", "useful", "legal",
            "winner", "thank", "year", "wave", "sausage", "worth", "useful", "legal", "will", "",
            "", "", "", "", ""
        ],
        [
            "letter", "advice", "cage", "absurd", "amount", "doctor", "acoustic", "avoid",
            "letter", "advice", "cage", "absurd", "amount", "doctor", "acoustic", "avoid",
            "letter", "always", "", "", "", "", "", ""
        ],
        [
            "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo",
            "zoo", "zoo", "zoo", "zoo", "zoo", "when", "", "", "", "", "", ""
        ],
        [
            "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon",
            "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon",
            "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "abandon", "art"
        ],
        [
            "legal", "winner", "thank", "year", "wave", "sausage", "worth", "useful", "legal",
            "winner", "thank", "year", "wave", "sausage", "worth", "useful", "legal", "winner",
            "thank", "year", "wave", "sausage", "worth", "title"
        ],
        [
            "letter", "advice", "cage", "absurd", "amount", "doctor", "acoustic", "avoid",
            "letter", "advice", "cage", "absurd", "amount", "doctor", "acoustic", "avoid",
            "letter", "advice", "cage", "absurd", "amount", "doctor", "acoustic", "bless"
        ],
        [
            "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo",
            "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "zoo", "vote"
        ],
        [
            "ozone", "drill", "grab", "fiber", "curtain", "grace", "pudding", "thank", "cruise",
            "elder", "eight", "picnic", "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "gravity", "machine", "north", "sort", "system", "female", "filter", "attitude",
            "volume", "fold", "club", "stay", "feature", "office", "ecology", "stable", "narrow",
            "fog", "", "", "", "", "", ""
        ],
        [
            "hamster", "diagram", "private", "dutch", "cause", "delay", "private", "meat", "slide",
            "toddler", "razor", "book", "happy", "fancy", "gospel", "tennis", "maple", "dilemma",
            "loan", "word", "shrug", "inflict", "delay", "length"
        ],
        [
            "scheme", "spot", "photo", "card", "baby", "mountain", "device", "kick", "cradle",
            "pact", "join", "borrow", "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "horn", "tenant", "knee", "talent", "sponsor", "spell", "gate", "clip", "pulse",
            "soap", "slush", "warm", "silver", "nephew", "swap", "uncle", "crack", "brave", "", "",
            "", "", "", ""
        ],
        [
            "panda", "eyebrow", "bullet", "gorilla", "call", "smoke", "muffin", "taste", "mesh",
            "discover", "soft", "ostrich", "alcohol", "speed", "nation", "flash", "devote",
            "level", "hobby", "quick", "inner", "drive", "ghost", "inside"
        ],
        [
            "cat", "swing", "flag", "economy", "stadium", "alone", "churn", "speed", "unique",
            "patch", "report", "train", "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "light", "rule", "cinnamon", "wrap", "drastic", "word", "pride", "squirrel", "upgrade",
            "then", "income", "fatal", "apart", "sustain", "crack", "supply", "proud", "access",
            "", "", "", "", "", ""
        ],
        [
            "all", "hour", "make", "first", "leader", "extend", "hole", "alien", "behind", "guard",
            "gospel", "lava", "path", "output", "census", "museum", "junior", "mass", "reopen",
            "famous", "sing", "advance", "salt", "reform"
        ],
        [
            "vessel", "ladder", "alter", "error", "federal", "sibling", "chat", "ability", "sun",
            "glass", "valve", "picture", "", "", "", "", "", "", "", "", "", "", "", ""
        ],
        [
            "scissors", "invite", "lock", "maple", "supreme", "raw", "rapid", "void", "congress",
            "muscle", "digital", "elegant", "little", "brisk", "hair", "mango", "congress",
            "clump", "", "", "", "", "", ""
        ],
        [
            "void", "come", "effort", "suffer", "camp", "survey", "warrior", "heavy", "shoot",
            "primary", "clutch", "crush", "open", "amazing", "screen", "patrol", "group", "space",
            "point", "ten", "exist", "slush", "involve", "unfold"
        ]
    ];
    const TV_PASS: &str = "TREZOR";
    const TV_SEED: [[&str; 2]; 24] = [
        [
            "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1",
            "e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04",
        ],
        [
            "2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1296106559a3c8",
            "0937a1c1069be3a3a5bd381ee6260e8d9739fce1f607",
        ],
        [
            "d71de856f81a8acc65e6fc851a38d4d7ec216fd0796d0a6827a3ad6ed5511a30fa280f12eb2e47ed2ac0",
            "3b5c462a0358d18d69fe4f985ec81778c1b370b652a8",
        ],
        [
            "ac27495480225222079d7be181583751e86f571027b0497b5b5d11218e0a8a13332572917f0f8e5a5896",
            "20c6f15b11c61dee327651a14c34e18231052e48c069",
        ],
        [
            "035895f2f481b1b0f01fcf8c289c794660b289981a78f8106447707fdd9666ca06da5a9a565181599b79",
            "f53b844d8a71dd9f439c52a3d7b3e8a79c906ac845fa",
        ],
        [
            "f2b94508732bcbacbcc020faefecfc89feafa6649a5491b8c952cede496c214a0c7b3c392d168748f2d4",
            "a612bada0753b52a1c7ac53c1e93abd5c6320b9e95dd",
        ],
        [
            "107d7c02a5aa6f38c58083ff74f04c607c2d2c0ecc55501dadd72d025b751bc27fe913ffb796f841c49b",
            "1d33b610cf0e91d3aa239027f5e99fe4ce9e5088cd65",
        ],
        [
            "0cd6e5d827bb62eb8fc1e262254223817fd068a74b5b449cc2f667c3f1f985a76379b43348d952e2265b",
            "4cd129090758b3e3c2c49103b5051aac2eaeb890a528",
        ],
        [
            "bda85446c68413707090a52022edd26a1c9462295029f2e60cd7c4f2bbd3097170af7a4d73245cafa9c3",
            "cca8d561a7c3de6f5d4a10be8ed2a5e608d68f92fcc8",
        ],
        [
            "bc09fca1804f7e69da93c2f2028eb238c227f2e9dda30cd63699232578480a4021b146ad717fbb7e451c",
            "e9eb835f43620bf5c514db0f8add49f5d121449d3e87",
        ],
        [
            "c0c519bd0e91a2ed54357d9d1ebef6f5af218a153624cf4f2da911a0ed8f7a09e2ef61af0aca007096df",
            "430022f7a2b6fb91661a9589097069720d015e4e982f",
        ],
        [
            "dd48c104698c30cfe2b6142103248622fb7bb0ff692eebb00089b32d22484e1613912f0a5b694407be89",
            "9ffd31ed3992c456cdf60f5d4564b8ba3f05a69890ad",
        ],
        [
            "274ddc525802f7c828d8ef7ddbcdc5304e87ac3535913611fbbfa986d0c9e5476c91689f9c8a54fd55bd",
            "38606aa6a8595ad213d4c9c9f9aca3fb217069a41028",
        ],
        [
            "628c3827a8823298ee685db84f55caa34b5cc195a778e52d45f59bcf75aba68e4d7590e101dc414bc1bb",
            "d5737666fbbef35d1f1903953b66624f910feef245ac",
        ],
        [
            "64c87cde7e12ecf6704ab95bb1408bef047c22db4cc7491c4271d170a1b213d20b385bc1588d9c7b38f1",
            "b39d415665b8a9030c9ec653d75e65f847d8fc1fc440",
        ],
        [
            "ea725895aaae8d4c1cf682c1bfd2d358d52ed9f0f0591131b559e2724bb234fca05aa9c02c57407e04ee",
            "9dc3b454aa63fbff483a8b11de949624b9f1831a9612",
        ],
        [
            "fd579828af3da1d32544ce4db5c73d53fc8acc4ddb1e3b251a31179cdb71e853c56d2fcb11aed39898ce",
            "6c34b10b5382772db8796e52837b54468aeb312cfc3d",
        ],
        [
            "72be8e052fc4919d2adf28d5306b5474b0069df35b02303de8c1729c9538dbb6fc2d731d5f832193cd9f",
            "b6aeecbc469594a70e3dd50811b5067f3b88b28c3e8d",
        ],
        [
            "deb5f45449e615feff5640f2e49f933ff51895de3b4381832b3139941c57b59205a42480c52175b6efcf",
            "faa58a2503887c1e8b363a707256bdd2b587b46541f5",
        ],
        [
            "4cbdff1ca2db800fd61cae72a57475fdc6bab03e441fd63f96dabd1f183ef5b782925f00105f318309a7",
            "e9c3ea6967c7801e46c8a58082674c860a37b93eda02",
        ],
        [
            "26e975ec644423f4a4c4f4215ef09b4bd7ef924e85d1d17c4cf3f136c2863cf6df0a475045652c57eb5f",
            "b41513ca2a2d67722b77e954b4b3fc11f7590449191d",
        ],
        [
            "2aaa9242daafcee6aa9d7269f17d4efe271e1b9a529178d7dc139cd18747090bf9d60295d0ce74309a78",
            "852a9caadf0af48aae1c6253839624076224374bc63f",
        ],
        [
            "7b4a10be9d98e6cba265566db7f136718e1398c71cb581e1b2f464cac1ceedf4f3e274dc270003c670ad",
            "8d02c4558b2f8e39edea2775c9e232c7cb798b069e88",
        ],
        [
            "01f5bced59dec48e362f2c45b5de68b9fd6c92c6634f44d6d40aab69056506f0e35524a518034ddc1192",
            "e1dacd32c1ed3eaa3c3b131c88ed8e7e54c49a5d0998",
        ]
    ];
    const TV_XKEY: [[&str; 2]; 24] = [
        [
            "xprv9s21ZrQH143K3h3fDYiay8mocZ3afhfULfb5GX8kCBdno77K4HiA15Tg23wpbeF1pLfs1c5SPmYHrEpT",
            "uuRhxMwvKDwqdKiGJS9XFKzUsAF"
        ],
        [
            "xprv9s21ZrQH143K2gA81bYFHqU68xz1cX2APaSq5tt6MFSLeXnCKV1RVUJt9FWNTbrrryem4ZckN8k4Ls1H",
            "6nwdvDTvnV7zEXs2HgPezuVccsq"
        ],
        [
            "xprv9s21ZrQH143K2shfP28KM3nr5Ap1SXjz8gc2rAqqMEynmjt6o1qboCDpxckqXavCwdnYds6yBHZGKHv7",
            "ef2eTXy461PXUjBFQg6PrwY4Gzq"
        ],
        [
            "xprv9s21ZrQH143K2V4oox4M8Zmhi2Fjx5XK4Lf7GKRvPSgydU3mjZuKGCTg7UPiBUD7ydVPvSLtg9hjp7MQ",
            "TYsW67rZHAXeccqYqrsx8LcXnyd"
        ],
        [
            "xprv9s21ZrQH143K3mEDrypcZ2usWqFgzKB6jBBx9B6GfC7fu26X6hPRzVjzkqkPvDqp6g5eypdk6cyhGnBn",
            "gbjeHTe4LsuLG1cCmKJka5SMkmU"
        ],
        [
            "xprv9s21ZrQH143K3Lv9MZLj16np5GzLe7tDKQfVusBni7toqJGcnKRtHSxUwbKUyUWiwpK55g1DUSsw76TF",
            "1T93VT4gz4wt5RM23pkaQLnvBh7"
        ],
        [
            "xprv9s21ZrQH143K3VPCbxbUtpkh9pRG371UCLDz3BjceqP1jz7XZsQ5EnNkYAEkfeZp62cDNj13ZTEVG1TE",
            "ro9sZ9grfRmcYWLBhCocViKEJae"
        ],
        [
            "xprv9s21ZrQH143K36Ao5jHRVhFGDbLP6FCx8BEEmpru77ef3bmA928BxsqvVM27WnvvyfWywiFN8K6yToqM",
            "aGYfzS6Db1EHAXT5TuyCLBXUfdm"
        ],
        [
            "xprv9s21ZrQH143K32qBagUJAMU2LsHg3ka7jqMcV98Y7gVeVyNStwYS3U7yVVoDZ4btbRNf4h6ibWpY22iR",
            "mXq35qgLs79f312g2kj5539ebPM"
        ],
        [
            "xprv9s21ZrQH143K3Y1sd2XVu9wtqxJRvybCfAetjUrMMco6r3v9qZTBeXiBZkS8JxWbcGJZyio8TrZtm6pk",
            "bzG8SYt1sxwNLh3Wx7to5pgiVFU"
        ],
        [
            "xprv9s21ZrQH143K3CSnQNYC3MqAAqHwxeTLhDbhF43A4ss4ciWNmCY9zQGvAKUSqVUf2vPHBTSE1rB2pg4a",
            "vopqSiLVzXEU8KziNnVPauTqLRo"
        ],
        [
            "xprv9s21ZrQH143K2WFF16X85T2QCpndrGwx6GueB72Zf3AHwHJaknRXNF37ZmDrtHrrLSHvbuRejXcnYxoZ",
            "KvRquTPyp2JiNG3XcjQyzSEgqCB"
        ],
        [
            "xprv9s21ZrQH143K2oZ9stBYpoaZ2ktHj7jLz7iMqpgg1En8kKFTXJHsjxry1JbKH19YrDTicVwKPehFKTbm",
            "axgVEc5TpHdS1aYhB2s9aFJBeJH"
        ],
        [
            "xprv9s21ZrQH143K3uT8eQowUjsxrmsA9YUuQQK1RLqFufzybxD6DH6gPY7NjJ5G3EPHjsWDrs9iivSbmvjc",
            "9DQJbJGatfa9pv4MZ3wjr8qWPAK"
        ],
        [
            "xprv9s21ZrQH143K2XTAhys3pMNcGn261Fi5Ta2Pw8PwaVPhg3D8DWkzWQwjTJfskj8ofb81i9NP2cUNKxwj",
            "ueJHHMQAnxtivTA75uUFqPFeWzk"
        ],
        [
            "xprv9s21ZrQH143K3FperxDp8vFsFycKCRcJGAFmcV7umQmcnMZaLtZRt13QJDsoS5F6oYT6BB4sS6zmTmyQ",
            "AEkJKxJ7yByDNtRe5asP2jFGhT6"
        ],
        [
            "xprv9s21ZrQH143K3R1SfVZZLtVbXEB9ryVxmVtVMsMwmEyEvgXN6Q84LKkLRmf4ST6QrLeBm3jQsb9gx1uo",
            "23TS7vo3vAkZGZz71uuLCcywUkt"
        ],
        [
            "xprv9s21ZrQH143K2WNnKmssvZYM96VAr47iHUQUTUyUXH3sAGNjhJANddnhw3i3y3pBbRAVk5M5qUGFr4rH",
            "bEWwXgX4qrvrceifCYQJbbFDems"
        ],
        [
            "xprv9s21ZrQH143K4G28omGMogEoYgDQuigBo8AFHAGDaJdqQ99QKMQ5J6fYTMfANTJy6xBmhvsNZ1CJzRZ6",
            "4PWbnTFUn6CDV2FxoMDLXdk95DQ"
        ],
        [
            "xprv9s21ZrQH143K3wtsvY8L2aZyxkiWULZH4vyQE5XkHTXkmx8gHo6RUEfH3Jyr6NwkJhvano7Xb2o6UqFK",
            "WHVo5scE31SGDCAUsgVhiUuUDyh"
        ],
        [
            "xprv9s21ZrQH143K3rEfqSM4QZRVmiMuSWY9wugscmaCjYja3SbUD3KPEB1a7QXJoajyR2T1SiXU7rFVRXMV",
            "9XdYVSZe7JoUXdP4SRHTxsT1nzm"
        ],
        [
            "xprv9s21ZrQH143K2QWV9Wn8Vvs6jbqfF1YbTCdURQW9dLFKDovpKaKrqS3SEWsXCu6ZNky9PSAENg6c9AQY",
            "Hcg4PjopRGGKmdD313ZHszymnps"
        ],
        [
            "xprv9s21ZrQH143K4aERa2bq7559eMCCEs2QmmqVjUuzfy5eAeDX4mqZffkYwpzGQRE2YEEeLVRoH4CSHxia",
            "nrFaVnMN2RYaPUZJhJx8S5j6puX"
        ],
        [
            "xprv9s21ZrQH143K39rnQJknpH1WEPFJrzmAqqasiDcVrNuk926oizzJDDQkdiTvNPr2FYDYzWgiMiC63Ymf",
            "PAa2oPyNB23r2g7d1yiK6WpqaQS"
        ]
    ];

    #[test]
    fn test_checksum() {
        assert_eq!(vec![0x00; 32].checksum(), 0x66);
        assert_eq!(vec![0x00; 28].checksum(), 0x3a);
        assert_eq!(vec![0x00; 24].checksum(), 0x9c);
        assert_eq!(vec![0x00; 20].checksum(), 0xd8);
        assert_eq!(vec![0x00; 16].checksum(), 0x30);
        assert_eq!(vec![0xff; 32].checksum(), 0xaf);
        assert_eq!(vec![0xff; 28].checksum(), 0x32);
        assert_eq!(vec![0xff; 24].checksum(), 0x44);
        assert_eq!(vec![0xff; 20].checksum(), 0x98);
        assert_eq!(vec![0xff; 16].checksum(), 0x50);
    }

    #[test]
    fn test_checksum_validation() {
        assert!([[0x00; 32], [0x66; 32]].concat()[..33].checksum_validation());
        assert!([[0x00; 28], [0x3a; 28]].concat()[..29].checksum_validation());
        assert!([[0x00; 24], [0x9c; 24]].concat()[..25].checksum_validation());
        assert!([[0x00; 20], [0xd8; 20]].concat()[..21].checksum_validation());
        assert!([[0x00; 16], [0x30; 16]].concat()[..17].checksum_validation());
    }

    #[test]
    fn test_choose_lang() {
        assert_eq!("c".choose_lang().unwrap(), chinese_simplified::CS);
        assert_eq!("e".choose_lang().unwrap(), english::E);
        assert_eq!("f".choose_lang().unwrap(), french::F);
        assert_eq!("i".choose_lang().unwrap(), italian::I);
        assert_eq!("j".choose_lang().unwrap(), japanese::J);
        assert_eq!("k".choose_lang().unwrap(), korean::K);
        assert_eq!("o".choose_lang().unwrap(), portuguese::P);
        assert_eq!("s".choose_lang().unwrap(), spanish::S);
        assert_eq!("t".choose_lang().unwrap(), chinese_traditional::CT);
        assert_eq!("z".choose_lang().unwrap(), czech::CZ);
        assert_eq!("".choose_lang().unwrap(), english::E);
        assert_eq!(
            "anything else".choose_lang().unwrap_err(),
            Error::Flag(String::from("anything else"))
        );
    }

    #[test]
    fn test_detect_lang() {
        assert_eq!(
            [
                "同", "考", "柳", "难", "昨", "玻", "渡", "鱼", "住", "理", "箱", "亩"
            ].join(SPC).detect_lang().unwrap(),
            chinese_simplified::CS
        );
        assert_eq!(
            [
                "同", "考", "柳", "難", "昨", "玻", "渡", "魚", "住", "理", "箱", "畝"
            ].join(SPC).detect_lang().unwrap(),
            chinese_traditional::CT
        );
        assert_eq!(
            [
                "army", "van", "defense", "carry", "jealous", "true", "garbage", "claim", "echo",
                "media", "make", "crunch"
            ].join(SPC).detect_lang().unwrap(),
            english::E
        );
        assert_eq!(
            [
                "army", "van", "offense", "carry", "jealous", "true", "garbage", "claim", "echo",
                "media", "make", "crunch"
            ].join(SPC).detect_lang().unwrap_err(),
            Error::Word(String::from("offense"), String::from("defense"))
        );
        assert_eq!(
            [
                "balvan", "kopnout", "soucit", "herna", "velmoc", "pecka", "rozinka", "karamel",
                "hymna", "blud", "paruka", "porod"
            ].join(SPC).detect_lang().unwrap(),
            czech::CZ
        );
        assert_eq!(
            [
                "adhésif", "éligible", "puzzle", "citoyen", "stable", "maintien","peigne",
                "dioxyde", "cueillir", "alliage", "lundi", "mythique"
            ].join(SPC).detect_lang().unwrap(),
            french::F
        );
        assert_eq!(
            [
                "agonismo", "fenomeno", "schiena", "corolla", "tampone", "orzo", "ridurre",
                "epilogo", "dinnanzi", "ametista", "orizzonte", "piombo"
            ].join(SPC).detect_lang().unwrap(),
            italian::I
        );
        assert_eq!(
            [
                "あらし", "こぜん", "はなす", "ぎしき", "まよう", "ちしりょう", "にんにく",
                "けまり", "くせげ", "いせき", "ちえん", "てみやげ"
            ].join(IDS).detect_lang().unwrap(),
            japanese::J
        );
        assert_eq!(
            [
                "강력히", "별도", "주민", "덩어리", "텔레비전", "예금",
                "장식", "반드시", "명함", "결승", "영웅", "유행"
            ].join(SPC).detect_lang().unwrap(),
            korean::K
        );
        assert_eq!(
            [
                "acusador", "donzela", "pulmonar", "carvalho", "sozinho", "loteria", "panfleto",
                "dedal", "colmeia", "ajoelhar", "lixeira", "molusco"
            ].join(SPC).detect_lang().unwrap(),
            portuguese::P
        );
        assert_eq!(
            [
                "afinar", "espuma", "regir", "cinco", "tarro", "motivo", "pisar", "dragón",
                "cuento", "alteza", "moño", "oír"
            ].join(SPC).detect_lang().unwrap(),
            spanish::S
        );
        assert_eq!(
            "thisisnotavalidwordinanylanguage".detect_lang().unwrap_err(),
            Error::Word(String::from("thisisnotavalidwordinanylanguage"), String::new())
        );
        // in case of 50/50 of any other language with english, assume english
        assert_eq!(
            [
                "digitar", "subtrair", "obrigado", "prancha", "plumagem", "vasilha", "fun",
                "trash", "sport", "alley", "decade", "spin"
            ].join(SPC).detect_lang().unwrap_err(),
            Error::Word(String::from("digitar"), String::from("digital"))
        );
        // stays with english
        assert_eq!(
            [
                "triagem", "subtrair", "obrigado", "prancha", "plumagem", "test", "fun", "trash",
                "sport", "alley", "decade", "spin"
            ].join(SPC).detect_lang().unwrap_err(),
            Error::Word(String::from("triagem"), String::from("trigger"))
        );
        // decides it's portuguese and suggest accordingly
        assert_eq!(
            [
                "firmeza", "subtrair", "obrigado", "prancha", "plumagem", "acima", "abaixo",
                "eager", "sport", "alley", "test", "spin"
            ].join(SPC).detect_lang().unwrap_err(),
            Error::Word(String::from("eager"), String::from("exagero"))
        );
    }

    #[test]
    fn test_extended_key() {
        let mut bytes = [0x00; 64];
        for (idx, seed) in TV_SEED.iter().enumerate() {
            bytes[..].copy_from_slice(&seed.concat().hex_bytes().unwrap());
            assert_eq!( bytes.extended_key(&XPRV).unwrap(), TV_XKEY[idx].concat());
        }
    }

    #[test]
    fn test_handle_arguments() {
        for (idx, ent) in TV_ENTROPY.iter().enumerate() {
            assert_eq!(
                handle_arguments(&init_clap().get_matches_from(vec!["", ent])).unwrap(), ()
            );
            assert!(
                handle_arguments(
                    &init_clap().get_matches_from(vec!["", &TV_SEED[idx].concat()])
                ).is_ok()
            );
            let mut args = vec![""];
            let mnemonic = TV_MNEMONIC[idx].join(SPC);
            args.append(
                &mut mnemonic.trim_end()
                    .split_whitespace()
                    .collect::<Vec<&str>>()
            );
            assert!(handle_arguments(&init_clap().get_matches_from(args)).is_ok(),);
        }
        let argument = ["a"; LEN_SEED + 1];
        for nb_hex_char in 1..LEN_SEED + 1 {
            if LEN_ENT.contains(&nb_hex_char) || nb_hex_char == LEN_SEED {
                assert!(
                    handle_arguments(
                        &init_clap().get_matches_from(&["", &argument[..nb_hex_char].concat()])
                    ).is_ok()
                );
            } else {
                assert_eq!(
                    handle_arguments(
                        &init_clap().get_matches_from(&["", &argument[..nb_hex_char].concat()])
                    ).unwrap_err(),
                    Error::Data(
                        argument[..nb_hex_char].concat(),
                        argument[..nb_hex_char].concat().len()
                    )
                );
            }
        }
        assert!(
            handle_arguments(
                &init_clap().get_matches_from(&[
                    "", "あらし", "こぜん", "はなす", "ぎしき", "まよう", "ちしりょう", "にんにく",
                    "けまり", "くせげ", "いせき", "ちえん", "てみやげ"
                ])
            ).is_ok()
        );
        assert_eq!( // first: binary name
            handle_arguments(&init_clap().get_matches_from(&[""; 3])).unwrap_err(),
            Error::NbWords(2)
        );
        assert_eq!(
            handle_arguments(&init_clap().get_matches_from(&[""; 10])).unwrap_err(),
            Error::NbWords(9)
        );
        assert_eq!(
            handle_arguments(
                &init_clap().get_matches_from(&["", &["a"; 32].concat(), "-g"])
            ).unwrap_err(),
            Error::Flag(String::from("g"))
        );
        assert_eq!(
            handle_arguments(
                &init_clap().get_matches_from(&["", "-g", "invalid", "number", "of", "words"])
            ).unwrap_err(),
            Error::NbWords(4)
        );
        assert_eq!(
            handle_arguments(
                &init_clap().get_matches_from(&[
                    "", "-g", "invalid", "number", "of", "words", "5", "6", "7", "8", "9", "10",
                    "11"
                ])
            ).unwrap_err(),
            Error::NbWords(11)
        );
        assert_eq!(
            handle_arguments(
                &init_clap().get_matches_from(&[
                    "", "-j", "invalid", "number", "of", "words", "5", "6", "7", "8", "9", "10",
                    "11"
                ])
            ).unwrap_err(),
            Error::NbWords(11)
        );
    }

    #[test]
    fn test_hex_bytes() {
        assert_eq!("0488ade4".hex_bytes().unwrap(), XPRV);
        assert_eq!("BABACA".hex_bytes().unwrap(), [0xba, 0xba, 0xca]);
    }

    #[test]
    fn test_hex_string() {
        assert_eq!(XPRV.hex_string(), String::from("0488ade4"));
        assert_eq!([0xba, 0xba, 0xca].hex_string(), String::from("babaca"));
    }

    #[test]
    fn test_init_clap() {
        for (idx, ent) in TV_ENTROPY.iter().enumerate() {
            assert!(&init_clap().get_matches_from_safe(vec!["", ent]).is_ok());
            assert!(&init_clap().get_matches_from_safe(vec!["", &TV_SEED[idx].concat()]).is_ok());
            let mut args = vec![""];
            let mnemonic = TV_MNEMONIC[idx].join(SPC);
            args.append(
                &mut mnemonic.trim_end()
                    .split_whitespace()
                    .collect::<Vec<&str>>()
            );
            assert!(&init_clap().get_matches_from_safe(args).is_ok());
        }
        let mnemonic = TV_MNEMONIC[0].join(SPC);
        for flag in &LANG_FLAGS {
            let raw_flag = ["-", flag].concat();
            let mut args = vec!["", &raw_flag];
            args.append(
                &mut mnemonic.trim_end()
                    .split_whitespace()
                    .collect::<Vec<&str>>()
            );
            assert!(&init_clap().get_matches_from_safe(&args).is_ok());
        }
        assert!(
            &init_clap().get_matches_from_safe(
                &[
                    "", "あらし", "こぜん", "はなす", "ぎしき", "まよう", "ちしりょう",
                    "にんにく", "けまり", "くせげ", "いせき", "ちえん", "てみやげ"
                ]
            ).is_ok()
        );
    }

    #[test]
    fn test_invalid_word() {
        for mnemonic in &TV_MNEMONIC {
            assert_eq!(
                mnemonic.join(SPC).trim_end().invalid_word(&english::E), None
            );
        }
        let cs = ["同", "考", "柳", "难", "昨", "玻", "渡", "鱼", "住", "理", "箱", "亩"];
        assert_eq!(cs.join(SPC).invalid_word(&chinese_simplified::CS), None);
        assert_eq!(
            cs.join(SPC).invalid_word(&chinese_traditional::CT),
            Some((String::from("难"), 75.0))
        );
        assert_eq!("\u{20}\u{3000}".invalid_word(&english::E), Some((String::from(""), 0.0)));
        assert_eq!(
            [
                "firmeza", "subtrair", "obrigado", "prancha", "plumagem", "vasilha", "fun",
                "trash", "sport", "alley", "decade", "spin"
            ].join(SPC).invalid_word(&english::E),
            Some((String::from("firmeza"), 50.0))
        );
        assert_eq!(
            [
                "firmeza", "subtrair", "obrigado", "prancha", "plumagem", "vasilha", "fun",
                "trash", "sport", "alley", "decade", "spin"
            ].join(SPC).invalid_word(&portuguese::P),
            Some((String::from("fun"), 50.0))
        );
        assert_eq!(
            [
                "firmeza", "subtrair", "obrigado", "among", "push", "test", "fun", "trash",
                "sport", "alley", "decade", "spin"
            ].join(SPC).invalid_word(&english::E),
            Some((String::from("firmeza"), 75.0))
        );
        assert_eq!(
            [
                "firmeza", "subtrair", "obrigado", "prancha", "plumagem", "vasilha", "adega",
                "acima", "abaixo", "alley", "decade", "spin"
            ].join(SPC).invalid_word(&portuguese::P),
            Some((String::from("alley"), 75.0))
        );
    }

    #[test]
    fn test_is_hex() {
        assert!("0123456789abcdf".is_hex());
        assert!("ABCDEF".is_hex());
        assert!(!"ghijkl".is_hex());
        assert!(!"'!@#$%&*;:><?".is_hex());
    }

    #[test]
    fn test_last_word() {
        assert_eq!(
            [
                "小", "又", "驻", "库", "单", "酒", "逼", "名", "受", "捕", "游", "姻", "乱", "充",
                "邵", "警", "接", "龙", "含", "请", "梯", "垂", "暖"
            ].join(SPC).last_word().unwrap(),
            ["它", "严", "送", "祖", "辩", "潜", "炒", "疯"]
        );
        assert_eq!(
            [
                "籌", "驟", "意", "厘", "紀", "玻", "雷", "暗", "奇", "決", "層", "毒", "疆", "寸",
                "隙", "佔", "筒", "蘭", "環", "槽", "夾", "浙", "唯"
            ].join(SPC).last_word().unwrap(),
            ["它", "候", "怎", "孫", "俄", "珍", "鞏", "枯"]
        );
        assert_eq!(
            [
                "svisle", "mrzutost", "astronom", "jalovec", "kalamita", "podraz", "obrys", "slib",
                "emoce", "plamen", "skrz", "pasivita", "panna", "zavalit", "podepsat", "nymfa",
                "bitva", "nelibost", "samizdat", "celer", "granule", "nejprve", "charita"
            ].join(SPC).last_word().unwrap(),
            ["adresa", "epopej", "krystal", "navenek", "pastelka", "prahory", "tajga", "zezadu"]
        );
        assert_eq!(
            [
                "vital", "virus", "wait", "nuclear", "foil", "reopen", "portion", "conduct",
                "pudding", "much", "valid", "welcome", "travel", "spray", "valley", "actress",
                "fatigue", "farm", "major", "hero", "real", "setup", "nut"
            ].join(SPC).last_word().unwrap(),
            ["brass", "clip", "father", "hockey", "matrix", "pistol", "sweet", "utility"]
        );
        assert_eq!(
            [
                "émulsion", "pratique", "prouesse", "instinct", "onduler", "pupitre", "minimal",
                "durcir", "unique", "cassure", "requin", "score", "viande", "jouissif", "ogive",
                "amertume", "filou", "xénon", "aider", "astre", "matière", "éolien", "odeur"
            ].join(SPC).last_word().unwrap(),
            [
                "bermuda", "chéquier", "élaborer", "ficeler", "logique", "patience", "refaire",
                "torpille"
            ]
        );
        assert_eq!(
            [
                "lancetta", "eppure", "riunione", "pennuto", "abolire", "vano", "topazio",
                "annidato", "guanto", "staffa", "stiletto", "inoltrare", "parola", "costante",
                "melodia", "attorno", "corredo", "inoltrare", "gazebo", "evaso", "raffica",
                "decreto", "aspro"
            ].join(SPC).last_word().unwrap(),
            ["angelo", "brodo", "emanato", "lumaca", "ottagono", "restauro", "situato", "zufolo"]
        );
        assert_eq!(
            [
                "ぐんたい", "うりきれ", "おうべい", "くださる", "たもつ", "まさつ", "しへい",
                "そんちょう", "けんにん", "しはらい", "さよく", "おんどけい", "えおり", "ふあん",
                "ぬくもり", "きくらげ", "さくひん", "ついたち", "かがみ", "ほせい", "むさぼる",
                "おくさま", "なにわ"
            ].join(IDS).last_word().unwrap(),
            [
                "いけばな", "きちょう", "けつじょ", "しらせる", "たいこ", "なまえ", "ふきん",
                "りんご"
            ]
        );
        assert_eq!(
            [
                "여학생", "사나이", "졸음", "영향", "교환", "콘서트",
                "클래식", "평소", "소금", "정성", "월급", "먼지", "난방",
                "글씨", "피아노", "공사", "전문", "이력서", "고급", "애정",
                "화학", "이것", "택시"
            ].join(SPC).last_word().unwrap(),
            [
                "그늘", "당연히", "보장", "신문", "연세", "작업", "친척",
                "피망"
            ]
        );
        assert_eq!(
            [
                "drenagem", "elogiar", "repudiar", "bagagem", "despesa", "cebola", "acomodar",
                "negrito", "impacto", "charme", "enchente", "indireto", "roupa", "timidez",
                "miolo", "gabarito", "intocado", "provador", "superior", "ciclone", "sondar",
                "amolador", "populoso"
            ].join(SPC).last_word().unwrap(),
            [
                "alterar", "captador", "dueto", "expulsar", "manada", "muscular", "rebolar",
                "voleibol"
            ]
        );
        let s = [
            "ficha", "ahogo", "fecha", "premio", "marfil", "gris", "hacer", "cadáver", "caída",
            "vinagre", "gato", "defensa", "bomba", "ronco", "romper", "siete", "lote", "morro",
            "salmón", "escudo", "pelar", "acoso", "alzar"
        ];
        assert_eq!(
            s.join(SPC).last_word().unwrap(),
            ["barba", "buzón", "escolar", "jornada", "naval", "pregunta", "rencor", "tubo"]
        );
        assert_eq!(s[..20].join(SPC).last_word().unwrap().len(), 16);
        assert_eq!(s[..17].join(SPC).last_word().unwrap().len(), 32);
        assert_eq!(s[..14].join(SPC).last_word().unwrap().len(), 64);
        assert_eq!(s[..11].join(SPC).last_word().unwrap().len(), 128);
        assert_eq!(
            s[..7].join(SPC).last_word().unwrap_err(), Error::NbWords(7)
        );
    }

    #[test]
    fn test_mnemonic_bytes() {
        assert_eq!(
            [
                "agree", "expire", "shallow", "cram", "timber", "neglect", "regular", "eight",
                "detail", "any", "name", "pass", "fiber", "cook", "museum", "column", "inflict",
                "sentence", "gaze", "audit", "simple", "slender", "hub", "addict"
            ].join(SPC).mnemonic_bytes().unwrap(),
            [
                0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46,
                0x4b, 0x50, 0x55, 0x5a, 0x5f, 0x64, 0x69, 0x6e, 0x73, 0x78, 0x7d, 0x82, 0x87, 0x8c,
                0x91, 0x96, 0x9b, 0xa0, 0x1a
            ]
        );
        assert_eq!(
            [
                "agree", "expire", "shallow", "cram", "timber", "neglect", "regular", "eight",
                "detail", "any", "name", "pass", "fiber", "cook", "museum", "column", "inflict",
                "sentence", "gaze", "audit", "solid"
            ].join(SPC).mnemonic_bytes().unwrap(),
            [
                0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46,
                0x4b, 0x50, 0x55, 0x5a, 0x5f, 0x64, 0x69, 0x6e, 0x73, 0x78, 0x7d, 0x82, 0x87, 0x8c,
                0xea
            ]
        );
        assert_eq!(
            [
                "agree", "expire", "shallow", "cram", "timber", "neglect", "regular", "eight",
                "detail", "any", "name", "pass", "fiber", "cook", "museum", "column", "inflict",
                "setup"
            ].join(SPC).mnemonic_bytes().unwrap(),
            [
                0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46,
                0x4b, 0x50, 0x55, 0x5a, 0x5f, 0x64, 0x69, 0x6e, 0x73, 0x78, 0x90
            ]
        );
        assert_eq!(
            [
                "agree", "expire", "shallow", "cram", "timber", "neglect", "regular", "eight",
                "detail", "any", "name", "pass", "fiber", "cook", "move"
            ].join(SPC).mnemonic_bytes().unwrap(),
            [
                0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46,
                0x4b, 0x50, 0x55, 0x5a, 0x5f, 0x64, 0x30
            ]
        );
        let bytes = [
            0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46,
            0x4b, 0x50, 0xf0
        ];
        assert_eq!(
            [
                "agree", "expire", "shallow", "cram", "timber", "neglect", "regular", "eight",
                "detail", "any", "name", "peanut"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            ["同", "考", "柳", "难", "昨", "玻", "渡", "鱼", "住", "理", "箱", "亩"]
                .join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            ["同", "考", "柳", "難", "昨", "玻", "渡", "魚", "住", "理", "箱", "畝"]
                .join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "balvan", "kopnout", "soucit", "herna", "velmoc", "pecka", "rozinka", "karamel",
                "hymna", "blud", "paruka", "porod"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "adhésif", "éligible", "puzzle", "citoyen", "stable", "maintien", "peigne",
                "dioxyde", "cueillir", "alliage", "lundi", "mythique"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "agonismo", "fenomeno", "schiena", "corolla", "tampone", "orzo", "ridurre",
                "epilogo", "dinnanzi", "ametista", "orizzonte", "piombo"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "あらし", "こぜん", "はなす", "ぎしき", "まよう", "ちしりょう", "にんにく",
                "けまり", "くせげ", "いせき", "ちえん", "てみやげ"
            ].join(IDS).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "강력히", "별도", "주민", "덩어리", "텔레비전", "예금",
                "장식", "반드시", "명함", "결승", "영웅", "유행"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "acusador", "donzela", "pulmonar", "carvalho", "sozinho", "loteria", "panfleto",
                "dedal", "colmeia", "ajoelhar", "lixeira", "molusco"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
        assert_eq!(
            [
                "afinar", "espuma", "regir", "cinco", "tarro", "motivo", "pisar", "dragón",
                "cuento", "alteza", "moño", "oír"
            ].join(SPC).mnemonic_bytes().unwrap(),
            bytes
        );
    }

    #[test]
    fn test_mnemonic_string() {
        for (idx, entropy) in TV_ENTROPY.iter().enumerate() {
            assert_eq!(
                entropy.hex_bytes().unwrap()
                .mnemonic_string(&english::E).unwrap(),
                TV_MNEMONIC[idx].join(SPC).trim_end()
            );
        }
        let bytes = [
            0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46,
            0x4b, 0x50
        ];
        assert_eq!(
            bytes.mnemonic_string(&english::E).unwrap(),
            [
                "agree", "expire", "shallow", "cram", "timber", "neglect", "regular", "eight",
                "detail", "any", "name", "peanut"
            ].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&chinese_simplified::CS).unwrap(),
            ["同", "考", "柳", "难", "昨", "玻", "渡", "鱼", "住", "理", "箱", "亩"].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&chinese_traditional::CT).unwrap(),
            ["同", "考", "柳", "難", "昨", "玻", "渡", "魚", "住", "理", "箱", "畝"].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&czech::CZ).unwrap(),
            [
                "balvan", "kopnout", "soucit", "herna", "velmoc", "pecka", "rozinka", "karamel",
                "hymna", "blud", "paruka", "porod"
            ].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&french::F).unwrap(),
            [
                "adhésif", "éligible", "puzzle", "citoyen", "stable", "maintien", "peigne",
                "dioxyde", "cueillir", "alliage", "lundi", "mythique"
            ].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&italian::I).unwrap(),
            [
                "agonismo", "fenomeno", "schiena", "corolla", "tampone", "orzo", "ridurre",
                "epilogo", "dinnanzi", "ametista", "orizzonte", "piombo"
            ].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&japanese::J).unwrap(),
            [
                "あらし", "こぜん", "はなす", "ぎしき", "まよう", "ちしりょう", "にんにく",
                "けまり", "くせげ", "いせき", "ちえん", "てみやげ"
            ].join(IDS) // ideographic space when showing japanese mnemonic
        );
        assert_eq!(
            bytes.mnemonic_string(&korean::K).unwrap(),
            [
                "강력히", "별도", "주민", "덩어리", "텔레비전", "예금",
                "장식", "반드시", "명함", "결승", "영웅", "유행"
            ].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&portuguese::P).unwrap(),
            [
                "acusador", "donzela", "pulmonar", "carvalho", "sozinho", "loteria", "panfleto",
                "dedal", "colmeia", "ajoelhar", "lixeira", "molusco"
            ].join(SPC)
        );
        assert_eq!(
            bytes.mnemonic_string(&spanish::S).unwrap(),
            [
                "afinar", "espuma", "regir", "cinco", "tarro", "motivo", "pisar", "dragón",
                "cuento", "alteza", "moño", "oír"
            ].join(SPC)
        );
        assert_eq!(
            bytes[1..].mnemonic_string(&english::E).unwrap_err(),
            Error::NbBytes(15)
        );
    }

    #[test]
    fn test_seed_bytes() {
        for (idx, mnemonic) in TV_MNEMONIC.iter().enumerate() {
            assert_eq!(
                mnemonic.join(SPC)
                    .trim_end()
                    .seed_bytes(TV_PASS)
                    .hex_string(),
                TV_SEED[idx].concat()
            );
        }
        assert_ne!(
            TV_MNEMONIC[0].join(SPC)
                .trim_end()
                .seed_bytes("LEDGER")
                .hex_string(),
            TV_SEED[0].concat()
        );
    }

    #[test]
    fn test_show_entropy() {
        for entropy in &TV_ENTROPY {
            assert!(entropy.show_entropy("j", "バンドメイド").is_ok());
        }
    }

    #[test]
    fn test_show_last() {
        for mnemonic in &TV_MNEMONIC {
            assert!(mnemonic[1..].join(SPC).trim_end().show_last().is_ok());
        }
    }

    #[test]
    fn test_show_mnemonic() {
        for mnemonic in &TV_MNEMONIC {
            assert!(mnemonic.join(SPC).trim_end().show_mnemonic(TV_PASS).is_ok());
        }
    }

    #[test]
    fn test_show_permutation() {
        // it will run "forever"
    }

    #[test]
    fn test_show_seed() {
        for seed in &TV_SEED {
            assert!(seed.concat().show_seed().is_ok());
        }
    }

    #[test]
    fn test_show_trasposition() {
        for mnemonic in &TV_MNEMONIC {
            assert!(
                mnemonic.join(SPC)
                    .trim_end()
                    .show_transposition("j", "くるっぽー！").is_ok()
            );
        }
    }

    #[test]
    fn test_suggestion() {
        // no suggestion for chinese because each word is a single symbol
        assert_eq!("push".suggestion(&czech::CZ), "puch");
        assert_eq!("offense".suggestion(&english::E), "defense");
        assert_eq!("trooper".suggestion(&french::F), "tremper");
        assert_eq!("office".suggestion(&italian::I), "offrire");
        assert_eq!("てみやげう".suggestion(&japanese::J), "てみやげ");
        assert_eq!("승용추".suggestion(&korean::K), "승용차");
        assert_eq!("motif".suggestion(&portuguese::P), "motim");
        assert_eq!("grass".suggestion(&spanish::S), "grasa");
        for wordlist in &WORDLISTS {
            assert_eq!("notintendedtohavesuggestioninanylanguage".suggestion(&wordlist), "");
            assert_eq!("%".suggestion(&wordlist), "");
        }
    }

    #[test]
    fn test_transposition() {
        let e = [
            "army", "van", "defense", "carry", "jealous", "true", "garbage", "claim", "echo",
            "media", "make", "crunch"
        ];
        let c = ["点", "挡", "眼", "器", "哥", "舒", "久", "示", "止", "累", "夏", "便"];
        assert_eq!(e.join(SPC).transposition("c").unwrap(), c.join(SPC));
        let f = [
            "amour", "troupeau", "couteau", "brèche", "gustatif", "tenaille", "exécuter",
            "capuche", "dicter", "lagune", "jaune", "cogner"
        ];
        assert_eq!(c.join(SPC).transposition("f").unwrap(), f.join(SPC));
        let i = [
            "anca", "unisono", "delta", "busta", "maiolica", "torrone", "globulo", "centesimo",
            "endemico", "nome", "muto", "crostata"
        ];
        assert_eq!(f.join(SPC).transposition("i").unwrap(), i.join(SPC));
        let j = [
            "いつか", "ゆうべ", "ぐあい", "おしえる", "せびろ", "むすめ", "さわる", "かいわ",
            "けなみ", "たたく", "たいふう", "きなこ"
        ];
        assert_eq!(i.join(SPC).transposition("j").unwrap(), j.join(IDS));
        let k = [
            "경주", "한복", "매년", "깍두기", "승용차", "포장", "사전",
            "농업", "바닷가", "얼음", "액수", "뒷산"
        ];
        assert_eq!(j.join(IDS).transposition("k").unwrap(), k.join(SPC));
        let o = [
            "alfinete", "trilogia", "citar", "berro", "graveto", "teimar", "evacuar", "broa",
            "debitar", "jurista", "irritado", "cerrado"
        ];
        assert_eq!(k.join(SPC).transposition("o").unwrap(), o.join(SPC));
        let s = [
            "amistad", "túnica", "costa", "broma", "juicio", "toalla", "furgón", "caña",
            "domingo", "masivo", "maldad", "código"
        ];
        assert_eq!(o.join(SPC).transposition("s").unwrap(), s.join(SPC));
        let t = ["點", "擋", "眼", "器", "哥", "舒", "久", "示", "止", "累", "夏", "便"];
        assert_eq!(o.join(SPC).transposition("t").unwrap(), t.join(SPC));
        let z = [
            "bouda", "vzpoura", "hrdina", "doufat", "nejprve", "vrhat", "limetka", "facka",
            "kapela", "operace", "ofsajd", "hoboj"
        ];
        assert_eq!(t.join(SPC).transposition("z").unwrap(), z.join(SPC));
        assert_eq!(z.join(SPC).transposition("e").unwrap(), e.join(SPC));
        assert_eq!(e.join(SPC).transposition("e").unwrap_err(), Error::SameLang);
    }

    #[test]
    fn test_unchecked_len() {
        assert_eq!([0x00; 33].unchecked_len(), 32);
        assert_eq!([0x00; 32].unchecked_len(), 32);
        assert_eq!([0x00; 29].unchecked_len(), 28);
        assert_eq!([0x00; 28].unchecked_len(), 28);
        assert_eq!([0x00; 25].unchecked_len(), 24);
        assert_eq!([0x00; 24].unchecked_len(), 24);
        assert_eq!([0x00; 21].unchecked_len(), 20);
        assert_eq!([0x00; 20].unchecked_len(), 20);
        assert_eq!([0x00; 17].unchecked_len(), 16);
        assert_eq!([0x00; 16].unchecked_len(), 16);
    }
}
