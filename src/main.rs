// mnemonic39/src/main.rs
// 20210706
// ceca69ec8e1bcad6c6d79e1dcf7214ff67766580a62b7d19a6fb094c97b4f2dc

//! Show information about a list of mnemonic words or entropy and can generate
//! a list of valid last words for a incomplete mnemonic informed, or create a
//! transposition of a mnemonic in one language to another. It can create a
//! valid mnemonic based on a valid list of words but with invalid checksum.

use mnemonic39::{init_clap, handle_arguments};

fn main() {
    handle_arguments(&init_clap().get_matches()).unwrap_or_else(|err| {
        clap::Error::with_description(
            &err.message(),
            clap::ErrorKind::InvalidValue
        ).exit();
    });
}
