/// Show information about a list of mnemonic words or entropy and can generate
/// a list of valid last words for a incomplete mnemonic informed, or create a
/// transposition of a mnemonic in one language to another. It can create a
/// valid mnemonic based on a valid list of words but with invalid checksum.
use mnemonic39::{handle_arguments, init_clap};

fn main() {
    handle_arguments(&init_clap().get_matches()).unwrap_or_else(|err| {
        eprintln!("\x1b[31m\x1b[1merror\x1b[m: {}", err);
        std::process::exit(err.status());
    });
}
