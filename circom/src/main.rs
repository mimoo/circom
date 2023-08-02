use circom::start;
use ansi_term::Colour;

use circom::input_user::Input;

fn main() {
    let user_input = Input::new().unwrap();
    dbg!(&user_input);

    let result = start(user_input);
    if result.is_err() {
        eprintln!("{}", Colour::Red.paint("previous errors were found"));
        std::process::exit(1);
    } else {
        println!("{}", Colour::Green.paint("Everything went okay, circom safe"));
        //std::process::exit(0);
    }
}
