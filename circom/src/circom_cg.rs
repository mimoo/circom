use rusqlite::{params, Connection};
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        print_usage();
        process::exit(1);
    }

    let command = &args[1];
    let db_path = args.last().unwrap();

    let conn = Connection::open_with_flags(
        db_path,
        rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY,
    )
    .unwrap_or_else(|e| {
        eprintln!("Failed to open {}: {}", db_path, e);
        process::exit(1);
    });

    match command.as_str() {
        "callees" => {
            if args.len() != 4 {
                eprintln!("Usage: circom-cg callees <name> <db>");
                process::exit(1);
            }
            cmd_callees(&conn, &args[2]);
        }
        "callers" => {
            if args.len() != 4 {
                eprintln!("Usage: circom-cg callers <name> <db>");
                process::exit(1);
            }
            cmd_callers(&conn, &args[2]);
        }
        "source" => {
            if args.len() != 4 {
                eprintln!("Usage: circom-cg source <name> <db>");
                process::exit(1);
            }
            cmd_source(&conn, &args[2]);
        }
        "list" => {
            cmd_list(&conn);
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            print_usage();
            process::exit(1);
        }
    }
}

fn print_usage() {
    eprintln!("Usage: circom-cg <command> [args] <db>");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  callees <name> <db>   List templates/functions called by <name>");
    eprintln!("  callers <name> <db>   List templates/functions that call <name>");
    eprintln!("  source  <name> <db>   Print the source code of <name>");
    eprintln!("  list    <db>          List all templates and functions");
}

fn cmd_callees(conn: &Connection, name: &str) {
    let mut stmt = conn
        .prepare("SELECT c.callee, e.kind, e.file FROM calls c LEFT JOIN entities e ON c.callee = e.name WHERE c.caller = ?1 ORDER BY c.callee")
        .unwrap();
    let rows = stmt
        .query_map(params![name], |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, Option<String>>(1)?,
                row.get::<_, Option<String>>(2)?,
            ))
        })
        .unwrap();

    for row in rows {
        let (callee, kind, file) = row.unwrap();
        let kind = kind.unwrap_or_else(|| "unknown".to_string());
        let file = file.unwrap_or_default();
        println!("{}\t{}\t{}", callee, kind, file);
    }
}

fn cmd_callers(conn: &Connection, name: &str) {
    let mut stmt = conn
        .prepare("SELECT c.caller, e.kind, e.file FROM calls c LEFT JOIN entities e ON c.caller = e.name WHERE c.callee = ?1 ORDER BY c.caller")
        .unwrap();
    let rows = stmt
        .query_map(params![name], |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, Option<String>>(1)?,
                row.get::<_, Option<String>>(2)?,
            ))
        })
        .unwrap();

    for row in rows {
        let (caller, kind, file) = row.unwrap();
        let kind = kind.unwrap_or_else(|| "unknown".to_string());
        let file = file.unwrap_or_default();
        println!("{}\t{}\t{}", caller, kind, file);
    }
}

fn cmd_source(conn: &Connection, name: &str) {
    let result: Result<(String, String, String), _> = conn.query_row(
        "SELECT kind, file, source FROM entities WHERE name = ?1",
        params![name],
        |row| Ok((row.get(0)?, row.get(1)?, row.get(2)?)),
    );
    match result {
        Ok((kind, file, source)) => {
            println!("// {} {} ({})", kind, name, file);
            println!("{}", source);
        }
        Err(_) => {
            eprintln!("Entity '{}' not found", name);
            process::exit(1);
        }
    }
}

fn cmd_list(conn: &Connection) {
    let mut stmt = conn
        .prepare("SELECT name, kind, file FROM entities ORDER BY kind, name")
        .unwrap();
    let rows = stmt
        .query_map([], |row| {
            Ok((
                row.get::<_, String>(0)?,
                row.get::<_, String>(1)?,
                row.get::<_, String>(2)?,
            ))
        })
        .unwrap();

    for row in rows {
        let (name, kind, file) = row.unwrap();
        println!("{}\t{}\t{}", name, kind, file);
    }
}
