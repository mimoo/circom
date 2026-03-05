use program_structure::abstract_syntax_tree::ast::*;
use program_structure::constants::UsefulConstants;
use program_structure::error_definition::Report;
use program_structure::program_archive::ProgramArchive;
use rusqlite::{params, Connection};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use type_analysis::check_types::check_types;

use crate::VERSION;

struct CallgraphArgs {
    input_file: String,
    db_path: PathBuf,
    link_libraries: Vec<PathBuf>,
    prime: String,
}

/// Check if --callgraph is present in args. Called early from main() to
/// short-circuit before normal circom CLI parsing.
pub fn maybe_run() -> Option<Result<(), ()>> {
    let args: Vec<String> = std::env::args().collect();
    let pos = args.iter().position(|a| a == "--callgraph")?;
    Some(run(&args, pos))
}

fn run(args: &[String], callgraph_pos: usize) -> Result<(), ()> {
    use ansi_term::Colour;

    let parsed = parse_args(args, callgraph_pos).map_err(|e| {
        eprintln!("{}", Colour::Red.paint(e));
    })?;

    let prime = UsefulConstants::new(&parsed.prime).get_p().clone();
    let result = parser::run_parser(
        parsed.input_file,
        VERSION,
        parsed.link_libraries,
        &prime,
        false,
    );
    let mut program_archive = match result {
        Err((file_library, reports)) => {
            Report::print_reports(&reports, &file_library);
            return Err(());
        }
        Ok((archive, warnings)) => {
            Report::print_reports(&warnings, &archive.file_library);
            archive
        }
    };

    match check_types(&mut program_archive) {
        Err(errs) => {
            Report::print_reports(&errs, program_archive.get_file_library());
            return Err(());
        }
        Ok(warns) => {
            Report::print_reports(&warns, program_archive.get_file_library());
        }
    }

    build_callgraph(&program_archive, &parsed.db_path).map_err(|e| {
        eprintln!("{}", Colour::Red.paint(format!("callgraph error: {}", e)));
    })?;

    println!(
        "{} {}",
        Colour::Green.paint("Callgraph written to"),
        parsed.db_path.display()
    );
    Ok(())
}

fn parse_args(args: &[String], callgraph_pos: usize) -> Result<CallgraphArgs, String> {
    let db_path = args
        .get(callgraph_pos + 1)
        .ok_or("--callgraph requires a path argument")?;

    // Find input file: last positional arg (not a flag or flag value)
    let mut input_file = None;
    let mut link_libraries = Vec::new();
    let mut prime = "bn128".to_string();
    let mut i = 1; // skip argv[0]
    while i < args.len() {
        let arg = &args[i];
        if arg == "--callgraph" {
            i += 2; // skip --callgraph and its value
            continue;
        }
        if arg == "-l" {
            if let Some(path) = args.get(i + 1) {
                link_libraries.push(PathBuf::from(path));
            }
            i += 2;
            continue;
        }
        if arg == "--prime" || arg == "-prime" {
            if let Some(p) = args.get(i + 1) {
                prime = p.clone();
            }
            i += 2;
            continue;
        }
        if arg.starts_with('-') {
            i += 1;
            continue;
        }
        // Positional arg — treat as input file
        input_file = Some(arg.clone());
        i += 1;
    }

    let input_file = input_file.ok_or("No input file specified")?;
    if !Path::new(&input_file).is_file() {
        return Err(format!("Input file does not exist: {}", input_file));
    }

    Ok(CallgraphArgs {
        input_file,
        db_path: PathBuf::from(db_path),
        link_libraries,
        prime,
    })
}

fn build_callgraph(program_archive: &ProgramArchive, db_path: &Path) -> Result<(), String> {
    let conn = Connection::open(db_path).map_err(|e| format!("Failed to open DB: {}", e))?;
    init_db(&conn)?;

    let file_lib = &program_archive.file_library;

    for (name, data) in program_archive.get_templates() {
        let file_id = data.get_file_id();
        let file_name = get_file_name(file_lib, file_id);
        let source = get_source_slice(file_lib, file_id, data.get_body(), name);
        let mut callees = HashSet::new();
        collect_calls_from_statement(data.get_body(), &mut callees);

        conn.execute(
            "INSERT OR REPLACE INTO entities (name, kind, file, source) VALUES (?1, ?2, ?3, ?4)",
            params![name, "template", file_name, source],
        )
        .map_err(|e| format!("Failed to insert template {}: {}", name, e))?;

        for callee in &callees {
            conn.execute(
                "INSERT OR IGNORE INTO calls (caller, callee) VALUES (?1, ?2)",
                params![name, callee],
            )
            .map_err(|e| format!("Failed to insert call edge: {}", e))?;
        }
    }

    for (name, data) in program_archive.get_functions() {
        let file_id = data.get_file_id();
        let file_name = get_file_name(file_lib, file_id);
        let source = get_source_slice(file_lib, file_id, data.get_body(), name);
        let mut callees = HashSet::new();
        collect_calls_from_statement(data.get_body(), &mut callees);

        conn.execute(
            "INSERT OR REPLACE INTO entities (name, kind, file, source) VALUES (?1, ?2, ?3, ?4)",
            params![name, "function", file_name, source],
        )
        .map_err(|e| format!("Failed to insert function {}: {}", name, e))?;

        for callee in &callees {
            conn.execute(
                "INSERT OR IGNORE INTO calls (caller, callee) VALUES (?1, ?2)",
                params![name, callee],
            )
            .map_err(|e| format!("Failed to insert call edge: {}", e))?;
        }
    }

    Ok(())
}

fn init_db(conn: &Connection) -> Result<(), String> {
    conn.execute_batch(
        "CREATE TABLE IF NOT EXISTS entities (
            name TEXT PRIMARY KEY,
            kind TEXT NOT NULL,
            file TEXT NOT NULL,
            source TEXT NOT NULL
        );
        CREATE TABLE IF NOT EXISTS calls (
            caller TEXT NOT NULL,
            callee TEXT NOT NULL,
            PRIMARY KEY (caller, callee)
        );
        CREATE INDEX IF NOT EXISTS idx_calls_callee ON calls(callee);",
    )
    .map_err(|e| format!("Failed to init DB: {}", e))
}

fn get_file_name(
    file_lib: &program_structure::file_definition::FileLibrary,
    file_id: usize,
) -> String {
    file_lib
        .get_files()
        .get(file_id)
        .map(|f| f.name().trim_matches('"').to_string())
        .unwrap_or_else(|| format!("<unknown file {}>", file_id))
}

fn get_source_slice(
    file_lib: &program_structure::file_definition::FileLibrary,
    file_id: usize,
    body: &Statement,
    entity_name: &str,
) -> String {
    let meta = statement_meta(body);
    if let Some(file) = file_lib.get_files().get(file_id) {
        let source = file.source();
        let start = meta.get_start();
        let end = meta.get_end();
        if start <= end && end <= source.len() {
            let before = &source[..start];
            let sig_start = find_definition_start(before, entity_name);
            return source[sig_start..end].to_string();
        }
    }
    String::new()
}

fn find_definition_start(before: &str, name: &str) -> usize {
    let patterns = [format!("template {}", name), format!("function {}", name)];
    let mut best = before.len();
    for pat in &patterns {
        if let Some(pos) = before.rfind(pat.as_str()) {
            if pos < best {
                best = pos;
            }
        }
    }
    if best == before.len() {
        return best;
    }
    let prefix = &before[..best];
    let trimmed = prefix.trim_end();
    if trimmed.ends_with("*/") {
        if let Some(comment_start) = trimmed.rfind("/*") {
            return comment_start;
        }
    }
    let mut pos = best;
    for line in prefix.lines().rev() {
        let t = line.trim();
        if t.starts_with("//") {
            pos = (line.as_ptr() as usize) - (before.as_ptr() as usize);
        } else if t.is_empty() {
            continue;
        } else {
            break;
        }
    }
    pos
}

fn statement_meta(stmt: &Statement) -> &Meta {
    match stmt {
        Statement::IfThenElse { meta, .. }
        | Statement::While { meta, .. }
        | Statement::Return { meta, .. }
        | Statement::InitializationBlock { meta, .. }
        | Statement::Declaration { meta, .. }
        | Statement::Substitution { meta, .. }
        | Statement::MultSubstitution { meta, .. }
        | Statement::UnderscoreSubstitution { meta, .. }
        | Statement::ConstraintEquality { meta, .. }
        | Statement::LogCall { meta, .. }
        | Statement::Block { meta, .. }
        | Statement::Assert { meta, .. } => meta,
    }
}

fn collect_calls_from_statement(stmt: &Statement, callees: &mut HashSet<String>) {
    match stmt {
        Statement::IfThenElse { cond, if_case, else_case, .. } => {
            collect_calls_from_expr(cond, callees);
            collect_calls_from_statement(if_case, callees);
            if let Some(else_case) = else_case {
                collect_calls_from_statement(else_case, callees);
            }
        }
        Statement::While { cond, stmt, .. } => {
            collect_calls_from_expr(cond, callees);
            collect_calls_from_statement(stmt, callees);
        }
        Statement::Return { value, .. } => {
            collect_calls_from_expr(value, callees);
        }
        Statement::InitializationBlock { initializations, .. } => {
            for init in initializations {
                collect_calls_from_statement(init, callees);
            }
        }
        Statement::Declaration { dimensions, .. } => {
            for dim in dimensions {
                collect_calls_from_expr(dim, callees);
            }
        }
        Statement::Substitution { rhe, .. } => {
            collect_calls_from_expr(rhe, callees);
        }
        Statement::MultSubstitution { lhe, rhe, .. } => {
            collect_calls_from_expr(lhe, callees);
            collect_calls_from_expr(rhe, callees);
        }
        Statement::UnderscoreSubstitution { rhe, .. } => {
            collect_calls_from_expr(rhe, callees);
        }
        Statement::ConstraintEquality { lhe, rhe, .. } => {
            collect_calls_from_expr(lhe, callees);
            collect_calls_from_expr(rhe, callees);
        }
        Statement::LogCall { args, .. } => {
            for arg in args {
                if let LogArgument::LogExp(expr) = arg {
                    collect_calls_from_expr(expr, callees);
                }
            }
        }
        Statement::Block { stmts, .. } => {
            for s in stmts {
                collect_calls_from_statement(s, callees);
            }
        }
        Statement::Assert { arg, .. } => {
            collect_calls_from_expr(arg, callees);
        }
    }
}

fn collect_calls_from_expr(expr: &Expression, callees: &mut HashSet<String>) {
    match expr {
        Expression::Call { id, args, .. } => {
            callees.insert(id.clone());
            for arg in args {
                collect_calls_from_expr(arg, callees);
            }
        }
        Expression::BusCall { id, args, .. } => {
            callees.insert(id.clone());
            for arg in args {
                collect_calls_from_expr(arg, callees);
            }
        }
        Expression::AnonymousComp { id, params, signals, .. } => {
            callees.insert(id.clone());
            for p in params {
                collect_calls_from_expr(p, callees);
            }
            for s in signals {
                collect_calls_from_expr(s, callees);
            }
        }
        Expression::InfixOp { lhe, rhe, .. } => {
            collect_calls_from_expr(lhe, callees);
            collect_calls_from_expr(rhe, callees);
        }
        Expression::PrefixOp { rhe, .. } => {
            collect_calls_from_expr(rhe, callees);
        }
        Expression::InlineSwitchOp { cond, if_true, if_false, .. } => {
            collect_calls_from_expr(cond, callees);
            collect_calls_from_expr(if_true, callees);
            collect_calls_from_expr(if_false, callees);
        }
        Expression::ParallelOp { rhe, .. } => {
            collect_calls_from_expr(rhe, callees);
        }
        Expression::ArrayInLine { values, .. } | Expression::Tuple { values, .. } => {
            for v in values {
                collect_calls_from_expr(v, callees);
            }
        }
        Expression::UniformArray { value, dimension, .. } => {
            collect_calls_from_expr(value, callees);
            collect_calls_from_expr(dimension, callees);
        }
        Expression::Variable { .. } | Expression::Number(..) => {}
    }
}
