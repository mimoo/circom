use crate::{
    file_definition::{FileLocation, FileID},
    error_definition::Report,
    error_code::{ReportCode},
};
use num_bigint::BigInt;
use serde_derive::{Deserialize, Serialize};

#[derive(Clone)]
pub enum Pragma {
    Version(Meta, FileID, Version),
    CustomGates(Meta, FileID),
    Unrecognized,
}

pub trait FillMeta {
    fn fill(&mut self, file_id: usize, elem_id: &mut usize);
}

pub type MainComponent = (Vec<String>, Expression);
pub fn build_main_component(public: Vec<String>, call: Expression) -> MainComponent {
    (public, call)
}

pub type Version = (usize, usize, usize);

#[derive(Clone)]
pub struct Meta {
    pub elem_id: usize,
    pub start: usize,
    pub end: usize,
    pub location: FileLocation,
    pub file_id: Option<usize>,
    pub component_inference: Option<String>,
    type_knowledge: TypeKnowledge,
    memory_knowledge: MemoryKnowledge,
}
impl Meta {
    pub fn new(start: usize, end: usize) -> Meta {
        Meta {
            end,
            start,
            elem_id: 0,
            location: start..end,
            file_id: Option::None,
            component_inference: None,
            type_knowledge: TypeKnowledge::default(),
            memory_knowledge: MemoryKnowledge::default(),
        }
    }
    pub fn change_location(&mut self, location: FileLocation, file_id: Option<usize>) {
        self.location = location;
        self.file_id = file_id;
    }
    pub fn get_start(&self) -> usize {
        self.location.start
    }
    pub fn get_end(&self) -> usize {
        self.location.end
    }
    pub fn get_file_id(&self) -> usize {
        if let Option::Some(id) = self.file_id {
            id
        } else {
            panic!("Empty file id accessed")
        }
    }
    pub fn get_memory_knowledge(&self) -> &MemoryKnowledge {
        &self.memory_knowledge
    }
    pub fn get_type_knowledge(&self) -> &TypeKnowledge {
        &self.type_knowledge
    }
    pub fn get_mut_memory_knowledge(&mut self) -> &mut MemoryKnowledge {
        &mut self.memory_knowledge
    }
    pub fn get_mut_type_knowledge(&mut self) -> &mut TypeKnowledge {
        &mut self.type_knowledge
    }
    pub fn file_location(&self) -> FileLocation {
        self.location.clone()
    }
    pub fn set_file_id(&mut self, file_id: usize) {
        self.file_id = Option::Some(file_id);
    }
}

#[derive(Clone)]
pub struct AST {
    pub meta: Meta,
    pub compiler_version: Option<Version>,
    pub custom_gates: bool,
    pub custom_gates_declared: bool,
    pub includes: Vec<String>,
    pub definitions: Vec<Definition>,
    pub main_component: Option<MainComponent>,
}

impl AST {
    pub fn new(
        meta: Meta,
        pragmas: Vec<Pragma>,
        includes: Vec<String>,
        definitions: Vec<Definition>,
        main_component: Option<MainComponent>,
    ) -> (AST, Vec<Report>) {
        let mut custom_gates = None;
        let mut compiler_version = None;
        let mut reports = Vec::new();
        for p in pragmas {
            match p {
                // TODO: don't panic
                Pragma::Version(location, file_id, ver) => match compiler_version {
                    Some(_) => reports.push(produce_report(
                        ReportCode::MultiplePragma,
                        location.start..location.end,
                        file_id,
                    )),
                    None => compiler_version = Some(ver),
                },
                Pragma::CustomGates(location, file_id) => match custom_gates {
                    Some(_) => reports.push(produce_report(
                        ReportCode::MultiplePragma,
                        location.start..location.end,
                        file_id,
                    )),
                    None => custom_gates = Some(true),
                },
                Pragma::Unrecognized => {} //This error is previously handled, and the
                                           //parsing continues to catch more parsing errors.
            }
        }

        let custom_gates_declared = definitions.iter().any(|definition| {
            matches!(definition, Definition::Template { is_custom_gate: true, .. })
        });

        (
            AST {
                meta,
                compiler_version,
                custom_gates: custom_gates.unwrap_or(false),
                custom_gates_declared,
                includes,
                definitions,
                main_component,
            },
            reports,
        )
    }
}

#[derive(Clone)]
pub enum Definition {
    Template {
        meta: Meta,
        name: String,
        args: Vec<String>,
        arg_location: FileLocation,
        body: Statement,
        parallel: bool,
        is_custom_gate: bool,
    },
    Function {
        meta: Meta,
        name: String,
        args: Vec<String>,
        arg_location: FileLocation,
        body: Statement,
    },
    Bus {
        meta: Meta,
        name: String,
        args: Vec<String>,
        arg_location: FileLocation,
        body: Statement,
    },
}
pub fn build_template(
    meta: Meta,
    name: String,
    args: Vec<String>,
    arg_location: FileLocation,
    body: Statement,
    parallel: bool,
    is_custom_gate: bool,
) -> Definition {
    Definition::Template { meta, name, args, arg_location, body, parallel, is_custom_gate }
}

pub fn build_function(
    meta: Meta,
    name: String,
    args: Vec<String>,
    arg_location: FileLocation,
    body: Statement,
) -> Definition {
    Definition::Function { meta, name, args, arg_location, body }
}

pub fn build_bus(
    meta: Meta,
    name: String,
    args: Vec<String>,
    arg_location: FileLocation,
    body: Statement,
) -> Definition {
    Definition::Bus { meta, name, args, arg_location, body }
}

#[derive(Clone)]
pub enum Statement {
    IfThenElse {
        meta: Meta,
        cond: Expression,
        if_case: Box<Statement>,
        else_case: Option<Box<Statement>>,
    },
    While {
        meta: Meta,
        cond: Expression,
        stmt: Box<Statement>,
    },
    Return {
        meta: Meta,
        value: Expression,
    },
    InitializationBlock {
        meta: Meta,
        xtype: VariableType,
        initializations: Vec<Statement>,
    },
    Declaration {
        meta: Meta,
        xtype: VariableType,
        name: String,
        dimensions: Vec<Expression>,
        is_constant: bool,
    },
    Substitution {
        meta: Meta,
        var: String,
        access: Vec<Access>,
        op: AssignOp,
        rhe: Expression,
    },
    MultSubstitution {
        meta: Meta,
        lhe: Expression,
        op: AssignOp,
        rhe: Expression,
    },
    UnderscoreSubstitution {
        meta: Meta,
        op: AssignOp,
        rhe: Expression,
    },
    ConstraintEquality {
        meta: Meta,
        lhe: Expression,
        rhe: Expression,
    },
    LogCall {
        meta: Meta,
        args: Vec<LogArgument>,
    },
    Block {
        meta: Meta,
        stmts: Vec<Statement>,
    },
    Assert {
        meta: Meta,
        arg: Expression,
    },
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum SignalType {
    Output,
    Input,
    Intermediate,
}

pub type TagList = Vec<String>;

#[derive(Clone, PartialEq, Eq)]
pub enum VariableType {
    Var,
    Signal(SignalType, TagList),
    Component,
    AnonymousComponent,
    Bus(String, SignalType, TagList),
}

#[derive(Clone)]
pub enum Expression {
    InfixOp {
        meta: Meta,
        lhe: Box<Expression>,
        infix_op: ExpressionInfixOpcode,
        rhe: Box<Expression>,
    },
    PrefixOp {
        meta: Meta,
        prefix_op: ExpressionPrefixOpcode,
        rhe: Box<Expression>,
    },
    InlineSwitchOp {
        meta: Meta,
        cond: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
    ParallelOp {
        meta: Meta,
        rhe: Box<Expression>,
    },
    Variable {
        meta: Meta,
        name: String,
        access: Vec<Access>,
    },
    Number(Meta, BigInt),
    Call {
        meta: Meta,
        id: String,
        args: Vec<Expression>,
    },
    BusCall {
        meta: Meta,
        id: String,
        args: Vec<Expression>,
    },
    AnonymousComp {
        meta: Meta,
        id: String,
        is_parallel: bool,
        params: Vec<Expression>,
        signals: Vec<Expression>,
        names: Option<Vec<(AssignOp, String)>>,
    },
    ArrayInLine {
        meta: Meta,
        values: Vec<Expression>,
    },
    Tuple {
        meta: Meta,
        values: Vec<Expression>,
    },
    UniformArray {
        meta: Meta,
        value: Box<Expression>,
        dimension: Box<Expression>,
    },
}

#[derive(Clone)]
pub enum Access {
    ComponentAccess(String),
    ArrayAccess(Expression),
}
pub fn build_component_access(acc: String) -> Access {
    Access::ComponentAccess(acc)
}
pub fn build_array_access(expr: Expression) -> Access {
    Access::ArrayAccess(expr)
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum AssignOp {
    AssignVar,
    AssignSignal,
    AssignConstraintSignal,
}

#[derive(Copy, Clone, PartialEq)]
pub enum ExpressionInfixOpcode {
    Mul,
    Div,
    Add,
    Sub,
    Pow,
    IntDiv,
    Mod,
    ShiftL,
    ShiftR,
    LesserEq,
    GreaterEq,
    Lesser,
    Greater,
    Eq,
    NotEq,
    BoolOr,
    BoolAnd,
    BitOr,
    BitAnd,
    BitXor,
}

#[derive(Copy, Clone, PartialEq)]
pub enum ExpressionPrefixOpcode {
    Sub,
    BoolNot,
    Complement,
}

// Knowledge buckets

#[derive(Clone, PartialOrd, PartialEq, Ord, Eq)]
pub enum TypeReduction {
    Variable,
    Component(Option<String>),
    Bus(Option<String>),
    Signal,
    Tag,
}

#[derive(Clone)]
pub enum LogArgument {
    LogStr(String),
    LogExp(Expression),
}
pub fn build_log_string(acc: String) -> LogArgument {
    LogArgument::LogStr(acc)
}
pub fn build_log_expression(expr: Expression) -> LogArgument {
    LogArgument::LogExp(expr)
}

#[derive(Default, Clone)]
pub struct TypeKnowledge {
    reduces_to: Option<TypeReduction>,
}
impl TypeKnowledge {
    pub fn new() -> TypeKnowledge {
        TypeKnowledge::default()
    }
    pub fn set_reduces_to(&mut self, reduces_to: TypeReduction) {
        self.reduces_to = Option::Some(reduces_to);
    }
    pub fn get_reduces_to(&self) -> TypeReduction {
        if let Option::Some(t) = &self.reduces_to {
            t.clone()
        } else {
            panic!("reduces_to knowledge is been look at without being initialized");
        }
    }
    pub fn is_var(&self) -> bool {
        self.get_reduces_to() == TypeReduction::Variable
    }

    pub fn is_initialized(&self) -> bool {
        if let Option::Some(_) = &self.reduces_to {
            true
        } else {
            false
        }
    }
    pub fn is_component(&self) -> bool {
        if let TypeReduction::Component(_) = self.get_reduces_to() {
            true
        } else {
            false
        }
    }
    pub fn is_signal(&self) -> bool {
        self.get_reduces_to() == TypeReduction::Signal
    }
    pub fn is_tag(&self) -> bool {
        self.get_reduces_to() == TypeReduction::Tag
    }
    pub fn is_bus(&self) -> bool {
        if let TypeReduction::Bus(_) = self.get_reduces_to() {
            true
        } else {
            false
        }
    }
}

#[derive(Default, Clone)]
pub struct MemoryKnowledge {
    concrete_dimensions: Option<Vec<usize>>,
    full_length: Option<usize>,
    abstract_memory_address: Option<usize>,
}
impl MemoryKnowledge {
    pub fn new() -> MemoryKnowledge {
        MemoryKnowledge::default()
    }
    pub fn set_concrete_dimensions(&mut self, value: Vec<usize>) {
        self.full_length = Option::Some(value.iter().fold(1, |p, v| p * (*v)));
        self.concrete_dimensions = Option::Some(value);
    }
    pub fn set_abstract_memory_address(&mut self, value: usize) {
        self.abstract_memory_address = Option::Some(value);
    }
    pub fn get_concrete_dimensions(&self) -> &[usize] {
        if let Option::Some(v) = &self.concrete_dimensions {
            v
        } else {
            panic!("concrete dimensions was look at without being initialized");
        }
    }
    pub fn get_full_length(&self) -> usize {
        if let Option::Some(v) = &self.full_length {
            *v
        } else {
            panic!("full dimension was look at without being initialized");
        }
    }
    pub fn get_abstract_memory_address(&self) -> usize {
        if let Option::Some(v) = &self.abstract_memory_address {
            *v
        } else {
            panic!("abstract memory address was look at without being initialized");
        }
    }
}

pub fn produce_report(error_code: ReportCode, location: FileLocation, file_id: FileID) -> Report {
    use ReportCode::*;
    let report = match error_code {
        UnclosedComment => {
            let mut report =
                Report::error("unterminated /* */".to_string(), ReportCode::UnclosedComment);
            report.add_primary(location, file_id, "Comment starts here".to_string());
            report
        }
        NoMainFoundInProject => Report::error(
            "No main specified in the project structure".to_string(),
            ReportCode::NoMainFoundInProject,
        ),
        MultipleMain => Report::error(
            "Multiple main components in the project structure".to_string(),
            ReportCode::MultipleMain,
        ),
        MissingSemicolon => {
            let mut report =
                Report::error(format!("Missing semicolon"), ReportCode::MissingSemicolon);
            report.add_primary(location, file_id, "A semicolon is needed here".to_string());
            report
        }
        UnrecognizedInclude => {
            let mut report = Report::error(
                "unrecognized argument in include directive".to_string(),
                ReportCode::UnrecognizedInclude,
            );
            report.add_primary(location, file_id, "this argument".to_string());
            report
        }
        UnrecognizedPragma => {
            let mut report = Report::error(
                "unrecognized argument in pragma directive".to_string(),
                ReportCode::UnrecognizedPragma,
            );
            report.add_primary(location, file_id, "this argument".to_string());
            report
        }
        UnrecognizedVersion => {
            let mut report = Report::error(
                "unrecognized version argument in pragma directive".to_string(),
                ReportCode::UnrecognizedVersion,
            );
            report.add_primary(location, file_id, "this argument".to_string());
            report
        }
        IllegalExpression => {
            let mut report =
                Report::error("illegal expression".to_string(), ReportCode::IllegalExpression);
            report.add_primary(location, file_id, "here".to_string());
            report
        }
        MultiplePragma => {
            let mut report =
                Report::error("Multiple pragma directives".to_string(), ReportCode::MultiplePragma);
            report.add_primary(location, file_id, "here".to_string());
            report
        }
        ExpectedIdentifier => {
            let mut report = Report::error(
                "An identifier is expected".to_string(),
                ReportCode::ExpectedIdentifier,
            );
            report.add_primary(location, file_id, "This should be an identifier".to_string());
            report
        }
        _ => unreachable!(),
    };
    report
}

pub fn produce_version_warning_report(path: String, version: Version) -> Report {
    let mut r = Report::warning(
        format!(
            "File {} does not include pragma version. Assuming pragma version {:?}",
            path, version
        ),
        ReportCode::NoCompilerVersionWarning,
    );
    r.add_note(format!("At the beginning of file {}, you should add the directive \"pragma circom <Version>\", to indicate which compiler version you are using.",path));
    r
}

pub fn produce_report_with_message(error_code: ReportCode, msg: String) -> Report {
    match error_code {
        ReportCode::FileOs => {
            Report::error(format!("Could not open file {}", msg), ReportCode::FileOs)
        }
        ReportCode::IncludeNotFound => {
            let mut r = Report::error(
                format!(" The file {} to be included has not been found", msg),
                ReportCode::IncludeNotFound,
            );
            r.add_note(
                "Consider using compilation option -l to indicate include paths".to_string(),
            );
            r
        }
        _ => unreachable!(),
    }
}

pub fn produce_compiler_version_report(
    path: String,
    required_version: Version,
    version: Version,
) -> Report {
    let report = Report::error(
        format!("File {} requires pragma version {:?} that is not supported by the compiler (version {:?})", path, required_version, version ),
        ReportCode::CompilerVersionError,
    );
    report
}

pub fn anonymous_inside_condition_error(meta: Meta) -> Report {
    let msg = "An anonymous component cannot be used inside a condition ".to_string();
    let mut report = Report::error(format!("{}", msg), ReportCode::AnonymousCompError);
    let file_id = meta.get_file_id().clone();
    report.add_primary(
        meta.location,
        file_id,
        "This is an anonymous component used inside a condition".to_string(),
    );
    report
}

pub fn anonymous_general_error(meta: Meta, msg: String) -> Report {
    let mut report = Report::error(format!("{}", msg), ReportCode::AnonymousCompError);
    let file_id = meta.get_file_id().clone();
    report.add_primary(
        meta.location,
        file_id,
        "This is the anonymous component whose use is not allowed".to_string(),
    );
    report
}

pub fn tuple_general_error(meta: Meta, msg: String) -> Report {
    let mut report = Report::error(format!("{}", msg), ReportCode::TupleError);
    let file_id = meta.get_file_id().clone();
    report.add_primary(
        meta.location,
        file_id,
        "This is the tuple whose use is not allowed".to_string(),
    );
    report
}

//
// ZKAI_BUGS: we need to get the code back from the metadata
//

pub fn print_statement(statement: &Statement) -> String {
    match statement {
        Statement::IfThenElse { cond, if_case, else_case, .. } => {
            let mut result =
                format!("if ({}) {{\n{}\n}}", print_expression(cond), print_statement(if_case));
            if let Some(else_case) = else_case {
                result.push_str(&format!(" else {{\n{}\n}}", print_statement(else_case)));
            }
            result
        }
        Statement::While { cond, stmt, .. } => {
            format!("while ({}) {{\n{}\n}}", print_expression(cond), print_statement(stmt))
        }
        Statement::Return { value, .. } => {
            format!("return {};", print_expression(value))
        }
        Statement::InitializationBlock { xtype, initializations, .. } => {
            let initializations =
                initializations.iter().map(print_statement).collect::<Vec<_>>().join("\n");
            format!("{} {{\n{}\n}}", print_variable_type(xtype), initializations)
        }
        Statement::Declaration { xtype, name, dimensions, is_constant, .. } => {
            let const_prefix = if *is_constant { "const " } else { "" };
            let dimensions = dimensions.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            format!("{}{} {}[{}];", const_prefix, print_variable_type(xtype), name, dimensions)
        }
        Statement::Substitution { var, access, op, rhe, .. } => {
            let access_str = access.iter().map(print_access).collect::<Vec<_>>().join("");
            format!("{}{} {}= {};", var, access_str, print_assign_op(op), print_expression(rhe))
        }
        Statement::MultSubstitution { lhe, op, rhe, .. } => {
            format!("{} {}= {};", print_expression(lhe), print_assign_op(op), print_expression(rhe))
        }
        Statement::UnderscoreSubstitution { op, rhe, .. } => {
            format!("_ {}= {};", print_assign_op(op), print_expression(rhe))
        }
        Statement::ConstraintEquality { lhe, rhe, .. } => {
            format!("{} === {};", print_expression(lhe), print_expression(rhe))
        }
        Statement::LogCall { args, .. } => {
            let args_str = args.iter().map(print_log_argument).collect::<Vec<_>>().join(", ");
            format!("log({});", args_str)
        }
        Statement::Block { stmts, .. } => {
            let stmts = stmts.iter().map(print_statement).collect::<Vec<_>>().join("\n");
            format!("{{\n{}\n}}", stmts)
        }
        Statement::Assert { arg, .. } => {
            format!("assert({});", print_expression(arg))
        }
    }
}

fn print_expression(expression: &Expression) -> String {
    match expression {
        Expression::InfixOp { lhe, infix_op, rhe, .. } => {
            format!(
                "{} {} {}",
                print_expression(lhe),
                print_infix_opcode(infix_op),
                print_expression(rhe)
            )
        }
        Expression::PrefixOp { prefix_op, rhe, .. } => {
            format!("{}{}", print_prefix_opcode(prefix_op), print_expression(rhe))
        }
        Expression::InlineSwitchOp { cond, if_true, if_false, .. } => {
            format!(
                "({}) ? ({}) : ({})",
                print_expression(cond),
                print_expression(if_true),
                print_expression(if_false)
            )
        }
        Expression::ParallelOp { rhe, .. } => {
            format!("|| {}", print_expression(rhe))
        }
        Expression::Variable { name, access, .. } => {
            let access_str = access.iter().map(print_access).collect::<Vec<_>>().join("");
            format!("{}{}", name, access_str)
        }
        Expression::Number(_, value) => {
            format!("{}", value)
        }
        Expression::Call { id, args, .. } => {
            let args_str = args.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            format!("{}({})", id, args_str)
        }
        Expression::BusCall { id, args, .. } => {
            let args_str = args.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            format!("{}!({})", id, args_str)
        }
        Expression::AnonymousComp { id, is_parallel, params, signals, names, .. } => {
            let parallel_str = if *is_parallel { "parallel " } else { "" };
            let params_str = params.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            let signals_str = signals.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            let names_str = names
                .as_ref()
                .map(|n| {
                    n.iter()
                        .map(|(op, name)| format!("{} {}", print_assign_op(op), name))
                        .collect::<Vec<_>>()
                        .join(", ")
                })
                .unwrap_or_default();
            format!("{}{}({}) [{}] {{ {} }}", parallel_str, id, params_str, signals_str, names_str)
        }
        Expression::ArrayInLine { values, .. } => {
            let values_str = values.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            format!("[{}]", values_str)
        }
        Expression::Tuple { values, .. } => {
            let values_str = values.iter().map(print_expression).collect::<Vec<_>>().join(", ");
            format!("({})", values_str)
        }
        Expression::UniformArray { value, dimension, .. } => {
            format!("[{}; {}]", print_expression(value), print_expression(dimension))
        }
    }
}

fn print_infix_opcode(op: &ExpressionInfixOpcode) -> &'static str {
    match op {
        ExpressionInfixOpcode::Mul => "*",
        ExpressionInfixOpcode::Div => "/",
        ExpressionInfixOpcode::Add => "+",
        ExpressionInfixOpcode::Sub => "-",
        ExpressionInfixOpcode::Pow => "**",
        ExpressionInfixOpcode::IntDiv => "//",
        ExpressionInfixOpcode::Mod => "%",
        ExpressionInfixOpcode::ShiftL => "<<",
        ExpressionInfixOpcode::ShiftR => ">>",
        ExpressionInfixOpcode::LesserEq => "<=",
        ExpressionInfixOpcode::GreaterEq => ">=",
        ExpressionInfixOpcode::Lesser => "<",
        ExpressionInfixOpcode::Greater => ">",
        ExpressionInfixOpcode::Eq => "==",
        ExpressionInfixOpcode::NotEq => "!=",
        ExpressionInfixOpcode::BoolOr => "||",
        ExpressionInfixOpcode::BoolAnd => "&&",
        ExpressionInfixOpcode::BitOr => "|",
        ExpressionInfixOpcode::BitAnd => "&",
        ExpressionInfixOpcode::BitXor => "^",
    }
}

fn print_prefix_opcode(op: &ExpressionPrefixOpcode) -> &'static str {
    match op {
        ExpressionPrefixOpcode::Sub => "-",
        ExpressionPrefixOpcode::BoolNot => "!",
        ExpressionPrefixOpcode::Complement => "~",
    }
}

fn print_variable_type(xtype: &VariableType) -> String {
    match xtype {
        VariableType::Var => "var".to_string(),
        VariableType::Signal(signal_type, tags) => {
            let signal_str = match signal_type {
                SignalType::Output => "signal output",
                SignalType::Input => "signal input",
                SignalType::Intermediate => "signal intermediate",
            };
            if tags.is_empty() {
                signal_str.to_string()
            } else {
                format!("{} [{}]", signal_str, tags.join(", "))
            }
        }
        VariableType::Component => "component".to_string(),
        VariableType::AnonymousComponent => "anonymous_component".to_string(),
        VariableType::Bus(name, signal_type, tags) => {
            let signal_str = match signal_type {
                SignalType::Output => "output",
                SignalType::Input => "input",
                SignalType::Intermediate => "intermediate",
            };
            format!("bus {} {} [{}]", name, signal_str, tags.join(", "))
        }
    }
}

fn print_access(access: &Access) -> String {
    match access {
        Access::ComponentAccess(name) => format!(".{}", name),
        Access::ArrayAccess(expr) => format!("[{}]", print_expression(expr)),
    }
}

fn print_assign_op(op: &AssignOp) -> &'static str {
    match op {
        AssignOp::AssignVar => "=",
        AssignOp::AssignSignal => "<-",
        AssignOp::AssignConstraintSignal => "<==",
    }
}

fn print_log_argument(arg: &LogArgument) -> String {
    match arg {
        LogArgument::LogStr(s) => format!("\"{}\"", s),
        LogArgument::LogExp(expr) => print_expression(expr),
    }
}
