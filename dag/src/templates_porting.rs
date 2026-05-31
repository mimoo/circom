use super::{Tree, DAG};
use circom_algebra::num_traits::AsPrimitive;
use constraint_writers::sym_writer::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufWriter, Write};
use serde_derive::Serialize;

#[derive(Debug, Clone, Serialize)]
struct TemplatesStruct {
    // template id -> (template name, signal name -> (signal id, full signal name))
    templates: Vec<(String, HashMap<String, (i64, String)>)>,
}

impl TemplatesStruct {
    pub fn new() -> TemplatesStruct {
        TemplatesStruct {
            templates: Vec::new(),
        }
    }
}

// pub struct TemplateFile {
//     data : TemplatesStruct,
// }

// impl TemplateFile {
//     pub fn new(file: &str) -> Result<TemplateFile, ()> {
//         let file = File::create(file).map_err(|_err| {})?;
//         // let writer = BufWriter::new(file);
//         Result::Ok(TemplateFile { data: TemplatesStruct::new() })
//     }

//     pub fn write_node(sym: &mut TemplateFile, elem: SymElem) -> Result<(), ()> {
//         sym.writer.write_all(elem.to_string().as_bytes()).map_err(|_err| {})?;
//         sym.writer.write_all(b"\n").map_err(|_err| {}) //?;
//         //sym.writer.flush().map_err(|_err| {})
//     }
    
//     pub fn finish_writing(mut sym: TemplateFile) -> Result<(), ()> {
// 	    sym.writer.flush().map_err(|_err| {})
//     }

//     // pub fn close(_sym: TemplateFile) {}
// }


pub fn write(dag: &DAG, file_name: &str) -> Result<(), ()> {
    let tree = Tree::new(dag);
    let mut data = TemplatesStruct::new();
    visit_tree(&tree, &mut data)?;

    let serialized_templates = serde_json::to_string_pretty(&data).unwrap();
    let mut dot_template = File::create(file_name).map_err(|_err| {})?;
    dot_template.write_all(serialized_templates.as_bytes()).map_err(|_err| {})?;
    Ok(())
}

fn visit_tree(tree: &Tree, dot_template: &mut TemplatesStruct) -> Result<(), ()> {
    let mut node_info = HashMap::new();
    let mut template_name = String::new();
    let node_id : i64 = tree.node_id.as_();
    for signal in &tree.signals {
        let name = HashMap::get(&tree.id_to_name, signal).unwrap();
        let symbol = format!("{}.{}", tree.path, name);
        // println!("Visiting signal: {} {}", name, tree.path);
        // let node_id = tree.node_id.as_();
        let node = tree.dag.nodes.get(tree.node_id).unwrap();
        template_name = node.template_name.clone();
        // println!("Visiting node: {} {} {:?}", node_id, node.template_name, node.parameters);
        node_info.insert(name.clone(), (signal.as_(), symbol));
    }

    dot_template.templates.push((template_name, node_info));


    for edge in Tree::get_edges(tree) {
        let subtree = Tree::go_to_subtree(tree, edge);
        visit_tree(&subtree, dot_template)?;
    }
    Ok(())
}
