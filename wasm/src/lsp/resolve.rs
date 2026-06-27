use std::rc::Rc;

use chialisp::compiler::comptypes::{
    BodyForm, CompileForm, DefunData, Export, ExportFunctionDesc, HelperForm, LongNameTranslation,
    ModuleImportSpec, NamespaceRefData,
};
use chialisp::compiler::sexp::{decode_string, SExp};

use crate::lsp::LSPServiceProvider;

pub trait HelperResolver {
    fn resolve_helper_reference(
        &mut self,
        frontend: &CompileForm,
        name: &[u8],
    ) -> Option<HelperForm>;
}

fn resolve_imported_name(
    service_provider: &mut LSPServiceProvider,
    namespace_ref: &NamespaceRefData,
    called_name: &[u8],
) -> Option<(String, Vec<u8>)> {
    match &namespace_ref.specification {
        ModuleImportSpec::Qualified(q) => {
            let filename_decoded = decode_string(&service_provider.get_filename_of_import(&q.name));
            if let Some((file_uri, parsed)) =
                service_provider.get_file_uri_and_ensure_parsing(&filename_decoded)
            {
                let qualified_parent = q
                    .target
                    .as_ref()
                    .map(|q| &q.name)
                    .unwrap_or_else(|| &q.name);
                for helper in parsed.compiled.helpers.iter() {
                    let full_helper_qualified_name = qualified_parent.with_child(helper.name());
                    let name_to_match =
                        full_helper_qualified_name.as_u8_vec(LongNameTranslation::Namespace);
                    if name_to_match == called_name {
                        return Some((file_uri, helper.name().to_vec()));
                    }
                }

                // Look into the exports to see if we have a match.
                for export in parsed.exports.iter() {
                    if let Export::Function(f) = &export.1 {
                        let use_name = f.as_name.clone().unwrap_or_else(|| f.name.clone());
                        let full_helper_qualified_name = qualified_parent.with_child(&use_name.value);
                        let name_to_match = full_helper_qualified_name.as_u8_vec(LongNameTranslation::Namespace);
                        if name_to_match == called_name {
                            return Some((file_uri, use_name.value));
                        }
                    }
                }
            }
        }
        ModuleImportSpec::Exposing(_, exposed_names) => {
            let filename_decoded =
                decode_string(&service_provider.get_filename_of_import(&namespace_ref.longname));
            if let Some((file_uri, _parsed)) =
                service_provider.get_file_uri_and_ensure_parsing(&filename_decoded)
            {
                for exposed_name in exposed_names.iter() {
                    let available_name = exposed_name.alias.as_ref().unwrap_or(&exposed_name.name);
                    if available_name == called_name {
                        return Some((file_uri, exposed_name.name.clone()));
                    }
                }
            }
        }
        ModuleImportSpec::Hiding(_, hidden_names) => {
            let filename_decoded =
                decode_string(&service_provider.get_filename_of_import(&namespace_ref.longname));
            if let Some((file_uri, _parsed)) =
                service_provider.get_file_uri_and_ensure_parsing(&filename_decoded)
            {
                for hidden_name in hidden_names.iter() {
                    let hidden = hidden_name.alias.as_ref().unwrap_or(&hidden_name.name);
                    if hidden == called_name {
                        return None;
                    }
                }
                return Some((file_uri, called_name.to_vec()));
            }
        }
    }

    None
}

fn final_export_name(bf: &ExportFunctionDesc) -> &[u8] {
    if let Some(as_name) = bf.as_name.as_ref() {
        return &as_name.value;
    }
    &bf.name.value
}

fn function_export_name_matches(bf: &ExportFunctionDesc, imported_name: &[u8]) -> bool {
    final_export_name(bf) == imported_name || bf.name.value == imported_name
}

impl HelperResolver for LSPServiceProvider {
    fn resolve_helper_reference<'a>(
        &mut self,
        frontend: &CompileForm,
        name: &[u8],
    ) -> Option<HelperForm> {
        for helper in frontend.helpers.iter() {
            let nsref = match helper {
                HelperForm::Defnsref(nsref) => nsref,
                _ => continue,
            };

            let Some((file_uri, imported_name)) = resolve_imported_name(self, nsref, name) else {
                continue;
            };

            if let Some(imported_doc) = self.get_parsed(&file_uri) {
                // program and program_hash are properties of clsp imports that have a main export.
                if file_uri.ends_with(".clsp") {
                    for (_, export) in imported_doc.exports.iter() {
                        match export {
                            Export::MainProgram(desc)
                                if imported_name == b"program"
                                    || imported_name == b"program_hash" =>
                            {
                                // Synthesize a defun that looks like a main export.
                                return Some(HelperForm::Defun(
                                    false,
                                    Box::new(DefunData {
                                        name: imported_name.clone(),
                                        nl: desc.loc.clone(),
                                        kw: desc.kw_loc.clone(),
                                        loc: desc.loc.clone(),
                                        args: desc.args.clone(),
                                        orig_args: desc.args.clone(),
                                        body: desc.expr.clone(),
                                        synthetic: None,
                                    }),
                                ));
                            }
                            Export::Function(bf)
                                if function_export_name_matches(bf, &imported_name) =>
                            {
                                let nil = SExp::Nil(bf.loc.clone());
                                let nil_rc = Rc::new(nil.clone());
                                return Some(HelperForm::Defun(
                                    false,
                                    Box::new(DefunData {
                                        loc: bf.loc.clone(),
                                        name: bf.name.value.clone(),
                                        nl: bf.name.loc.clone().unwrap_or_else(|| bf.loc.clone()),
                                        kw: bf.kw_loc.clone(),
                                        args: nil_rc.clone(),
                                        orig_args: nil_rc,
                                        body: Rc::new(BodyForm::Quoted(nil)),
                                        synthetic: None,
                                    }),
                                ));
                            }
                            _ => {}
                        }
                    }
                }

                for imported_helper in imported_doc.compiled.helpers.iter() {
                    match imported_helper {
                        HelperForm::Defun(_, defun) if defun.name == imported_name => {
                            return Some(imported_helper.clone());
                        }
                        HelperForm::Defmacro(mac) if mac.name == imported_name => {
                            return Some(imported_helper.clone());
                        }
                        _ => {}
                    }
                }
            }
        }

        // Direct reference.
        for f in frontend.helpers.iter() {
            if matches!(f, HelperForm::Defun(_, _) | HelperForm::Defmacro(_)) && name == f.name() {
                return Some(f.clone());
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use chialisp::compiler::comptypes::NameAndLoc;
    use chialisp::compiler::srcloc::Srcloc;

    fn name_and_loc(name: &str) -> NameAndLoc {
        NameAndLoc {
            value: name.as_bytes().to_vec(),
            loc: None,
        }
    }

    fn export_function_desc(name: &str, as_name: Option<&str>) -> ExportFunctionDesc {
        ExportFunctionDesc {
            loc: Srcloc::start("*test*"),
            kw_loc: None,
            name: name_and_loc(name),
            as_loc: None,
            as_name: as_name.map(name_and_loc),
        }
    }

    #[test]
    fn function_export_name_matches_source_and_final_names() {
        let desc = export_function_desc("original", Some("exported"));

        assert!(function_export_name_matches(&desc, b"original"));
        assert!(function_export_name_matches(&desc, b"exported"));
        assert!(!function_export_name_matches(&desc, b"other"));
    }
}
