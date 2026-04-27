use std::path::Path;

use chialisp::compiler::comptypes::{
    CompileForm, HelperForm, ImportLongName, LongNameTranslation, ModuleImportSpec,
    NamespaceRefData,
};
use chialisp::compiler::sexp::decode_string;
use chialisp::compiler::srcloc::Srcloc;

use crate::lsp::compopts::get_file_content;
use crate::lsp::parse::ParsedDoc;
use crate::lsp::semtok::{DocumentProcessingDescription, SemanticTokenSortable};
use crate::lsp::types::{urlify, TK_FUNCTION_IDX, TK_MACRO_IDX};
use crate::lsp::LSPServiceProvider;

pub trait HelperResolver {
    fn resolve_helper_reference<'a>(
        &mut self,
        frontend: &CompileForm,
        name: &[u8],
    ) -> Option<HelperForm>;
}

fn get_filename_of_import(longname: &ImportLongName) -> String {
    let imported_filename = longname.as_u8_vec(LongNameTranslation::Filename(".clinc".to_string()));
    decode_string(&imported_filename)
}

fn resolve_imported_name(
    service_provider: &mut LSPServiceProvider,
    namespace_ref: &NamespaceRefData,
    called_name: &[u8],
) -> Option<(String, Vec<u8>)> {
    match &namespace_ref.specification {
        ModuleImportSpec::Qualified(q) => {
            let filename_decoded = get_filename_of_import(&q.name);
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
            }
        }
        ModuleImportSpec::Exposing(_, exposed_names) => {
            let filename_decoded = get_filename_of_import(&namespace_ref.longname);
            if let Some((file_uri, parsed)) =
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
            let filename_decoded = get_filename_of_import(&namespace_ref.longname);
            if let Some((file_uri, parsed)) =
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

            let Some((file_uri, imported_name)) = resolve_imported_name(self, &nsref, name) else {
                continue;
            };

            if let Some(imported_doc) = self.get_parsed(&file_uri) {
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
            if matches!(f, HelperForm::Defun(_, _) | HelperForm::Defmacro(_)) {
                if name == f.name() {
                    return Some(f.clone());
                }
            }
        }

        None
    }
}
