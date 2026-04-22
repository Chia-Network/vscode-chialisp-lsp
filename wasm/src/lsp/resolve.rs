use std::path::Path;

use chialisp::compiler::comptypes::{CompileForm, HelperForm, LongNameTranslation, ModuleImportSpec};
use chialisp::compiler::sexp::decode_string;
use chialisp::compiler::srcloc::Srcloc;

use crate::lsp::compopts::get_file_content;
use crate::lsp::parse::ParsedDoc;
use crate::lsp::semtok::{DocumentProcessingDescription, SemanticTokenSortable};
use crate::lsp::types::{TK_FUNCTION_IDX, TK_MACRO_IDX, urlify};
use crate::lsp::LSPServiceProvider;

pub trait HelperResolver {
    fn resolve_helper_reference<'a>(
        &mut self,
        frontend: &CompileForm,
        name: &[u8],
    ) -> Option<HelperForm>;
}

fn resolve_imported_name(spec: &ModuleImportSpec, called_name: &[u8]) -> Option<Vec<u8>> {
    match spec {
        ModuleImportSpec::Qualified(_) => None,
        ModuleImportSpec::Exposing(_, exposed_names) => {
            for exposed_name in exposed_names.iter() {
                let available_name = exposed_name.alias.as_ref().unwrap_or(&exposed_name.name);
                if available_name == called_name {
                    return Some(exposed_name.name.clone());
                }
            }
            None
        }
        ModuleImportSpec::Hiding(_, hidden_names) => {
            for hidden_name in hidden_names.iter() {
                let hidden = hidden_name.alias.as_ref().unwrap_or(&hidden_name.name);
                if hidden == called_name {
                    return None;
                }
            }
            Some(called_name.to_vec())
        }
    }
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

            let Some(imported_name) = resolve_imported_name(&nsref.specification, name) else {
                continue;
            };

            let imported_filename = nsref
                .longname
                .as_u8_vec(LongNameTranslation::Filename(".clinc".to_string()));
            let imported_filename_decoded = decode_string(&imported_filename);

            if let Ok((filename, file_body)) = get_file_content(
                self.log.clone(),
                self.fs.clone(),
                self.get_workspace_root(),
                &self.config.include_paths,
                &decode_string(&imported_filename),
            ) {
                if let Some(file_uri) = self
                    .get_workspace_root()
                    .and_then(|r| r.join(&filename).to_str().map(urlify))
                {
                    self.save_doc(file_uri.clone(), file_body);
                    self.ensure_parsed_document(&file_uri);

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
