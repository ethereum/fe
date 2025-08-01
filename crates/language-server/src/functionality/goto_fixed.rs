pub async fn handle_goto_definition(
    backend: &mut FeLanguageServerBackend,
    params: async_lsp::lsp_types::GotoDefinitionParams,
) -> Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ResponseError> {
    let file_path_str = params
        .text_document_position_params
        .text_document
        .uri
        .path();
    let file_path = std::path::Path::new(file_path_str);

    let url = Url::from_file_path(file_path).map_err(|_| ResponseError {
        code: async_lsp::ErrorCode::REQUEST_FAILED,
        message: format!("Invalid file path: {file_path_str}"),
        data: None,
    })?;

    let position = params.text_document_position_params.position;
    let cursor = map_lsp_position_to_offset(&backend.db, &url, position);

    let Ok(Some(file)) = backend.db.workspace().file_for_url(&url) else {
        return Err(ResponseError {
            code: async_lsp::ErrorCode::REQUEST_FAILED,
            message: format!("File not found in index: {url} (original path: {file_path_str})"),
            data: None,
        });
    };

    let top_mod = map_file_to_mod(&backend.db, file);
    
    // Use the unified approach for all symbols
    let mut scopes = get_goto_target_scopes_for_cursor(&backend.db, top_mod, cursor).unwrap_or_default();
    
    // If we didn't find any scopes, check if this cursor is at an item definition name
    if scopes.is_empty() {
        use hir::span::item::{LazyStructSpan, LazyFuncSpan, LazyEnumSpan};
        use hir::hir_def::scope_graph::ScopeId;
        
        let items = top_mod.scope_graph(&backend.db).items_dfs(&backend.db);
        for item in items {
            match item {
                ItemKind::Struct(s) => {
                    let struct_span = LazyStructSpan::new(s);
                    if let Some(name_span) = struct_span.name().resolve(&backend.db) {
                        if name_span.range.start() == cursor {
                            scopes = vec![ScopeId::from_item(item)];
                            break;
                        }
                    }
                }
                ItemKind::Func(f) => {
                    let func_span = LazyFuncSpan::new(f);
                    if let Some(name_span) = func_span.name().resolve(&backend.db) {
                        if name_span.range.start() == cursor {
                            scopes = vec![ScopeId::from_item(item)];
                            break;
                        }
                    }
                }
                ItemKind::Enum(e) => {
                    let enum_span = LazyEnumSpan::new(e);
                    if let Some(name_span) = enum_span.name().resolve(&backend.db) {
                        if name_span.range.start() == cursor {
                            scopes = vec![ScopeId::from_item(item)];
                            break;
                        }
                    }
                }
                _ => {}
            }
        }
    }
    
    let locations = scopes
        .iter()
        .map(|scope| to_lsp_location_from_scope(&backend.db, *scope))
        .collect::<Vec<_>>();

    let result: Result<Option<async_lsp::lsp_types::GotoDefinitionResponse>, ()> =
        Ok(Some(async_lsp::lsp_types::GotoDefinitionResponse::Array(
            locations
                .into_iter()
                .filter_map(std::result::Result::ok)
                .collect(),
        )));
    
    let response = match result {
        Ok(response) => response,
        Err(e) => {
            error!("Error handling goto definition: {:?}", e);
            None
        }
    };
    
    Ok(response)
}