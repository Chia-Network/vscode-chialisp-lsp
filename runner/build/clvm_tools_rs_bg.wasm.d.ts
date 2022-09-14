/* tslint:disable */
/* eslint-disable */
export const memory: WebAssembly.Memory;
export function create_clvm_runner(a: number, b: number, c: number, d: number, e: number): number;
export function final_value(a: number): number;
export function remove_clvm_runner(a: number): void;
export function run_step(a: number): number;
export function compile(a: number, b: number, c: number, d: number): number;
export function compose_run_function(a: number, b: number, c: number, d: number, e: number): number;
export function create_repl(): number;
export function destroy_repl(a: number): void;
export function repl_run_string(a: number, b: number, c: number): number;
export function sexp_to_string(a: number): number;
export function create_lsp_service(a: number, b: number): number;
export function destroy_lsp_service(a: number): void;
export function lsp_service_handle_msg(a: number, b: number, c: number, d: number): void;
export function __wbindgen_malloc(a: number): number;
export function __wbindgen_realloc(a: number, b: number, c: number): number;
export function __wbindgen_add_to_stack_pointer(a: number): number;
export function __wbindgen_free(a: number, b: number): void;
export function __wbindgen_exn_store(a: number): void;
