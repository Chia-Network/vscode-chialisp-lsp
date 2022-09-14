/* tslint:disable */
/* eslint-disable */
/**
* @param {string} hex_prog
* @param {any} args_js
* @param {object} symbols
* @param {object} overrides
* @returns {any}
*/
export function create_clvm_runner(hex_prog: string, args_js: any, symbols: object, overrides: object): any;
/**
* @param {number} runner
* @returns {any}
*/
export function final_value(runner: number): any;
/**
* @param {number} runner
*/
export function remove_clvm_runner(runner: number): void;
/**
* @param {number} runner
* @returns {any}
*/
export function run_step(runner: number): any;
/**
* @param {any} input_js
* @param {any} filename_js
* @param {any[]} search_paths_js
* @returns {any}
*/
export function compile(input_js: any, filename_js: any, search_paths_js: any[]): any;
/**
* @param {string} hex_prog
* @param {object} symbol_table_js
* @param {string} function_name
* @returns {any}
*/
export function compose_run_function(hex_prog: string, symbol_table_js: object, function_name: string): any;
/**
* @returns {number}
*/
export function create_repl(): number;
/**
* @param {number} repl_id
*/
export function destroy_repl(repl_id: number): void;
/**
* @param {number} repl_id
* @param {string} input
* @returns {any}
*/
export function repl_run_string(repl_id: number, input: string): any;
/**
* @param {any} v
* @returns {any}
*/
export function sexp_to_string(v: any): any;
/**
* @param {any} file_reader
* @param {any} err_writer
* @returns {number}
*/
export function create_lsp_service(file_reader: any, err_writer: any): number;
/**
* @param {number} lsp
*/
export function destroy_lsp_service(lsp: number): void;
/**
* @param {number} lsp_id
* @param {string} msg
* @returns {any[]}
*/
export function lsp_service_handle_msg(lsp_id: number, msg: string): any[];
