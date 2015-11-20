open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Printf

let new_id =
  let n = ref 0 in
  fun () ->
    incr n;
    sprintf "__ppx_sql_%d" !n

let gen_stmt loc ~cacheable sql inp =
  let mkstr s = Exp.constant (Const_string (s, None)) in
  let mklid s = {txt=Lident s; loc} in
  let mkident s = Exp.ident (mklid s) in
  let mkapply fn args = Exp.apply (mkident fn) args in
  let mkpident txt = Pat.var {txt; loc} in

  let k = new_id () in
  let st = new_id () in
  let id =
    let signature =
      sprintf "%d-%f-%d-%S"
        Unix.(getpid ()) (Unix.gettimeofday ()) (Random.int 0x3FFFFFF) sql
    in Digest.to_hex (Digest.string signature) in
  let stmt_id =
    if cacheable
    then [%expr Some [%e mkstr id ]]
    else [%expr None] in
  let exp = List.fold_right (fun elem dir ->
    let typ = Sqlexpr_parser.in_type2str elem in
    [%expr [%e mkapply typ [("", dir)]]])
    inp
    [%expr [%e mkident k]] in
    let dir = [%expr fun [%p mkpident k] -> fun [%p mkpident st] ->
      let open Sqlexpr.Directives in [%e exp] [%e mkident st]
    ]  in
  [%expr {
    Sqlexpr.sql_statement = [%e mkstr sql];
    stmt_id = [%e stmt_id];
    directive = [%e dir];
  } ]

let gen_expr loc ~cacheable sql inp outp =
  let mklid s = {txt=Lident s; loc} in
  let mkident s = Exp.ident (mklid s) in
  let mkpident txt = Pat.var {txt; loc} in
  let mkint i = Exp.constant (Const_int i) in

  let stmt = gen_stmt loc ~cacheable sql inp in
  let id = new_id () in
  let conv s = Longident.(Ldot (Ldot (Lident "Sqlexpr", "Conversion"), s)) in
  let conv_exprs = List.mapi (fun i elem ->
    let txt = conv (Sqlexpr_parser.out_type2str elem) in
    let fn = Exp.ident {txt; loc} in
    let args = [("", [%expr Array.get [%e mkident id] [%e mkint i]])] in
    Exp.apply fn args) outp in
  let tuple_func =
    let e = match conv_exprs with
        [] -> assert false
      | [x] -> x
      | hd::tl -> Exp.tuple conv_exprs in
    [%expr fun [%p mkpident id] -> [%e e]] in
  [%expr {
    Sqlexpr.statement = [%e stmt];
    get_data = ([%e mkint (List.length outp)], [%e tuple_func]);
  }]

let stmts = ref []
let init_stmts = ref []

let gen_sql ?(init=false) ?(cacheable=false) loc str =
  let (sql, inp, outp) = Sqlexpr_parser.parse str in

  (* accumulate statements *)
  if init
  then init_stmts := sql :: !init_stmts
  else stmts := sql :: !stmts;

  if [] = outp
  then gen_stmt loc ~cacheable sql inp
  else gen_expr loc ~cacheable sql inp outp

let sqlcheck_sqlite loc =
  let mkstr s = Exp.constant (Const_string (s, None)) in
  let statement_check = [%expr
    try ignore(Sqlite3.prepare db stmt)
    with Sqlite3.Error s ->
      ret := false;
      Format.fprintf fmt "Error in statement %S: %s\n" stmt s
  ] in
  let stmt_expr_f acc elem = [%expr [%e mkstr elem] :: [%e acc]] in
  let stmt_exprs = List.fold_left stmt_expr_f [%expr []] !stmts in
  let init_exprs = List.fold_left stmt_expr_f [%expr []] !init_stmts in
  let check_db_expr = [%expr fun db fmt ->
    let ret = ref true in
    List.iter (fun stmt -> print_endline ("checking " ^ stmt); [%e statement_check]) [%e stmt_exprs];
    !ret
  ] in
  let init_db_expr = [%expr fun db fmt ->
    let ret = ref true in
    List.iter (fun stmt -> match Sqlite3.exec db stmt with
      | Sqlite3.Rc.OK -> ()
      | rc -> begin
        ret := false;
        Format.fprintf fmt "Error in init. SQL statement (%s)@ %S@\n"
          (Sqlite3.errmsg db) stmt
      end) [%e init_exprs];
      !ret
  ] in
  let in_mem_check_expr = [%expr fun fmt ->
    let db = Sqlite3.db_open ":memory:" in
    init_db db fmt && check_db db fmt
  ] in
  [%expr
    let init_db = [%e init_db_expr] in
    let check_db = [%e check_db_expr] in
    let in_mem_check = [%e in_mem_check_expr] in
    (init_db, check_db, in_mem_check)
  ]

let call fn loc = function
  | PStr [ {pstr_desc = Pstr_eval (
    { pexp_desc = Pexp_constant(Const_string(sym, None))}, _)} ] -> fn loc sym
  | _ -> raise (Location.Error(Location.error ~loc (
    "sqlexpr extension accepts a string")))

let call_sqlcheck loc = function
  | PStr [ {pstr_desc = Pstr_eval ({ pexp_desc =
    Pexp_constant(Const_string("sqlite", None))}, _)}] ->
      sqlcheck_sqlite loc
  | _ -> raise (Location.Error(Location.error ~loc (
    "sqlcheck extension accepts \"sqlite\"")))

let new_mapper argv = {
  default_mapper with
  expr = fun mapper expr ->
    match expr with
    (* is this an extension node? *)
    | {pexp_desc = Pexp_extension ({txt = "sql"; loc}, pstr)} ->
        call gen_sql loc pstr
    | {pexp_desc = Pexp_extension ({txt = "sqlc"; loc}, pstr)} ->
        call (gen_sql ~cacheable:true) loc pstr
    | {pexp_desc = Pexp_extension ({txt = "sqlinit"; loc}, pstr)} ->
        call (gen_sql ~init:true) loc pstr
    | {pexp_desc = Pexp_extension ({txt = "sqlcheck"; loc}, pstr)} ->
        call_sqlcheck loc pstr
    (* Delegate to the default mapper *)
    | x -> default_mapper.expr mapper x;
}

let () =
  Random.self_init ();
  register "sqlexpr" new_mapper
