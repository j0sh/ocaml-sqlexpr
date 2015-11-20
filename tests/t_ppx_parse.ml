open OUnit
open Sqlexpr_parser

let ae = assert_equal ~printer:(fun x -> x)
let pq q = parse q |> fun (sql, _, _) -> sql

let test_sql _ =

  let s = pq "insert into values(%s{foo}, %s{bar})" in
  ae "insert into values(?{foo}, ?{bar})" s;

  let s = pq "@d{kilroy} was @s{here}" in
  ae "kilroy was here" s;

  (* verifies the order of regex substitution. output substitution pass runs
   * before the input pass to avoid injecting valid sqlexpr metacharacter '?'.
   * For example, given the following string, running the input pass first would
   * result in a valid sqlexpr string, leading to an incorrect substitiion in
   * the output pass. never mind that immediately adjacent inputs+outputs in
   *  valid SQL are extremely unlikely... *)
  let s = pq "@s%d{abc}" in
  ae "@s?{abc}" s;

  (* test invalid expressions and adjacencies *)
  let s = pq "@s@s %d@s{abc}%d@s%d@s%d{def}%d{ghi}@s" in
  ae "@s@s ?abc?@s?@s?{def}?{ghi}@s" s;

  (* column name is not alphanumeric so leave as-is *)
  (* also check that whitespace is preserved *)
  let s = pq "@s{:kilroy}     @@was %@{here}" in
  ae "@s{:kilroy}     @@was %@{here}" s;

  let s = pq "excellent" in
  ae "excellent" s


let test_quotes _ =

  (* single quotes *)
  let s = pq "strftime('%s-%d', %s-%d @s{abc}%d{def} '@s{abc}%d{def}')" in
  ae "strftime('%s-%d', ?-? abc?{def} '@s{abc}%d{def}')" s;

  (* double quotes *)
  let s = pq {|strftime("%s-%d", %s-%d @s{abc}%d{def} "@s{abc}%d{def}")|} in
  ae {|strftime("%s-%d", ?-? abc?{def} "@s{abc}%d{def}")|} s;

  (* mixed quotes and nested quotes *)
  let s = pq {|@s{abc}"@s{def}"'@d{ghi}''%f'%f"%S"%S "'@s{jkl}%d'" '"%d'"|} in
  ae {|abc"@s{def}"'@d{ghi}''%f'?"%S"? "'@s{jkl}%d'" '"%d'"|} s;

  (* more nested and unbalanced quotes *)
  let s = pq {|"'%d'" %d '"%d"' "'%d"'|} in
  ae {|"'%d'" ? '"%d"' "'%d"'|} s

let tests =
  "ppx_tests">::: [
    "test_sql">::test_sql;
    "test_quotes">::test_quotes;
  ]

let _ = run_test_tt_main tests
