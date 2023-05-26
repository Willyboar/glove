import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import glove

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`

// Tests for QBE.Value Display
// Test QBE.Value.Temporary
pub fn qbe_value_temp_test() {
  glove.Temporary("temp")
  |> glove.display_value()
  |> should.equal("%temp")
}

// Test QBE.Value.Global
pub fn qbe_value_global_test() {
  glove.Global("global")
  |> glove.display_value()
  |> should.equal("$global")
}

// Test QBE.Value.Const
pub fn qbe_value_const_test() {
  glove.Const(1)
  |> glove.display_value()
  |> should.equal("1")
}

// Tests for QBE.DataItem Display
pub fn display_data_item_test() {
  let item1 = glove.Symbol("symbol1", Some(10))
  let item2 = glove.Symbol("symbol2", None)
  let item3 = glove.Str("string value")
  let item4 = glove.Constant(42)

  // Symbol with offset
  item1
  |> glove.display_data_item
  |> should.equal("$symbol1 +10")

  // Symbol without offset
  item2
  |> glove.display_data_item
  |> should.equal("$symbol2")

  // String value
  item3
  |> glove.display_data_item
  |> should.equal("\"string value\"")

  // Constant value
  item4
  |> glove.display_data_item
  |> should.equal("42")
}

// Tests for QBE.Statement Display
pub fn display_statement_test() {
  // Test Assign statement
  let assign =
    glove.Assign(
      glove.Temporary("temp"),
      glove.Word,
      glove.Add(glove.Temporary("a"), glove.Temporary("b")),
    )
  assign
  |> glove.display_statement
  |> should.equal("%temp =w add %a, %b")
  // Test Volatile statement

  let volatile = glove.Volatile(glove.Ret(Some(glove.Const(0))))
  volatile
  |> glove.display_statement
  |> should.equal("ret 0")

  let empty_volatile = glove.Volatile(glove.Ret(None))
  empty_volatile
  |> glove.display_statement
  |> should.equal("ret")
}

// Tests for QBE.Block Display
pub fn display_block_test() {
  // Test empty block
  let empty_block = glove.Block("label", [])
  empty_block
  |> glove.display_block()
  |> should.equal("label:\n")

  // Test block with statements
  let statements = [
    glove.Assign(
      glove.Temporary("temp1"),
      glove.Word,
      glove.Add(glove.Temporary("a"), glove.Temporary("b")),
    ),
    glove.Volatile(glove.Ret(Some(glove.Temporary("temp1")))),
  ]

  let block_with_statements = glove.Block("label", statements)
  block_with_statements
  |> glove.display_block
  |> should.equal("label:\n%temp1 =w add %a, %b\nret %temp1")
}

// Tests for QBE.Linkage Display
pub fn display_linkage_test() {
  // Test case with exported and section
  let linkage1 =
    glove.Linkage(
      exported: True,
      section: Some("mysection"),
      secflags: Some("flags"),
    )
  linkage1
  |> glove.display_linkage
  |> should.equal("export section \"mysection\" \"flags\" ")

  // Test case with exported and no section
  let linkage2 = glove.Linkage(exported: True, section: None, secflags: None)
  linkage2
  |> glove.display_linkage
  |> should.equal("export ")

  // Test case with no exported and section
  let linkage3 =
    glove.Linkage(
      exported: False,
      section: Some("othersection"),
      secflags: None,
    )
  linkage3
  |> glove.display_linkage
  |> should.equal("section \"othersection\" ")

  // Test case with no exported and no section
  let linkage4 = glove.Linkage(exported: False, section: None, secflags: None)
  linkage4
  |> glove.display_linkage
  |> should.equal("")
}

// Tests for QBE.Typedef
pub fn display_type_def_test() {
  // Test case 1: TypeDef with no alignment and single item
  let def1 = glove.TypeDef("myType", None, [#(glove.Word, 1)])
  def1
  |> glove.display_type_def
  |> should.equal("type :myType = { w }")

  // Test case 2: TypeDef with alignment and multiple items
  let def2 =
    glove.TypeDef(
      "struct",
      Some(4),
      [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
    )
  def2
  |> glove.display_type_def
  |> should.equal("type :struct = align 4 { w 2, w, w 3 }")
  // Add more test cases as needed
}

// Tests for QBE.Types
pub fn display_type_test() {
  // Test case 1: Base types
  let result1 = glove.Word
  result1
  |> glove.display_type
  |> should.equal("w")

  let result2 = glove.Long
  result2
  |> glove.display_type
  |> should.equal("l")

  let result3 = glove.Single
  result3
  |> glove.display_type
  |> should.equal("s")

  let result4 = glove.Double
  result4
  |> glove.display_type
  |> should.equal("d")

  let result5 = glove.Byte
  result5
  |> glove.display_type
  |> should.equal("b")

  let result6 = glove.Halfword
  result6
  |> glove.display_type
  |> should.equal("h")

  // Test case 2: Extended types
  let def1 = glove.TypeDef("myType", None, [#(glove.Word, 1)])
  let result5 = glove.Agreegate(def1)
  result5
  |> glove.display_type
  |> should.equal("type :myType = { w }")
}
