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
