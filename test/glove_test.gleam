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
  |> should.equal("ret 0\n")

  let empty_volatile = glove.Volatile(glove.Ret(None))
  empty_volatile
  |> glove.display_statement
  |> should.equal("ret\n")
}

// Tests for QBE.Block Display
pub fn display_block_test() {
  // Test empty block
  let empty_block = glove.Block("label", [])
  empty_block
  |> glove.display_block()
  |> should.equal("label\n")

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
  |> should.equal("label\n%temp1 =w add %a, %b\nret %temp1\n")
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
  let result5 = glove.Aggregate(def1)
  result5
  |> glove.display_type
  |> should.equal("type :myType = { w }")
}

// Tests for QBE.Instructions
pub fn display_inst_test() {
  // Test case for Call with 2 args
  let call1 =
    glove.Call(
      glove.Global("vadd"),
      [
        #(glove.Single, glove.Temporary("a")),
        #(glove.Long, glove.Temporary("ap")),
      ],
    )
  call1
  |> glove.display_inst
  |> should.equal("call $vadd(s %a, l %ap)")

  //Test case for empty call
  let call2 = glove.Call(glove.Global("vsub"), [])
  call2
  |> glove.display_inst
  |> should.equal("call $vsub()")

  // Test case for call with single arg
  let call3 =
    glove.Call(glove.Global("div"), [#(glove.Single, glove.Temporary("b"))])
  call3
  |> glove.display_inst
  |> should.equal("call $div(s %b)")

  // Test case for add instruction
  let add = glove.Add(glove.Temporary("a"), glove.Temporary("b"))
  add
  |> glove.display_inst
  |> should.equal("add %a, %b")

  // Test case for sub instruction
  let sub = glove.Sub(glove.Temporary("a"), glove.Temporary("b"))
  sub
  |> glove.display_inst
  |> should.equal("sub %a, %b")

  // Test case for mul instruction
  let mul = glove.Mul(glove.Temporary("a"), glove.Temporary("b"))
  mul
  |> glove.display_inst
  |> should.equal("mul %a, %b")

  // Test case for div instruction
  let div = glove.Div(glove.Temporary("a"), glove.Temporary("b"))
  div
  |> glove.display_inst
  |> should.equal("div %a, %b")

  // Test case for rem instruction
  let rem = glove.Rem(glove.Temporary("a"), glove.Temporary("b"))
  rem
  |> glove.display_inst
  |> should.equal("rem %a, %b")

  // Test case for comparing integer values
  let comp1 =
    glove.Comp(
      glove.Word,
      glove.Slt,
      glove.Temporary("a"),
      glove.Temporary("b"),
    )
  comp1
  |> glove.display_inst
  |> should.equal("cslt w %a %b")

  // Test case for comparing aggregate types
  let comp2 =
    glove.Comp(
      glove.Aggregate(glove.TypeDef(
        "struct",
        Some(4),
        [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
      )),
      glove.Eq,
      glove.Temporary("a"),
      glove.Temporary("b"),
    )
  comp2
  |> glove.display_inst
  |> should.equal("Cannot Compare aggregate types")

  // Test case for logical AND
  let and = glove.And(glove.Temporary("a"), glove.Temporary("b"))
  and
  |> glove.display_inst
  |> should.equal("and %a, %b")

  // Test case for logical OR
  let or = glove.Or(glove.Temporary("a"), glove.Temporary("b"))
  or
  |> glove.display_inst
  |> should.equal("or %a, %b")

  // Test case for copying a value
  let copy = glove.Copy(glove.Temporary("a"))
  copy
  |> glove.display_inst
  |> should.equal("copy %a")

  // Test case for returning a value
  let ret1 = glove.Ret(Some(glove.Const(10)))
  ret1
  |> glove.display_inst
  |> should.equal("ret 10\n")

  // Test case for returning without a value
  let ret2 = glove.Ret(None)
  ret2
  |> glove.display_inst
  |> should.equal("ret\n")

  // Test case for conditional jump if nonzero
  let jnz = glove.Jnz(glove.Const(1), "label1", "label2")
  jnz
  |> glove.display_inst
  |> should.equal("jnz 1, @label1, @label2")

  // Test case for unconditional jump
  let jmp = glove.Jmp("label")
  jmp
  |> glove.display_inst
  |> should.equal("jmp @label")

  // Test case for allocating 4 bytes
  let alloc4 = glove.Alloc4(16)
  alloc4
  |> glove.display_inst
  |> should.equal("alloc4 16")

  // Test case for allocating 8 bytes
  let alloc8 = glove.Alloc8(32)
  alloc8
  |> glove.display_inst
  |> should.equal("alloc8 32")

  // Test case for allocating 16 bytes
  let alloc16 = glove.Alloc16(64)
  alloc16
  |> glove.display_inst
  |> should.equal("alloc16 64")

  // Test case for storing a value
  let store = glove.Store(glove.Word, glove.Const(42), glove.Temporary("r1"))
  store
  |> glove.display_inst
  |> should.equal("storew 42 %r1")

  // Test case for loading a value
  let load = glove.Load(glove.Word, glove.Temporary("r2"))
  load
  |> glove.display_inst
  |> should.equal("loadw %r2")

  // Test case for storing an aggregate value
  let store2 =
    glove.Store(
      glove.Aggregate(glove.TypeDef(
        "struct",
        Some(4),
        [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
      )),
      glove.Const(42),
      glove.Temporary("%r5"),
    )
  store2
  |> glove.display_inst
  |> should.equal("Store to an aggregate type")

  // Test case for loading an aggregate value
  let load2 =
    glove.Load(
      glove.Aggregate(glove.TypeDef(
        "struct",
        Some(4),
        [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
      )),
      glove.Temporary("%r6"),
    )
  load2
  |> glove.display_inst
  |> should.equal("Load aggregate type")

  // Test case for blitting values
  let blit = glove.Blit(glove.Temporary("r3"), glove.Temporary("r4"), 8)
  blit
  |> glove.display_inst
  |> should.equal("blit %r3, %r4, 8")
}

pub fn display_data_def_test() {
  // Test case with all fields populated
  let linkage =
    glove.Linkage(
      exported: True,
      section: Some("mysection"),
      secflags: Some("flags"),
    )
  let items = [
    #(glove.Single, glove.Constant(42)),
    #(glove.Double, glove.Constant(3)),
  ]
  let def = glove.DataDef(linkage, "mydata", Some(4), items)

  let expected =
    "export section \"mysection\" \"flags\" data $mydata = align 4 { s 42, d 3 }"
  let result = glove.display_data_def(def)
  should.equal(result, expected)
}

// Tests for Linkage functions
// Test for Linkage private without section
pub fn private_test() {
  let expected = glove.Linkage(exported: False, section: None, secflags: None)
  let result = glove.private()
  should.equal(result, expected)
}

// Test for Linkage private with section
pub fn private_with_section_test() {
  let section = "mysection"
  let expected =
    glove.Linkage(exported: False, section: Some(section), secflags: None)
  let result = glove.private_with_section(section)
  should.equal(result, expected)
}

// Test for Linkage public without section
pub fn public_test() {
  let expected = glove.Linkage(exported: True, section: None, secflags: None)
  let result = glove.public()
  should.equal(result, expected)
}

// Test for Linkage public with section
pub fn public_with_section_test() {
  let section = "mysection"
  let expected =
    glove.Linkage(exported: True, section: Some(section), secflags: None)
  let result = glove.public_with_section(section)
  should.equal(result, expected)
}

// Tests for display QBE.Datadef
pub fn display_data_test() {
  let data_def =
    glove.DataDef(
      linkage: glove.Linkage(exported: True, section: None, secflags: None),
      name: "str",
      align: None,
      items: [
        #(glove.Byte, glove.Str("hello world")),
        #(glove.Byte, glove.Constant(0)),
      ],
    )

  let expected = "export data $str = { b \"hello world\", b 0 }"
  let result = glove.display_data_def(data_def)
  should.equal(result, expected)
}

// Tests for display QBE.functions
pub fn display_function_test() {
  let function =
    glove.Function(
      linkage: glove.Linkage(exported: True, section: None, secflags: None),
      name: "main",
      arguments: [],
      return_ty: Some(glove.Word),
      blocks: [
        glove.Block(
          "@start",
          [
            glove.Volatile(glove.Call(
              glove.Global("puts"),
              [#(glove.Long, glove.Global("str"))],
            )),
            glove.Volatile(glove.Ret(Some(glove.Const(0)))),
          ],
        ),
      ],
    )

  let expected =
    "export function w $main() {\n@start\ncall $puts(l $str)\nret 0\n}"

  let result = glove.display_function(function)
  should.equal(result, expected)
}

// Tests for display QBE.Blocks
pub fn display_blocks_test() {
  let blocks = [
    glove.Block(
      "@start",
      [
        glove.Volatile(glove.Call(
          glove.Global("puts"),
          [#(glove.Long, glove.Global("str"))],
        )),
        glove.Volatile(glove.Ret(Some(glove.Const(0)))),
      ],
    ),
  ]

  let expected = "@start\ncall $puts(l $str)\nret 0\n"

  let result = glove.display_blocks(blocks)
  should.equal(result, expected)
}

// TEst for display arguments
pub fn display_arguments_test() {
  let arguments = [
    #(glove.Word, glove.Global("arg1")),
    #(glove.Byte, glove.Global("arg2")),
    #(glove.Long, glove.Global("arg3")),
  ]

  let expected = "w $arg1, b $arg2, l $arg3"

  let result = glove.display_arguments(arguments)
  should.equal(result, expected)
}

// Tests for display QBE.Modules
pub fn display_module_test() {
  // Test case with empty module
  let empty_module = glove.Module(functions: [], types: [], data: [])
  empty_module
  |> glove.display_module
  |> should.equal("\n")

  // Test case with functions, types, and data
  let add_func =
    glove.Function(
      linkage: glove.private(),
      name: "add",
      arguments: [
        #(glove.Word, glove.Temporary("a")),
        #(glove.Word, glove.Temporary("b")),
      ],
      return_ty: Some(glove.Word),
      blocks: [
        glove.Block(
          label: "@start",
          statements: [
            glove.Assign(
              glove.Temporary("c"),
              glove.Word,
              glove.Add(glove.Temporary("a"), glove.Temporary("b")),
            ),
            glove.Volatile(glove.Ret(Some(glove.Temporary("c")))),
          ],
        ),
      ],
    )

  let main_func =
    glove.Function(
      linkage: glove.public(),
      name: "main",
      arguments: [],
      return_ty: Some(glove.Word),
      blocks: [
        glove.Block(
          label: "@start",
          statements: [
            glove.Assign(
              glove.Temporary("r"),
              glove.Word,
              glove.Call(
                glove.Global("add"),
                [#(glove.Word, glove.Const(1)), #(glove.Word, glove.Const(1))],
              ),
            ),
            glove.Volatile(glove.Call(
              glove.Global("printf"),
              [
                #(glove.Long, glove.Global("fmt")),
                #(glove.Word, glove.Temporary("r")),
              ],
            )),
            glove.Volatile(glove.Ret(Some(glove.Const(0)))),
          ],
        ),
      ],
    )

  let functions = [add_func, main_func]

  let fmt_data =
    glove.DataDef(
      linkage: glove.private(),
      name: "fmt",
      align: None,
      items: [
        #(glove.Byte, glove.Str("One and one make %d!\n")),
        #(glove.Byte, glove.Constant(0)),
      ],
    )

  let data = [fmt_data]

  let module = glove.Module(functions: functions, types: [], data: data)

  module
  |> glove.display_module
  |> should.equal(
    "function w $add(w %a, w %b) {\n" <> "@start\n" <> "%c =w add %a, %b\n" <> "ret %c\n}\n" <> "export function w $main() {\n" <> "@start\n" <> "%r =w call $add(w 1, w 1)\n" <> "call $printf(l $fmt, w %r)\n" <> "ret 0\n}\n" <> "data $fmt = " <> "{ b \"One and one make %d!\n\", b 0 }",
  )
}

// Test for new_datadef function
pub fn new_datadef_test() {
  let datadef = glove.new_datadef()

  // Assert the default values
  should.equal(datadef.linkage, glove.private())
  should.equal(datadef.name, "")
  should.equal(datadef.align, None)
  should.equal(datadef.items, [])
}

pub fn new_module_test() {
  let module = glove.new_module()

  // Assert empty lists for functions, types, and data
  should.equal(module.functions, [])
  should.equal(module.types, [])
  should.equal(module.data, [])
}

pub fn add_function_test() {
  let module = glove.new_module()
  let function = glove.new_function()
  let updated_module = glove.add_function(module, function)

  // Assert the function is added to the module
  should.equal(updated_module.functions, [function])
  should.equal(updated_module.types, [])
  should.equal(updated_module.data, [])
}

pub fn add_data_test() {
  let module = glove.new_module()
  let data_def = glove.new_datadef()
  let updated_module = glove.add_data(module, data_def)

  // Assert the data definition is added to the module
  should.equal(updated_module.functions, [])
  should.equal(updated_module.types, [])
  should.equal(updated_module.data, [data_def])
}

pub fn add_type_test() {
  let module = glove.new_module()
  let type_def =
    glove.TypeDef("my_type", None, [#(glove.Word, 2), #(glove.Word, 3)])
  let updated_module = glove.add_type(module, type_def)

  // Assert the type definition is added to the module
  should.equal(updated_module.functions, [])
  should.equal(
    updated_module.types,
    [glove.TypeDef("my_type", None, [#(glove.Word, 2), #(glove.Word, 3)])],
  )
  should.equal(updated_module.data, [])
}

// Test for add instruction function
pub fn add_inst_test() {
  let block = glove.Block(label: "my_block", statements: [])
  let inst = glove.Call(glove.Global("foo"), [])
  let new_block = glove.add_inst(block, inst)
  should.equal(new_block.statements, [glove.Volatile(inst)])
}

// Test for assign instruction function
pub fn assign_inst_test() {
  let block = glove.Block(label: "my_block", statements: [])
  let val = glove.Temporary("tmp")
  let typ = glove.Word
  let inst = glove.Call(glove.Global("bar"), [])
  let new_block = glove.assign_inst(block, val, typ, inst)
  should.equal(new_block.statements, [glove.Assign(val, typ, inst)])
}

pub fn jumps_test() {
  let block_with_jump =
    glove.Block(
      label: "my_block",
      statements: [
        glove.Assign(
          glove.Temporary("r"),
          glove.Word,
          glove.Call(
            glove.Global("add"),
            [#(glove.Word, glove.Const(1)), #(glove.Word, glove.Const(1))],
          ),
        ),
        glove.Volatile(glove.Ret(Some(glove.Const(0)))),
        // Non-jump instruction
        glove.Assign(
          glove.Temporary("r"),
          glove.Word,
          glove.Call(
            glove.Global("add"),
            [#(glove.Word, glove.Const(1)), #(glove.Word, glove.Const(1))],
          ),
        ),
        glove.Volatile(glove.Jmp("label")),
      ],
    )

  should.equal(glove.jumps(block_with_jump), True)

  let block_without_jump =
    glove.Block(
      label: "my_block",
      statements: [
        glove.Assign(
          glove.Temporary("r"),
          glove.Word,
          glove.Call(
            glove.Global("add"),
            [#(glove.Word, glove.Const(1)), #(glove.Word, glove.Const(1))],
          ),
        ),
        glove.Volatile(glove.Ret(Some(glove.Const(0)))),
        // Non-jump instruction
        glove.Assign(
          glove.Temporary("r"),
          glove.Word,
          glove.Call(
            glove.Global("add"),
            [#(glove.Word, glove.Const(1)), #(glove.Word, glove.Const(1))],
          ),
        ),
        glove.Volatile(glove.Add(glove.Const(1), glove.Const(2))),
      ],
    )

  should.equal(glove.jumps(block_without_jump), False)
}

// Test add_block function
pub fn add_block_test() {
  let block = glove.add_block("my_block")
  should.equal(block.label, "my_block")
  should.equal(block.statements, [])
}

// Test last_block function
pub fn last_block_test() {
  let blocks = [
    glove.Block(label: "block1", statements: []),
    glove.Block(label: "block2", statements: []),
    glove.Block(label: "block3", statements: []),
  ]

  should.equal(
    glove.last_block(blocks),
    Some(glove.Block(label: "block3", statements: [])),
  )

  let empty_blocks: List(glove.Block) = []
  should.equal(glove.last_block(empty_blocks), None)
}

// Test for new function
pub fn new_function_test() {
  let function = glove.new_function()
  should.equal(function.linkage, glove.private())
  should.equal(function.name, "")
  should.equal(function.arguments, [])
  should.equal(function.return_ty, None)
  should.equal(function.blocks, [])
}

pub fn into_abi_test() {
  should.equal(glove.into_abi(glove.Byte), glove.Word)
  should.equal(glove.into_abi(glove.Halfword), glove.Word)
  should.equal(glove.into_abi(glove.Word), glove.Word)
  should.equal(glove.into_abi(glove.Single), glove.Single)
  should.equal(glove.into_abi(glove.Long), glove.Long)
  should.equal(glove.into_abi(glove.Double), glove.Double)
  should.equal(
    glove.into_abi(glove.Aggregate(glove.TypeDef(
      "struct",
      Some(4),
      [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
    ))),
    glove.Aggregate(glove.TypeDef(
      "struct",
      Some(4),
      [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
    )),
  )
  // Assuming td is a valid `TypeDef`
}

pub fn into_base_test() {
  should.equal(glove.into_base(glove.Byte), glove.Word)
  should.equal(glove.into_base(glove.Halfword), glove.Word)
  should.equal(glove.into_base(glove.Word), glove.Word)
  should.equal(glove.into_base(glove.Single), glove.Single)
  should.equal(glove.into_base(glove.Long), glove.Long)
  should.equal(glove.into_base(glove.Double), glove.Double)
  should.equal(
    glove.into_base(glove.Aggregate(glove.TypeDef(
      "struct",
      Some(4),
      [#(glove.Word, 2), #(glove.Word, 1), #(glove.Word, 3)],
    ))),
    glove.Long,
  )
  // Assuming td is a valid `TypeDef`
}
