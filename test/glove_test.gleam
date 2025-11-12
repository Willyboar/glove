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
    glove.TypeDef("struct", Some(4), [
      #(glove.Word, 2),
      #(glove.Word, 1),
      #(glove.Word, 3),
    ])
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
    glove.Call(glove.Global("vadd"), [
      #(glove.Single, glove.Temporary("a")),
      #(glove.Long, glove.Temporary("ap")),
    ])
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

  // Test case for comparing integer values (legacy format)
  let comp1 =
    glove.Comp(
      glove.Word,
      glove.Slt,
      glove.Temporary("a"),
      glove.Temporary("b"),
    )
  comp1
  |> glove.display_inst
  |> should.equal("csltw %a, %b")

  // Test case for comparing aggregate types
  let comp2 =
    glove.Comp(
      glove.Aggregate(
        glove.TypeDef("struct", Some(4), [
          #(glove.Word, 2),
          #(glove.Word, 1),
          #(glove.Word, 3),
        ]),
      ),
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
  |> should.equal("storew 42, %r1")

  // Test case for loading a value
  let load = glove.Load(glove.Word, glove.Temporary("r2"))
  load
  |> glove.display_inst
  |> should.equal("loadw %r2")

  // Test case for storing an aggregate value
  let store2 =
    glove.Store(
      glove.Aggregate(
        glove.TypeDef("struct", Some(4), [
          #(glove.Word, 2),
          #(glove.Word, 1),
          #(glove.Word, 3),
        ]),
      ),
      glove.Const(42),
      glove.Temporary("%r5"),
    )
  store2
  |> glove.display_inst
  |> should.equal("Store to an aggregate type")

  // Test case for loading an aggregate value
  let load2 =
    glove.Load(
      glove.Aggregate(
        glove.TypeDef("struct", Some(4), [
          #(glove.Word, 2),
          #(glove.Word, 1),
          #(glove.Word, 3),
        ]),
      ),
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
        glove.Block("@start", [
          glove.Volatile(
            glove.Call(glove.Global("puts"), [
              #(glove.Long, glove.Global("str")),
            ]),
          ),
          glove.Volatile(glove.Ret(Some(glove.Const(0)))),
        ]),
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
    glove.Block("@start", [
      glove.Volatile(
        glove.Call(glove.Global("puts"), [#(glove.Long, glove.Global("str"))]),
      ),
      glove.Volatile(glove.Ret(Some(glove.Const(0)))),
    ]),
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
        glove.Block(label: "@start", statements: [
          glove.Assign(
            glove.Temporary("c"),
            glove.Word,
            glove.Add(glove.Temporary("a"), glove.Temporary("b")),
          ),
          glove.Volatile(glove.Ret(Some(glove.Temporary("c")))),
        ]),
      ],
    )

  let main_func =
    glove.Function(
      linkage: glove.public(),
      name: "main",
      arguments: [],
      return_ty: Some(glove.Word),
      blocks: [
        glove.Block(label: "@start", statements: [
          glove.Assign(
            glove.Temporary("r"),
            glove.Word,
            glove.Call(glove.Global("add"), [
              #(glove.Word, glove.Const(1)),
              #(glove.Word, glove.Const(1)),
            ]),
          ),
          glove.Volatile(
            glove.Call(glove.Global("printf"), [
              #(glove.Long, glove.Global("fmt")),
              #(glove.Word, glove.Temporary("r")),
            ]),
          ),
          glove.Volatile(glove.Ret(Some(glove.Const(0)))),
        ]),
      ],
    )

  let functions = [add_func, main_func]

  let fmt_data =
    glove.DataDef(linkage: glove.private(), name: "fmt", align: None, items: [
      #(glove.Byte, glove.Str("One and one make %d!\n")),
      #(glove.Byte, glove.Constant(0)),
    ])

  let data = [fmt_data]

  let module = glove.Module(functions: functions, types: [], data: data)

  module
  |> glove.display_module
  |> should.equal(
    "function w $add(w %a, w %b) {\n"
    <> "@start\n"
    <> "%c =w add %a, %b\n"
    <> "ret %c\n}\n"
    <> "export function w $main() {\n"
    <> "@start\n"
    <> "%r =w call $add(w 1, w 1)\n"
    <> "call $printf(l $fmt, w %r)\n"
    <> "ret 0\n}\n"
    <> "data $fmt = "
    <> "{ b \"One and one make %d!\n\", b 0 }",
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
  should.equal(updated_module.types, [
    glove.TypeDef("my_type", None, [#(glove.Word, 2), #(glove.Word, 3)]),
  ])
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
    glove.Block(label: "my_block", statements: [
      glove.Assign(
        glove.Temporary("r"),
        glove.Word,
        glove.Call(glove.Global("add"), [
          #(glove.Word, glove.Const(1)),
          #(glove.Word, glove.Const(1)),
        ]),
      ),
      glove.Volatile(glove.Ret(Some(glove.Const(0)))),
      // Non-jump instruction
      glove.Assign(
        glove.Temporary("r"),
        glove.Word,
        glove.Call(glove.Global("add"), [
          #(glove.Word, glove.Const(1)),
          #(glove.Word, glove.Const(1)),
        ]),
      ),
      glove.Volatile(glove.Jmp("label")),
    ])

  should.equal(glove.jumps(block_with_jump), True)

  let block_without_jump =
    glove.Block(label: "my_block", statements: [
      glove.Assign(
        glove.Temporary("r"),
        glove.Word,
        glove.Call(glove.Global("add"), [
          #(glove.Word, glove.Const(1)),
          #(glove.Word, glove.Const(1)),
        ]),
      ),
      glove.Volatile(glove.Ret(Some(glove.Const(0)))),
      // Non-jump instruction
      glove.Assign(
        glove.Temporary("r"),
        glove.Word,
        glove.Call(glove.Global("add"), [
          #(glove.Word, glove.Const(1)),
          #(glove.Word, glove.Const(1)),
        ]),
      ),
      glove.Volatile(glove.Add(glove.Const(1), glove.Const(2))),
    ])

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
    glove.into_abi(
      glove.Aggregate(
        glove.TypeDef("struct", Some(4), [
          #(glove.Word, 2),
          #(glove.Word, 1),
          #(glove.Word, 3),
        ]),
      ),
    ),
    glove.Aggregate(
      glove.TypeDef("struct", Some(4), [
        #(glove.Word, 2),
        #(glove.Word, 1),
        #(glove.Word, 3),
      ]),
    ),
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
    glove.into_base(
      glove.Aggregate(
        glove.TypeDef("struct", Some(4), [
          #(glove.Word, 2),
          #(glove.Word, 1),
          #(glove.Word, 3),
        ]),
      ),
    ),
    glove.Long,
  )
  // Assuming td is a valid `TypeDef`
}

// Tests for new instructions
// Tests for new arithmetic and bitwise instructions
pub fn new_arithmetic_inst_test() {
  // Unsigned division
  glove.Udiv(glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("udiv %a, %b")

  // Unsigned remainder
  glove.Urem(glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("urem %a, %b")

  // Negation
  glove.Neg(glove.Temporary("a"))
  |> glove.display_inst
  |> should.equal("neg %a")

  // XOR
  glove.Xor(glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("xor %a, %b")

  // Arithmetic right shift
  glove.Sar(glove.Temporary("a"), glove.Const(4))
  |> glove.display_inst
  |> should.equal("sar %a, 4")

  // Logical right shift
  glove.Shr(glove.Temporary("a"), glove.Const(4))
  |> glove.display_inst
  |> should.equal("shr %a, 4")

  // Left shift
  glove.Shl(glove.Temporary("a"), glove.Const(2))
  |> glove.display_inst
  |> should.equal("shl %a, 2")
}

// Tests for new store instructions
pub fn new_store_inst_test() {
  // Store word
  glove.Storew(glove.Const(42), glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("storew 42, %ptr")

  // Store long
  glove.Storel(glove.Const(100), glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("storel 100, %ptr")

  // Store single
  glove.Stores(glove.Temporary("val"), glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("stores %val, %ptr")

  // Store double
  glove.Stored(glove.Temporary("val"), glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("stored %val, %ptr")

  // Store half-word
  glove.Storeh(glove.Const(10), glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("storeh 10, %ptr")

  // Store byte
  glove.Storeb(glove.Const(255), glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("storeb 255, %ptr")
}

// Tests for new load instructions
pub fn new_load_inst_test() {
  // Load word
  glove.Loadw(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadw %ptr")

  // Load word with sign extension
  glove.Loadsw(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadsw %ptr")

  // Load word with zero extension
  glove.Loaduw(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loaduw %ptr")

  // Load long
  glove.Loadl(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadl %ptr")

  // Load single
  glove.Loads(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loads %ptr")

  // Load double
  glove.Loadd(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadd %ptr")

  // Load half-word signed
  glove.Loadsh(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadsh %ptr")

  // Load half-word unsigned
  glove.Loaduh(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loaduh %ptr")

  // Load byte signed
  glove.Loadsb(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadsb %ptr")

  // Load byte unsigned
  glove.Loadub(glove.Temporary("ptr"))
  |> glove.display_inst
  |> should.equal("loadub %ptr")
}

// Tests for signed integer comparisons
pub fn signed_comparison_inst_test() {
  // Signed less than (word)
  glove.Comp(glove.Word, glove.Sltw, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("csltw %a, %b")

  // Signed less than (long)
  glove.Comp(glove.Long, glove.Sltl, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("csltl %a, %b")

  // Signed less or equal (word)
  glove.Comp(glove.Word, glove.Slew, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("cslew %a, %b")

  // Signed less or equal (long)
  glove.Comp(glove.Long, glove.Slel, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("cslel %a, %b")

  // Signed greater than (word)
  glove.Comp(glove.Word, glove.Sgtw, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("csgtw %a, %b")

  // Signed greater than (long)
  glove.Comp(glove.Long, glove.Sgtl, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("csgtl %a, %b")

  // Signed greater or equal (word)
  glove.Comp(glove.Word, glove.Sgew, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("csgew %a, %b")

  // Signed greater or equal (long)
  glove.Comp(glove.Long, glove.Sgel, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("csgel %a, %b")
}

// Tests for unsigned integer comparisons
pub fn unsigned_comparison_inst_test() {
  // Unsigned less than (word)
  glove.Comp(
    glove.Word,
    glove.Cultw,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cultw %a, %b")

  // Unsigned less than (long)
  glove.Comp(
    glove.Long,
    glove.Cultl,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cultl %a, %b")

  // Unsigned less or equal (word)
  glove.Comp(
    glove.Word,
    glove.Culew,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("culew %a, %b")

  // Unsigned less or equal (long)
  glove.Comp(
    glove.Long,
    glove.Culel,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("culel %a, %b")

  // Unsigned greater than (word)
  glove.Comp(
    glove.Word,
    glove.Cugtw,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cugtw %a, %b")

  // Unsigned greater than (long)
  glove.Comp(
    glove.Long,
    glove.Cugtl,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cugtl %a, %b")

  // Unsigned greater or equal (word)
  glove.Comp(
    glove.Word,
    glove.Cugew,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cugew %a, %b")

  // Unsigned greater or equal (long)
  glove.Comp(
    glove.Long,
    glove.Cugel,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cugel %a, %b")
}

// Tests for equality comparisons
pub fn equality_comparison_inst_test() {
  // Equal (word)
  glove.Comp(glove.Word, glove.Ceqw, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("ceqw %a, %b")

  // Equal (long)
  glove.Comp(glove.Long, glove.Ceql, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("ceql %a, %b")

  // Not equal (word)
  glove.Comp(glove.Word, glove.Cnew, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("cnew %a, %b")

  // Not equal (long)
  glove.Comp(glove.Long, glove.Cnel, glove.Temporary("a"), glove.Temporary("b"))
  |> glove.display_inst
  |> should.equal("cnel %a, %b")
}

// Tests for floating-point comparisons
pub fn float_comparison_inst_test() {
  // Equal (single)
  glove.Comp(
    glove.Single,
    glove.Ceqs,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("ceqs %a, %b")

  // Equal (double)
  glove.Comp(
    glove.Double,
    glove.Ceqd,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("ceqd %a, %b")

  // Not equal (single)
  glove.Comp(
    glove.Single,
    glove.Cnes,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cnes %a, %b")

  // Not equal (double)
  glove.Comp(
    glove.Double,
    glove.Cned,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cned %a, %b")

  // Less than (single)
  glove.Comp(
    glove.Single,
    glove.Clts,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("clts %a, %b")

  // Less than (double)
  glove.Comp(
    glove.Double,
    glove.Cltd,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cltd %a, %b")

  // Less or equal (single)
  glove.Comp(
    glove.Single,
    glove.Cles,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cles %a, %b")

  // Less or equal (double)
  glove.Comp(
    glove.Double,
    glove.Cled,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cled %a, %b")

  // Greater than (single)
  glove.Comp(
    glove.Single,
    glove.Cgts,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cgts %a, %b")

  // Greater than (double)
  glove.Comp(
    glove.Double,
    glove.Cgtd,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cgtd %a, %b")

  // Greater or equal (single)
  glove.Comp(
    glove.Single,
    glove.Cges,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cges %a, %b")

  // Greater or equal (double)
  glove.Comp(
    glove.Double,
    glove.Cged,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cged %a, %b")

  // Ordered (single)
  glove.Comp(
    glove.Single,
    glove.Cos,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cos %a, %b")

  // Ordered (double)
  glove.Comp(
    glove.Double,
    glove.Cod,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cod %a, %b")

  // Unordered (single)
  glove.Comp(
    glove.Single,
    glove.Cuos,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cuos %a, %b")

  // Unordered (double)
  glove.Comp(
    glove.Double,
    glove.Cuod,
    glove.Temporary("a"),
    glove.Temporary("b"),
  )
  |> glove.display_inst
  |> should.equal("cuod %a, %b")
}

// Tests for conversion instructions
pub fn conversion_inst_test() {
  // Sign-extend word to long
  glove.Extsw(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("extsw %val")

  // Zero-extend word to long
  glove.Extuw(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("extuw %val")

  // Sign-extend half-word
  glove.Extsh(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("extsh %val")

  // Zero-extend half-word
  glove.Extuh(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("extuh %val")

  // Sign-extend byte
  glove.Extsb(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("extsb %val")

  // Zero-extend byte
  glove.Extub(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("extub %val")

  // Extend single to double
  glove.Exts(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("exts %val")

  // Truncate double to single
  glove.Truncd(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("truncd %val")

  // Single to signed integer
  glove.Stosi(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("stosi %val")

  // Single to unsigned integer
  glove.Stoui(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("stoui %val")

  // Double to signed integer
  glove.Dtosi(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("dtosi %val")

  // Double to unsigned integer
  glove.Dtoui(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("dtoui %val")

  // Signed word to float
  glove.Swtof(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("swtof %val")

  // Unsigned word to float
  glove.Uwtof(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("uwtof %val")

  // Signed long to float
  glove.Sltof(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("sltof %val")

  // Unsigned long to float
  glove.Ultof(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("ultof %val")

  // Cast
  glove.Cast(glove.Temporary("val"))
  |> glove.display_inst
  |> should.equal("cast %val")
}

// Tests for control flow instructions
pub fn control_flow_inst_test() {
  // Hlt
  glove.Hlt
  |> glove.display_inst
  |> should.equal("hlt")

  // Phi node
  glove.Phi([
    #("start", glove.Const(0)),
    #("loop", glove.Temporary("x")),
    #("end", glove.Const(1)),
  ])
  |> glove.display_inst
  |> should.equal("phi @start 0, @loop %x, @end 1")
}

// Tests for variadic instructions
pub fn variadic_inst_test() {
  // Vastart
  glove.Vastart(glove.Temporary("valist"))
  |> glove.display_inst
  |> should.equal("vastart %valist")

  // Vaarg
  glove.Vaarg(glove.Temporary("valist"))
  |> glove.display_inst
  |> should.equal("vaarg %valist")
}

// Tests for size and align functions
pub fn size_basic_types_test() {
  // Test basic type sizes
  glove.Byte
  |> glove.size
  |> should.equal(1)

  glove.Halfword
  |> glove.size
  |> should.equal(2)

  glove.Word
  |> glove.size
  |> should.equal(4)

  glove.Single
  |> glove.size
  |> should.equal(4)

  glove.Long
  |> glove.size
  |> should.equal(8)

  glove.Double
  |> glove.size
  |> should.equal(8)
}

pub fn align_basic_types_test() {
  // For non-aggregate types, alignment equals size
  glove.Byte
  |> glove.align
  |> should.equal(1)

  glove.Halfword
  |> glove.align
  |> should.equal(2)

  glove.Word
  |> glove.align
  |> should.equal(4)

  glove.Long
  |> glove.align
  |> should.equal(8)
}

pub fn size_aggregate_test() {
  // Test struct with padding: { w, b, w }
  // Layout: [wwww][b___][wwww]
  // Offsets: 0-3 (word), 4 (byte), 5-7 (padding), 8-11 (word)
  // Size: 12 bytes (with 4-byte alignment)
  let struct1 =
    glove.TypeDef("test1", None, [
      #(glove.Word, 1),
      // 4 bytes at offset 0
      #(glove.Byte, 1),
      // 1 byte at offset 4
      #(glove.Word, 1),
      // 4 bytes at offset 8 (after 3 bytes padding)
    ])

  glove.Aggregate(struct1)
  |> glove.size
  |> should.equal(12)

  // Test struct with array: { w 3 }
  // Size: 12 bytes (3 * 4)
  let struct2 = glove.TypeDef("test2", None, [#(glove.Word, 3)])

  glove.Aggregate(struct2)
  |> glove.size
  |> should.equal(12)

  // Test empty struct
  let empty_struct = glove.TypeDef("empty", None, [])

  glove.Aggregate(empty_struct)
  |> glove.size
  |> should.equal(0)

  // Test struct with mixed types: { l, b, h }
  // Layout: [llllllll][b_][hh]
  // Offsets: 0-7 (long), 8 (byte), 9 (padding), 10-11 (halfword)
  // Size: 16 bytes (with 8-byte alignment from long)
  let struct3 =
    glove.TypeDef("test3", None, [
      #(glove.Long, 1),
      // 8 bytes
      #(glove.Byte, 1),
      // 1 byte
      #(glove.Halfword, 1),
      // 2 bytes
    ])

  glove.Aggregate(struct3)
  |> glove.size
  |> should.equal(16)
}

pub fn align_aggregate_test() {
  // Test struct alignment without explicit align
  // Alignment should be max of member alignments
  let struct1 =
    glove.TypeDef("test1", None, [
      #(glove.Word, 1),
      // align 4
      #(glove.Byte, 1),
      // align 1
    ])

  glove.Aggregate(struct1)
  |> glove.align
  |> should.equal(4)
  // max(4, 1) = 4

  // Test struct with long (8-byte alignment)
  let struct2 =
    glove.TypeDef("test2", None, [
      #(glove.Long, 1),
      // align 8
      #(glove.Halfword, 1),
      // align 2
    ])

  glove.Aggregate(struct2)
  |> glove.align
  |> should.equal(8)
  // max(8, 2) = 8

  // Test struct with explicit alignment
  let struct3 =
    glove.TypeDef("test3", Some(16), [
      #(glove.Word, 1),
      // align 4
    ])

  glove.Aggregate(struct3)
  |> glove.align
  |> should.equal(16)
  // explicit alignment

  // Test empty struct alignment
  let empty_struct = glove.TypeDef("empty", None, [])

  glove.Aggregate(empty_struct)
  |> glove.align
  |> should.equal(1)
  // default for empty
}
