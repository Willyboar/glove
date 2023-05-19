import gleam/int
import gleam/string
import gleam/option.{None, Option, Some}
import gleam/list

// QBE Comparison Operators
pub type Comp {
  // Less Than
  Slt
  // Less or Equal
  Sle
  // Greater than
  Sgt
  // Greater or equal
  Sge
  // Equal
  Eq
  // Not equal
  Ne
}

// QBE instruction
pub type Inst {
  // Adds values 
  Add(Value, Value)
  // Substracts value(b) from value(a)
  Sub(Value, Value)
  // Multiplies values 
  Mul(Value, Value)
  // Divides value(a) by value(b)
  Div(Value, Value)
  // Returns a remaider from division
  Rem(Value, Value)
  // Perform a Comparison
  Comp(Type, Comp, Value, Value)
  // Bitwise AND
  And(Value, Value)
  // Bitwise OR
  Or(Value, Value)
  // Copies either temporary or literal value
  Copy(Value)
  // Return from a function, optionally with a value
  Ret(Option(Value))
  // Jumps to first label if a value is nonzero or to the second one otherwise
  Jnz(Value, String, String)
  // Unconditionally jumps to a label
  Jmp(String)
  // Calls a function
  Call(Value, #(Type, Value))
  // Allocates a 4-byte aligned area on the stack
  Alloc4(Int)
  // Allocates a 8-byte aligned area on the stack
  Alloc8(Int)
  // Allocates a 16-byte aligned area on the stack
  Alloc16(Int)
  // Stores a value into memory pointed to by destination.
  // (type, destination, value)
  Store(Type, Value, Value)
  // Loads a value from memory pointed to by source
  // (type, source)
  Load(Type, Value)
  // (source, destination, n)
  //
  // Copy `n` bytes from the source address to the destination address.
  //
  // n must be a constant value.
  //
  // Minimum supported QBE version 1.1
  Blit(Value, Value, Int)
}

// Display function for Instructions
// UNFINISHED
pub fn display_inst(inst: Inst) -> String {
  case inst {
    Add(a, b) -> "add " <> display_value(a) <> ", " <> display_value(b)
    Sub(a, b) -> "sub" <> display_value(a) <> ", " <> display_value(b)
    Mul(a, b) -> "mul" <> display_value(a) <> ", " <> display_value(b)
    Div(a, b) -> "div" <> display_value(a) <> ", " <> display_value(b)
    Rem(a, b) -> "rem" <> display_value(a) <> ", " <> display_value(b)
    Comp(ty, cmp, a, b) -> {
      case ty {
        Agreegate(_) -> "Cannot Compare aggregate types"
        _ ->
          case cmp {
            Slt ->
              "c" <> "slt" <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Sle ->
              "c" <> "sle" <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Sgt ->
              "c" <> "sgt" <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Sge ->
              "c" <> "sge" <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Eq ->
              "c" <> "eq" <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
            Ne ->
              "c" <> "ne" <> display_type(ty) <> " " <> display_value(a) <> " " <> display_value(
                b,
              )
          }
      }
    }
    And(a, b) -> "and " <> display_value(a) <> ", " <> display_value(b)
    Or(a, b) -> "or " <> display_value(a) <> ", " <> display_value(b)
    Copy(val) -> "copy " <> display_value(val)
    Ret(val) -> {
      case val {
        Some(val) -> "ret " <> display_value(val)
        None -> "ret"
      }
    }
    Jnz(val, if_nonzero, if_zero) ->
      "jnz " <> display_value(val) <> ", @" <> if_nonzero <> ", @" <> if_zero
    Jmp(str) -> "jmp @" <> str
    Call(val1, #(typ, val2)) -> "call"
    Alloc4(int) -> "alloc4 " <> int.to_string(int)
    Alloc8(int) -> "alloc8 " <> int.to_string(int)
    Alloc16(int) -> "alloc16 " <> int.to_string(int)
    Store(typ, value, dest) ->
      case typ {
        Agreegate(_) -> "Store to an aggregate type"
        _ ->
          "store" <> display_type(typ) <> " " <> display_value(value) <> " " <> display_value(
            dest,
          )
      }

    Load(typ, val) ->
      case typ {
        Agreegate(_) -> "Load aggregate type"
        _ -> "load" <> display_type(typ) <> " " <> display_value(val)
      }
    Blit(src, dest, n) ->
      "blit " <> display_value(src) <> ", " <> display_value(dest) <> ", " <> int.to_string(
        n,
      )
  }
}

// QBE Value for instructions
pub type Value {
  // `%`-temporary
  Temporary(name: String)
  // `$`-global
  Global(name: String)
  // Constant
  Const(value: Int)
}

// Display Value function
pub fn display_value(value: Value) -> String {
  case value {
    Temporary(name) -> "%" <> name
    Global(name) -> "$" <> name
    Const(value) -> int.to_string(value)
  }
}

// QBE Types
pub type Type {
  // Base Types
  Word
  Long
  Single
  Double
  // Extended Types
  Byte
  Halfword
  Agreegate(TypeDef)
}

//

// Display Type function
pub fn display_type(ty: Type) -> String {
  case ty {
    Byte -> "b"
    Halfword -> "h"
    Word -> "w"
    Long -> "l"
    Single -> "s"
    Double -> "d"
    // Aggregate type with a specified name
    Agreegate(ty) -> display_type_def(ty)
  }
}

// Returns a C ABI type. Extended types are converted to closest base
// types
pub fn into_abi() -> Nil {
  todo
}

// Returns the closest base type
pub fn into_base() -> Nil {
  todo
}

// Returns byte size for values of the type
pub fn size() -> Int {
  todo
}

// QBE data definition
pub type DataDef {
  DataDef(linkage: Linkage, name: String, align: Int, items: #(Type, DataItem))
}

pub fn new_datadef() -> DataDef {
  todo
}

// Display function for Datadef
pub fn display_datadef() -> String {
  todo
}

// QBE aggregate type definition
pub type TypeDef {
  TypeDef(name: String, align: Option(Int), items: #(Type, Int))
}

// Display function for TypeDef
pub fn display_type_def(def: TypeDef) -> String {
  let align_str = case def.align {
    Some(align) -> "align " <> int.to_string(align) <> " "
    None -> ""
  }

  let items_str = "asdf"

  "type :" <> def.name <> " = " <> align_str <> "{ " <> items_str <> " }"
}

// QBE Data definition item
pub type DataItem {
  // Symbol and offset
  Symbol(String, Option(Int))
  // String
  Str(String)
  // Integer
  Constant(Int)
}

// Display function for DataItem
pub fn display_data_item(item: DataItem) -> String {
  case item {
    Symbol(name, offset) -> {
      case offset {
        Some(off) -> "$" <> name <> " +" <> int.to_string(off)
        None -> "$" <> name
      }
    }
    Str(string) -> "\"" <> string <> "\""
    Constant(val) -> int.to_string(val)
  }
}

// IR Statement
pub type Statement {
  Assign(Value, Type, Inst)
  Volatile(Inst)
}

// Display function for Statement 
pub fn display_statement(stmt: Statement) -> String {
  case stmt {
    Assign(val, typ, inst) ->
      display_value(val) <> " =" <> display_type(typ) <> " " <> display_inst(
        inst,
      )
    Volatile(inst) -> display_inst(inst)
  }
}

// Function block with a label
pub type Block {
  Block(label: String, statements: List(Statement))
}

// Display function for block
pub fn display_block(block: Block) -> String {
  let label = block.label
  let statements =
    block.statements
    |> list.map(display_statement)
    |> string.join("\n")

  label <> ":\n" <> statements
}

/// Adds a new instruction to the block
pub fn add_inst() -> Nil {
  todo
}

/// Adds a new instruction assigned to a temporary
pub fn assign_inst() -> Nil {
  todo
}

/// Returns true if the block's last instruction is a jump
pub fn jumps() -> Bool {
  todo
}

// QBE Function
pub type Function {
  Function(
    linkage: Linkage,
    name: String,
    arguments: #(Type, Value),
    return_ty: Type,
    blocks: List(Block),
  )
}

// Display function for functions
pub fn display_function() -> String {
  todo
}

// Instantiates an empty function and returns it
pub fn new_function() -> Function {
  todo
}

// Adds a new empty block with a specified label and returns 
// a reference to it
pub fn add_block() -> Block {
  todo
}

// Returns a reference to the last block
pub fn last_block() -> Nil {
  todo
}

// Adds a new instruction to the last block
pub fn add_instr() -> Nil {
  todo
}

// Adds a new instruction assigned to a temporary
pub fn assign_instr() -> Nil {
  todo
}

// Linkage of a function or data defintion (e.g. section and
// private/public status)
pub type Linkage {
  Linkage(exported: Bool, section: Option(String), secflags: Option(String))
}

// Returns the default configuration for private linkage
pub fn private() -> Linkage {
  todo
}

// Returns the configuration for private linkage with a provided section
pub fn private_with_section() -> Nil {
  todo
}

// A complete IL file
pub type Module {
  Module(functions: List(Function), types: List(TypeDef), data: List(DataDef))
}

// Creates a new module
//pub fn new_module() -> Module {
//  Module(functions: list(), types: list(), data: list())
//}

// Display function for Module
pub fn display_module(module: Module) -> String {
  todo
}

pub fn add_function(module: Module, function: Function) -> Module {
  todo
}

pub fn add_type(module: Module, type_def: TypeDef) -> Module {
  todo
}

pub fn add_data(module: Module, data_def: DataDef) -> Module {
  todo
}
