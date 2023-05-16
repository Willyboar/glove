import gleam/int

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

pub type Option {
  Option(Value)
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
  Ret(Option)
  // Jumps to first label if a value is nonzero or to the second one otherwise
  Jnz(Value, String, String)
  // Unconditionally jumps to a label
  Jmp(String)
  // Calls a function
  Call(String, #(Type, Value))
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
pub fn display_inst() -> String {
  todo
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
    Temporary(name) -> "%{" <> name <> "}"
    Global(name) -> "${" <> name <> "}"
    Const(value) -> "{" <> int.to_string(value) <> "}"
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
  // Agreegate type with a specified name
  Agreegate(TypeDef)
}

// Display Type function
pub fn display_type(ty: Type) -> String {
  case ty {
    Byte -> "b"
    Halfword -> "h"
    Word -> "w"
    Long -> "l"
    Single -> "s"
    Double -> "d"
    Agreegate(TypeDef) -> display_type_def(TypeDef)
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
  TypeDef(name: String, align: Option, items: #(Type, Int))
}

// Display function for TypeDef
pub fn display_type_def() -> Nil {
  todo
}

// QBE Data definition item
pub type DataItem {
  // Symbol and offset
  Symbol(String, Int)
  // String
  Str(String)
  //
  Constant(Int)
}

// Display function for DataItem
pub fn display_data_item() -> String {
  todo
}

// IR Statement
pub type Statement {
  Assign(Value, Type, Inst)
  Volatile(Inst)
}

// Display function for Statement
pub fn display_statement() -> String {
  todo
}

// Function block with a label
pub type Block {
  Block(label: String, statements: #(Statement))
}

// Display function for block
pub fn display_block() -> String {
  todo
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
    return_ty: Option(Type),
    blocks: #(Block),
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

/// A complete IL file
pub type Module {
  Module(functions: #(Function), types: #(TypeDef), data: #(DataDef))
}

// Creates a new module
pub fn new_module() -> Module {
  Module(functions: #(), types: #(), data: #())
}

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