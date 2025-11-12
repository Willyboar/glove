import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

/// QBE Comparison Operators
/// QBE Comparison Operators
pub type Comp {
  // Signed integer comparisons
  /// Signed less than (word)
  Sltw
  /// Signed less than (long)
  Sltl
  /// Signed less or equal (word)
  Slew
  /// Signed less or equal (long)
  Slel
  /// Signed greater than (word)
  Sgtw
  /// Signed greater than (long)
  Sgtl
  /// Signed greater or equal (word)
  Sgew
  /// Signed greater or equal (long)
  Sgel
  // Unsigned integer comparisons
  /// Unsigned less than (word)
  Cultw
  /// Unsigned less than (long)
  Cultl
  /// Unsigned less or equal (word)
  Culew
  /// Unsigned less or equal (long)
  Culel
  /// Unsigned greater than (word)
  Cugtw
  /// Unsigned greater than (long)
  Cugtl
  /// Unsigned greater or equal (word)
  Cugew
  /// Unsigned greater or equal (long)
  Cugel
  // Integer equality (works for word and long)
  /// Equal (word)
  Ceqw
  /// Equal (long)
  Ceql
  /// Not equal (word)
  Cnew
  /// Not equal (long)
  Cnel
  // Floating-point comparisons
  /// Equal (single)
  Ceqs
  /// Equal (double)
  Ceqd
  /// Not equal (single)
  Cnes
  /// Not equal (double)
  Cned
  /// Less than (single)
  Clts
  /// Less than (double)
  Cltd
  /// Less or equal (single)
  Cles
  /// Less or equal (double)
  Cled
  /// Greater than (single)
  Cgts
  /// Greater than (double)
  Cgtd
  /// Greater or equal (single)
  Cges
  /// Greater or equal (double)
  Cged
  /// Ordered - no NaN (single)
  Cos
  /// Ordered - no NaN (double)
  Cod
  /// Unordered - contains NaN (single)
  Cuos
  /// Unordered - contains NaN (double)
  Cuod
  // Legacy - for backward compatibility
  /// Legacy: Less Than (deprecated - use Sltw/Sltl)
  Slt
  /// Legacy: Less or Equal (deprecated - use Slew/Slel)
  Sle
  /// Legacy: Greater than (deprecated - use Sgtw/Sgtl)
  Sgt
  /// Legacy: Greater or equal (deprecated - use Sgew/Sgel)
  Sge
  /// Legacy: Equal (deprecated - use Ceqw/Ceql)
  Eq
  /// Legacy: Not equal (deprecated - use Cnew/Cnel)
  Ne
}

/// QBE instruction
pub type Inst {
  // Arithmetic and Bits
  /// Adds values
  Add(Value, Value)
  /// Substracts value(b) from value(a)
  Sub(Value, Value)
  /// Multiplies values
  Mul(Value, Value)
  /// Divides value(a) by value(b) (signed)
  Div(Value, Value)
  /// Unsigned division
  Udiv(Value, Value)
  /// Returns a remainder from division (signed)
  Rem(Value, Value)
  /// Unsigned remainder
  Urem(Value, Value)
  /// Negation
  Neg(Value)
  /// Bitwise AND
  And(Value, Value)
  /// Bitwise OR
  Or(Value, Value)
  /// Bitwise XOR
  Xor(Value, Value)
  /// Arithmetic right shift
  Sar(Value, Value)
  /// Logical right shift
  Shr(Value, Value)
  /// Left shift
  Shl(Value, Value)
  // Comparisons
  /// Perform a Comparison
  Comp(Type, Comp, Value, Value)
  // Memory
  /// Allocates a 4-byte aligned area on the stack
  Alloc4(Int)
  /// Allocates a 8-byte aligned area on the stack
  Alloc8(Int)
  /// Allocates a 16-byte aligned area on the stack
  Alloc16(Int)
  /// Store word
  Storew(Value, Value)
  /// Store long
  Storel(Value, Value)
  /// Store single
  Stores(Value, Value)
  /// Store double
  Stored(Value, Value)
  /// Store half-word
  Storeh(Value, Value)
  /// Store byte
  Storeb(Value, Value)
  /// Load word (syntactic sugar - same as loadsw)
  Loadw(Value)
  /// Load word with sign extension
  Loadsw(Value)
  /// Load word with zero extension
  Loaduw(Value)
  /// Load long
  Loadl(Value)
  /// Load single
  Loads(Value)
  /// Load double
  Loadd(Value)
  /// Load half-word signed
  Loadsh(Value)
  /// Load half-word unsigned
  Loaduh(Value)
  /// Load byte signed
  Loadsb(Value)
  /// Load byte unsigned
  Loadub(Value)
  /// Copy `n` bytes from source to destination address
  Blit(Value, Value, Int)
  // Conversions
  /// Sign-extend word to long
  Extsw(Value)
  /// Zero-extend word to long
  Extuw(Value)
  /// Sign-extend half-word
  Extsh(Value)
  /// Zero-extend half-word
  Extuh(Value)
  /// Sign-extend byte
  Extsb(Value)
  /// Zero-extend byte
  Extub(Value)
  /// Extend single to double
  Exts(Value)
  /// Truncate double to single
  Truncd(Value)
  /// Single to signed integer
  Stosi(Value)
  /// Single to unsigned integer
  Stoui(Value)
  /// Double to signed integer
  Dtosi(Value)
  /// Double to unsigned integer
  Dtoui(Value)
  /// Signed word to float
  Swtof(Value)
  /// Unsigned word to float
  Uwtof(Value)
  /// Signed long to float
  Sltof(Value)
  /// Unsigned long to float
  Ultof(Value)
  /// Bitwise reinterpret cast
  Cast(Value)
  /// Copies either temporary or literal value
  Copy(Value)
  // Variadic
  /// Initialize variable argument list
  Vastart(Value)
  /// Fetch next variadic argument
  Vaarg(Value)
  // Control Flow
  /// Return from a function, optionally with a value
  Ret(Option(Value))
  /// Jumps to first label if a value is nonzero or to the second one otherwise
  Jnz(Value, String, String)
  /// Unconditionally jumps to a label
  Jmp(String)
  /// Program termination
  Hlt
  /// Calls a function
  Call(Value, List(#(Type, Value)))
  /// SSA phi node - value selection from predecessors
  /// List of (label, value) pairs
  Phi(List(#(String, Value)))
  // Legacy compatibility - old generic store/load
  /// Generic store (deprecated - use specific store instructions)
  Store(Type, Value, Value)
  /// Generic load (deprecated - use specific load instructions)
  Load(Type, Value)
}

/// Display function for Instructions
pub fn display_inst(inst: Inst) -> String {
  case inst {
    // Arithmetic
    Add(a, b) -> "add " <> display_value(a) <> ", " <> display_value(b)
    Sub(a, b) -> "sub " <> display_value(a) <> ", " <> display_value(b)
    Mul(a, b) -> "mul " <> display_value(a) <> ", " <> display_value(b)
    Div(a, b) -> "div " <> display_value(a) <> ", " <> display_value(b)
    Udiv(a, b) -> "udiv " <> display_value(a) <> ", " <> display_value(b)
    Rem(a, b) -> "rem " <> display_value(a) <> ", " <> display_value(b)
    Urem(a, b) -> "urem " <> display_value(a) <> ", " <> display_value(b)
    Neg(a) -> "neg " <> display_value(a)
    And(a, b) -> "and " <> display_value(a) <> ", " <> display_value(b)
    Or(a, b) -> "or " <> display_value(a) <> ", " <> display_value(b)
    Xor(a, b) -> "xor " <> display_value(a) <> ", " <> display_value(b)
    Sar(a, b) -> "sar " <> display_value(a) <> ", " <> display_value(b)
    Shr(a, b) -> "shr " <> display_value(a) <> ", " <> display_value(b)
    Shl(a, b) -> "shl " <> display_value(a) <> ", " <> display_value(b)
    // Comparisons
    Comp(ty, cmp, a, b) -> {
      case ty {
        Aggregate(_) -> "Cannot Compare aggregate types"
        _ -> {
          let cmp_string = case cmp {
            // Signed integer comparisons
            Sltw -> "csltw"
            Sltl -> "csltl"
            Slew -> "cslew"
            Slel -> "cslel"
            Sgtw -> "csgtw"
            Sgtl -> "csgtl"
            Sgew -> "csgew"
            Sgel -> "csgel"
            // Unsigned integer comparisons
            Cultw -> "cultw"
            Cultl -> "cultl"
            Culew -> "culew"
            Culel -> "culel"
            Cugtw -> "cugtw"
            Cugtl -> "cugtl"
            Cugew -> "cugew"
            Cugel -> "cugel"
            // Equality comparisons
            Ceqw -> "ceqw"
            Ceql -> "ceql"
            Cnew -> "cnew"
            Cnel -> "cnel"
            // Float comparisons
            Ceqs -> "ceqs"
            Ceqd -> "ceqd"
            Cnes -> "cnes"
            Cned -> "cned"
            Clts -> "clts"
            Cltd -> "cltd"
            Cles -> "cles"
            Cled -> "cled"
            Cgts -> "cgts"
            Cgtd -> "cgtd"
            Cges -> "cges"
            Cged -> "cged"
            Cos -> "cos"
            Cod -> "cod"
            Cuos -> "cuos"
            Cuod -> "cuod"
            // Legacy comparisons (type-generic)
            Slt -> "cslt" <> display_type(ty)
            Sle -> "csle" <> display_type(ty)
            Sgt -> "csgt" <> display_type(ty)
            Sge -> "csge" <> display_type(ty)
            Eq -> "ceq" <> display_type(ty)
            Ne -> "cne" <> display_type(ty)
          }
          cmp_string <> " " <> display_value(a) <> ", " <> display_value(b)
        }
      }
    }
    // Memory operations
    Alloc4(int) -> "alloc4 " <> int.to_string(int)
    Alloc8(int) -> "alloc8 " <> int.to_string(int)
    Alloc16(int) -> "alloc16 " <> int.to_string(int)
    Storew(val, dest) ->
      "storew " <> display_value(val) <> ", " <> display_value(dest)
    Storel(val, dest) ->
      "storel " <> display_value(val) <> ", " <> display_value(dest)
    Stores(val, dest) ->
      "stores " <> display_value(val) <> ", " <> display_value(dest)
    Stored(val, dest) ->
      "stored " <> display_value(val) <> ", " <> display_value(dest)
    Storeh(val, dest) ->
      "storeh " <> display_value(val) <> ", " <> display_value(dest)
    Storeb(val, dest) ->
      "storeb " <> display_value(val) <> ", " <> display_value(dest)
    Loadw(src) -> "loadw " <> display_value(src)
    Loadsw(src) -> "loadsw " <> display_value(src)
    Loaduw(src) -> "loaduw " <> display_value(src)
    Loadl(src) -> "loadl " <> display_value(src)
    Loads(src) -> "loads " <> display_value(src)
    Loadd(src) -> "loadd " <> display_value(src)
    Loadsh(src) -> "loadsh " <> display_value(src)
    Loaduh(src) -> "loaduh " <> display_value(src)
    Loadsb(src) -> "loadsb " <> display_value(src)
    Loadub(src) -> "loadub " <> display_value(src)
    Blit(src, dest, n) ->
      "blit "
      <> display_value(src)
      <> ", "
      <> display_value(dest)
      <> ", "
      <> int.to_string(n)
    // Conversions
    Extsw(val) -> "extsw " <> display_value(val)
    Extuw(val) -> "extuw " <> display_value(val)
    Extsh(val) -> "extsh " <> display_value(val)
    Extuh(val) -> "extuh " <> display_value(val)
    Extsb(val) -> "extsb " <> display_value(val)
    Extub(val) -> "extub " <> display_value(val)
    Exts(val) -> "exts " <> display_value(val)
    Truncd(val) -> "truncd " <> display_value(val)
    Stosi(val) -> "stosi " <> display_value(val)
    Stoui(val) -> "stoui " <> display_value(val)
    Dtosi(val) -> "dtosi " <> display_value(val)
    Dtoui(val) -> "dtoui " <> display_value(val)
    Swtof(val) -> "swtof " <> display_value(val)
    Uwtof(val) -> "uwtof " <> display_value(val)
    Sltof(val) -> "sltof " <> display_value(val)
    Ultof(val) -> "ultof " <> display_value(val)
    Cast(val) -> "cast " <> display_value(val)
    Copy(val) -> "copy " <> display_value(val)
    // Variadic
    Vastart(addr) -> "vastart " <> display_value(addr)
    Vaarg(addr) -> "vaarg " <> display_value(addr)
    // Control Flow
    Ret(val) -> {
      case val {
        Some(val) -> "ret " <> display_value(val) <> "\n"
        None -> "ret\n"
      }
    }
    Jnz(val, if_nonzero, if_zero) ->
      "jnz " <> display_value(val) <> ", @" <> if_nonzero <> ", @" <> if_zero
    Jmp(str) -> "jmp @" <> str
    Hlt -> "hlt"
    Call(name, args) -> {
      let arg_str =
        args
        |> list.index_map(fn(arg, _) {
          let #(ty, val) = arg
          display_type(ty) <> " " <> display_value(val)
        })
        |> string.join(", ")

      "call " <> display_value(name) <> "(" <> arg_str <> ")"
    }

    Phi(branches) -> {
      let branch_str =
        branches
        |> list.map(fn(branch) {
          let #(label, val) = branch
          "@" <> label <> " " <> display_value(val)
        })
        |> string.join(", ")

      "phi " <> branch_str
    }

    Store(typ, value, dest) ->
      case typ {
        Aggregate(_) -> "Store to an aggregate type"
        _ ->
          "store"
          <> display_type(typ)
          <> " "
          <> display_value(value)
          <> ", "
          <> display_value(dest)
      }

    Load(typ, val) ->
      case typ {
        Aggregate(_) -> "Load aggregate type"
        _ -> "load" <> display_type(typ) <> " " <> display_value(val)
      }
  }
}

/// Bitwise operations
/// Bitwise operations
/// QBE Value for instructions
pub type Value {
  /// `%`-temporary
  Temporary(name: String)
  /// `$`-global
  Global(name: String)
  /// Constant
  Const(value: Int)
}

/// Display Value function
pub fn display_value(value: Value) -> String {
  case value {
    Temporary(name) -> "%" <> name
    Global(name) -> "$" <> name
    Const(value) -> int.to_string(value)
  }
}

/// QBE Types
pub type Type {
  /// Base Types
  Word
  Long
  Single
  Double
  /// Extended Types
  Byte
  Halfword
  Aggregate(TypeDef)
}

/// Display Type function
pub fn display_type(ty: Type) -> String {
  case ty {
    Byte -> "b"
    Halfword -> "h"
    Word -> "w"
    Long -> "l"
    Single -> "s"
    Double -> "d"
    Aggregate(ty) -> display_type_def(ty)
  }
}

/// Aggregate type with a specified name
/// Returns a C ABI type. Extended types are converted to closest base
/// types
pub fn into_abi(self) -> Type {
  case self {
    Byte | Halfword -> Word
    other -> other
  }
}

/// Returns the closest base type
pub fn into_base(self) -> Type {
  case self {
    Byte | Halfword -> Word
    Aggregate(_) -> Long
    other -> other
  }
}

/// Returns byte alignment for values of the type
pub fn align(self: Type) -> Int {
  case self {
    Aggregate(td) -> {
      case td.align {
        Some(alignment) -> alignment
        None -> {
          // The alignment of a type is the maximum alignment of its members
          // When there's no members, the alignment is usually defined to be 1.
          td.items
          |> list.map(fn(item) { align(item.0) })
          |> list.reduce(int.max)
          |> result.unwrap(1)
        }
      }
    }
    // For non-aggregate types, alignment equals size
    _ -> size(self)
  }
}

/// Returns byte size for values of the type
pub fn size(self: Type) -> Int {
  case self {
    Byte -> 1
    Halfword -> 2
    Word | Single -> 4
    Long | Double -> 8
    Aggregate(td) -> {
      // Calculate size with proper padding
      // Calculation from: https://en.wikipedia.org/wiki/Data_structure_alignment#Computing%20padding
      let offset =
        td.items
        |> list.fold(0, fn(offset, item) {
          let #(item_type, repeat) = item
          let item_align = align(item_type)
          let item_size = repeat * size(item_type)
          let padding = { item_align - { offset % item_align } } % item_align
          offset + padding + item_size
        })

      let type_align = align(self)
      let final_padding = { type_align - { offset % type_align } } % type_align

      // Size is the final offset with the padding that is left
      offset + final_padding
    }
  }
}

/// QBE data definition
pub type DataDef {
  DataDef(
    linkage: Linkage,
    name: String,
    align: Option(Int),
    items: List(#(Type, DataItem)),
  )
}

pub fn new_datadef() -> DataDef {
  DataDef(linkage: private(), name: "", align: None, items: [])
}

/// Display function for Datadef
pub fn display_data_def(def: DataDef) -> String {
  let linkage_str = display_linkage(def.linkage)
  let align_str = case def.align {
    Some(align) -> " align " <> int.to_string(align)
    None -> ""
  }

  let items_str =
    def.items
    |> list.index_map(fn(item, _) {
      case item {
        #(ty, di) -> display_type(ty) <> " " <> display_data_item(di)
      }
    })
    |> string.join(", ")

  linkage_str
  <> "data $"
  <> def.name
  <> " ="
  <> align_str
  <> " { "
  <> items_str
  <> " }"
}

/// QBE aggregate type definition
pub type TypeDef {
  TypeDef(name: String, align: Option(Int), items: List(#(Type, Int)))
}

/// Display function for TypeDef
pub fn display_type_def(def: TypeDef) -> String {
  let align_str = case def.align {
    Some(align) -> "align " <> int.to_string(align) <> " "
    None -> ""
  }

  let items_str =
    def.items
    |> list.index_map(fn(item, _) {
      case item {
        #(ty, count) ->
          case count > 1 {
            False -> display_type(ty)
            True -> display_type(ty) <> " " <> int.to_string(count)
          }
      }
    })
    |> string.join(", ")

  "type :" <> def.name <> " = " <> align_str <> "{ " <> items_str <> " }"
}

/// QBE Data definition item
pub type DataItem {
  /// Symbol and offset
  Symbol(String, Option(Int))
  /// String
  Str(String)
  /// Integer
  Constant(Int)
}

/// Display function for DataItem
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

/// IR Statement
pub type Statement {
  Assign(Value, Type, Inst)
  Volatile(Inst)
}

/// Display function for Statement
pub fn display_statement(stmt: Statement) -> String {
  case stmt {
    Assign(val, typ, inst) ->
      display_value(val)
      <> " ="
      <> display_type(typ)
      <> " "
      <> display_inst(inst)
    Volatile(inst) -> display_inst(inst)
  }
}

/// Function block with a label
pub type Block {
  Block(label: String, statements: List(Statement))
}

/// Display function for block
pub fn display_block(block: Block) -> String {
  let label = block.label
  let statements =
    block.statements
    |> list.map(display_statement)
    |> string.join("\n")

  label <> "\n" <> statements
}

/// Adds a new instruction to the block
pub fn add_inst(block: Block, inst: Inst) -> Block {
  Block(
    label: block.label,
    statements: list.append(block.statements, [Volatile(inst)]),
  )
}

/// Adds a new instruction assigned to a temporary
pub fn assign_inst(block: Block, val: Value, typ: Type, inst: Inst) -> Block {
  Block(
    label: block.label,
    statements: list.append(block.statements, [Assign(val, typ, inst)]),
  )
}

/// Returns true if the block's last instruction is a jump
pub fn jumps(block: Block) -> Bool {
  case list.last(block.statements) {
    Ok(statement) ->
      case statement {
        Volatile(instr) ->
          case instr {
            Ret(_) -> True
            Jmp(_) -> True
            Jnz(_, _, _) -> True
            _ -> False
          }
        _ -> False
      }
    Error(_) -> False
  }
}

/// QBE Function
pub type Function {
  Function(
    linkage: Linkage,
    name: String,
    arguments: List(#(Type, Value)),
    return_ty: Option(Type),
    blocks: List(Block),
  )
}

/// Display function for functions
pub fn display_function(func: Function) -> String {
  let linkage_str = display_linkage(func.linkage)
  let name_str = func.name
  let return_str = case func.return_ty {
    Some(ty) -> " " <> display_type(ty)
    None -> ""
  }
  let args_str = display_arguments(func.arguments)
  let blocks_str = display_blocks(func.blocks)

  linkage_str
  <> "function"
  <> return_str
  <> " "
  <> "$"
  <> name_str
  <> "("
  <> args_str
  <> ")"
  <> " {\n"
  <> blocks_str
  <> "}"
}

/// Display functions Arguments
pub fn display_arguments(arguments: List(#(Type, Value))) -> String {
  case arguments {
    [] -> ""
    _ ->
      arguments
      |> list.index_map(fn(arg, _) {
        case arg {
          #(ty, val) -> display_type(ty) <> " " <> display_value(val)
        }
      })
      |> string.join(", ")
  }
}

/// Display blocks
pub fn display_blocks(blocks: List(Block)) -> String {
  blocks
  |> list.map(fn(block) { display_block(block) })
  |> string.join("\n")
}

/// Instantiates an empty function and returns it
pub fn new_function() -> Function {
  Function(
    linkage: private(),
    name: "",
    arguments: [],
    return_ty: None,
    blocks: [],
  )
}

/// Adds a new empty block with a specified label and returns
/// a reference to it
pub fn add_block(label: String) -> Block {
  Block(label: label, statements: [])
}

/// Returns a reference to the last block
pub fn last_block(blocks: List(Block)) -> Option(Block) {
  case list.last(blocks) {
    Ok(block) -> Some(block)
    Error(_) -> None
  }
}

/// Linkage of a function or data defintion (e.g. section and
/// private/public status)
pub type Linkage {
  Linkage(exported: Bool, section: Option(String), secflags: Option(String))
}

/// Display function for Linkage
pub fn display_linkage(linkage: Linkage) -> String {
  let exported_str = case linkage.exported {
    True -> "export "
    False -> ""
  }
  let section_str = case linkage.section {
    Some(section) ->
      "section \""
      <> section
      <> "\""
      <> case linkage.secflags {
        Some(secflags) -> " \"" <> secflags <> "\""
        None -> ""
      }
      <> " "
    None -> ""
  }
  exported_str <> section_str
}

/// Returns the default configuration for private linkage
pub fn private() -> Linkage {
  Linkage(exported: False, section: None, secflags: None)
}

/// Returns the configuration for private linkage with a provided section
pub fn private_with_section(section: String) -> Linkage {
  Linkage(exported: False, section: Some(section), secflags: None)
}

/// Returns the default configuration for public linkage
pub fn public() -> Linkage {
  Linkage(exported: True, section: None, secflags: None)
}

/// Returns the configuration for public linkage with a provided section
pub fn public_with_section(section: String) -> Linkage {
  Linkage(exported: True, section: Some(section), secflags: None)
}

/// A complete IL file
pub type Module {
  Module(functions: List(Function), types: List(TypeDef), data: List(DataDef))
}

/// Creates a new module
pub fn new_module() -> Module {
  Module(functions: [], types: [], data: [])
}

/// Display function for Module
pub fn display_module(module: Module) -> String {
  let functions_str =
    module.functions
    |> list.map(display_function)
    |> string.join("\n")

  let types_str =
    module.types
    |> list.map(display_type_def)
    |> string.join("\n")

  let data_str =
    module.data
    |> list.map(display_data_def)
    |> string.join("\n")

  functions_str <> types_str <> "\n" <> data_str
}

/// Add function to module
pub fn add_function(module: Module, function: Function) -> Module {
  Module(
    functions: list.append(module.functions, [function]),
    types: module.types,
    data: module.data,
  )
}

/// Add type to module
pub fn add_type(module: Module, type_def: TypeDef) -> Module {
  Module(
    functions: module.functions,
    types: list.append(module.types, [type_def]),
    data: module.data,
  )
}

/// Add Data to module
pub fn add_data(module: Module, data_def: DataDef) -> Module {
  Module(
    functions: module.functions,
    types: module.types,
    data: list.append(module.data, [data_def]),
  )
}
